-- Copyright (c)2010-2011, Mark Wright.  All rights reserved.

{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, ExistentialQuantification, ScopedTypeVariables #-}

module Semantic where

import Prelude hiding (catch)
import Data.Dynamic
import Control.Exception
import C2HS (Ptr, StablePtr, deRefStablePtr, newStablePtr, freeStablePtr)
import Text.Antlrc.Lexer (CommonToken, tokenGetText, tokenGetLine, tokenGetCharPositionInLine)
import ArithInterface
import Text.PrettyPrint.HughesPJ

data Term a =
  TmTrue a
  | TmFalse a
  | TmIf a (Term a) (Term a) (Term a)
  | TmZero a
  | TmSucc a (Term a)
  | TmPred a (Term a)
  | TmIsZero a (Term a)
  deriving (Show)

data Info =
  Info
  {
    line :: Int,
    charPosition :: Int
  }
  deriving (Show)

type TermInfo = Term Info

dummyInfo :: Info
dummyInfo = Info 0 0

--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

printtmTerm :: Bool -> TermInfo -> Doc
printtmTerm outer (TmIf fi t1 t2 t3) =
  text "if " <> printtmTerm False t1 <> space <> text "then " <> printtmTerm False t2 <> space <> text "else " <> printtmTerm False t3
printtmTerm outer t = printtmAppTerm outer t

printtmAppTerm :: Bool -> TermInfo -> Doc
printtmAppTerm outer (TmPred _ t1) =
  text "pred " <> printtmATerm False t1
printtmAppTerm outer (TmIsZero _ t1) =
  text "iszero " <> printtmATerm False t1
printtmAppTerm outer t = printtmATerm outer t

printtmATerm :: Bool -> TermInfo -> Doc
printtmATerm outer (TmTrue _) = text "true"
printtmATerm outer (TmFalse _) = text "false"
printtmATerm outer (TmZero fi) = text "0"
printtmATerm outer (TmSucc _ t1) =
  let f :: Int -> TermInfo -> Doc
      f n (TmZero _) = int n
      f n (TmSucc _ s) = f (n+1) s
      f n _  = parens (text "succ " <> printtmATerm False t1)
     in f 1 t1
printtmATerm outer t = parens (printtmTerm outer t)

printtm :: TermInfo -> Doc
printtm = printtmTerm True

--------------------------------------------------------------------------------
-- Exceptions are not currently used
--------------------------------------------------------------------------------

data SomeTypeCheckerException = forall a . (Exception a) => SomeTypeCheckerException a
                              deriving Typeable

instance Show SomeTypeCheckerException where
  show (SomeTypeCheckerException e) = show e

instance Exception SomeTypeCheckerException

typeCheckerToException :: (Exception a) => a -> SomeException
typeCheckerToException = toException . SomeTypeCheckerException

typeCheckerFromException :: (Exception a) => SomeException -> Maybe a
typeCheckerFromException x = do 
  SomeTypeCheckerException a <- fromException x 
  cast a

data NoRuleApplies = NoRuleApplies
                   deriving (Typeable, Show)

instance Exception NoRuleApplies where
  toException = typeCheckerToException
  fromException = typeCheckerFromException

--------------------------------------------------------------------------------
-- State monads are not currently used
--------------------------------------------------------------------------------

data SemanticActions = 
  SemanticActions 
  {
    term :: TermInfo
  }
  deriving (Show)

-- type SemanticActionsState = State SemanticActions

-- foreign export ccall semanticActions :: IO (StablePtr SemanticActions)
-- semanticActions = newStablePtr SemanticActions

--------------------------------------------------------------------------------
-- evaluation
--------------------------------------------------------------------------------
  
isnumericval :: TermInfo -> Bool
isnumericval (TmZero _) = True
isnumericval (TmSucc _ t1) = isnumericval t1
isnumericval _ = False

isval :: TermInfo -> Bool
isval (TmTrue _) = True
isval (TmFalse _) = True
isval t | isnumericval t = True
isval _ = False

eval1 :: TermInfo -> Maybe TermInfo
eval1 (TmIf _ (TmTrue _) t2 t3) = Just t2
eval1 (TmIf _ (TmFalse _) t2 t3) = Just t3
eval1 (TmIf fi t1 t2 t3) =
  maybe Nothing f t1'
  where
  t1' = eval1 t1
  f :: TermInfo -> Maybe TermInfo
  f t1'' = Just (TmIf fi t1'' t2 t3)
eval1 (TmSucc fi t1) =
  maybe Nothing f t1'
  where
  t1' = eval1 t1
  f :: TermInfo -> Maybe TermInfo
  f t1'' = Just (TmSucc fi t1'')
eval1 (TmPred _ (TmZero i)) = Just (TmZero i)
eval1 (TmPred _ (TmSucc _ nv1)) | isnumericval nv1 = Just nv1
eval1 (TmPred fi t1) = 
  maybe Nothing f t1'
  where
  t1' = eval1 t1
  f :: TermInfo -> Maybe TermInfo
  f t1'' = Just (TmPred fi t1'')
eval1 (TmIsZero _ (TmZero i)) = Just (TmTrue i)
eval1 (TmIsZero _ (TmSucc i nv1)) | isnumericval nv1 = Just (TmFalse i)
eval1 (TmIsZero fi t1) =
  maybe Nothing f t1'
  where
  t1' = eval1 t1
  f :: TermInfo -> Maybe TermInfo
  f t1'' = Just (TmIsZero fi t1'')
eval1 _ = Nothing

eval :: TermInfo -> TermInfo
eval t = maybe t eval t'
  where t' = eval1 t

foreign export ccall saEvaluate :: StablePtr TermInfo -> IO ()
saEvaluate s =
  do
    t <- deRefStablePtr s
    freeStablePtr s
    catch  
      ((print . render . printtm . eval) t)
      (\(e::NoRuleApplies) -> print e)

--------------------------------------------------------------------------------
-- evaluation exiting recursion by throwing an exception
-- Has bugs:
-- (1) It spins with the seq or a bang pattern in evalEx
-- (2) Still spins on inputs: 
-- succ (pred 0);
-- succ 0;
--------------------------------------------------------------------------------

eval1Ex :: TermInfo -> TermInfo
eval1Ex (TmIf _ (TmTrue _) t2 t3) = t2
eval1Ex (TmIf _ (TmFalse _) t2 t3) = t3
eval1Ex (TmIf fi t1 t2 t3) =
  let t1' = eval1Ex t1 in
  TmIf fi t1' t2 t3
eval1Ex (TmSucc fi t1) =
  let t1' = eval1Ex t1 in
  TmSucc fi t1'
eval1Ex (TmPred _ (TmZero i)) = TmZero i
eval1Ex (TmPred _ (TmSucc _ nv1)) | isnumericval nv1 = nv1
eval1Ex (TmPred fi t1) = 
  let t1' = eval1Ex t1 in
  TmPred fi t1'
eval1Ex (TmIsZero _ (TmZero i)) = TmTrue i
eval1Ex (TmIsZero _ (TmSucc i nv1)) | isnumericval nv1 = TmFalse i
eval1Ex (TmIsZero fi t1) =
  let t1' = eval1Ex t1 in
  TmIsZero fi t1'
eval1Ex _ = throw NoRuleApplies

evalEx :: TermInfo -> IO TermInfo
evalEx t =
  catch  
  (let t' = eval1Ex t
   in seq t' (evalEx t')
  )
  (\(e::NoRuleApplies) -> return t)

foreign export ccall saEvaluateEx :: StablePtr TermInfo -> IO ()
saEvaluateEx s =
  do
    t <- deRefStablePtr s
    freeStablePtr s
    catch  
      (do { t' <- evalEx t; (print . render . printtm) t'; })
      (\(e::NoRuleApplies) -> print e)

--------------------------------------------------------------------------------
-- Constructing terms
--------------------------------------------------------------------------------

foreign export ccall saIf :: Ptr CommonToken -> StablePtr TermInfo -> StablePtr TermInfo -> StablePtr TermInfo -> IO (StablePtr TermInfo)
saIf token s1 s2 s3 =
  do
    t1 <- deRefStablePtr s1
    freeStablePtr s1
    t2 <- deRefStablePtr s2
    freeStablePtr s2
    t3 <- deRefStablePtr s3
    freeStablePtr s3
    -- obtain the source code line and charPosition from the token
    l <- tokenGetLine token
    c <- tokenGetCharPositionInLine token
    -- return the TmTrue term
    newStablePtr (TmIf (Info l c) t1 t2 t3)

foreign export ccall saSucc :: Ptr CommonToken -> StablePtr TermInfo -> IO (StablePtr TermInfo)
saSucc token s =
  do
    t <- deRefStablePtr s
    freeStablePtr s
    -- obtain the source code line and charPosition from the token
    l <- tokenGetLine token
    c <- tokenGetCharPositionInLine token
    -- return the TmTrue term
    newStablePtr (TmSucc (Info l c) t)

foreign export ccall saPred :: Ptr CommonToken -> StablePtr TermInfo -> IO (StablePtr TermInfo)
saPred token s =
  do
    t <- deRefStablePtr s
    freeStablePtr s
    -- obtain the source code line and charPosition from the token
    l <- tokenGetLine token
    c <- tokenGetCharPositionInLine token
    -- return the TmTrue term
    newStablePtr (TmPred (Info l c) t)

foreign export ccall saIsZero :: Ptr CommonToken -> StablePtr TermInfo -> IO (StablePtr TermInfo)
saIsZero token s =
  do
    t <- deRefStablePtr s
    freeStablePtr s
    -- obtain the source code line and charPosition from the token
    l <- tokenGetLine token
    c <- tokenGetCharPositionInLine token
    -- return the TmTrue term
    newStablePtr (TmIsZero (Info l c) t)

foreign export ccall saTrue :: Ptr CommonToken -> IO (StablePtr TermInfo)
saTrue token =
  do
    -- obtain the source code line and charPosition from the token
    l <- tokenGetLine token
    c <- tokenGetCharPositionInLine token
    -- return the TmTrue term
    newStablePtr (TmTrue (Info l c))

foreign export ccall saFalse :: Ptr CommonToken -> IO (StablePtr TermInfo)
saFalse token =
  do
    -- obtain the source code line and charPosition from the token
    l <- tokenGetLine token
    c <- tokenGetCharPositionInLine token
    -- return the TmFalse term
    newStablePtr (TmFalse (Info l c))

intV :: Info -> Int -> TermInfo
intV i n
  | n <= 0 = TmZero i
  | otherwise = TmSucc i (intV i (n - 1))

foreign export ccall saIntV :: Ptr CommonToken -> IO (StablePtr TermInfo)
saIntV token =
  do
    -- read the IntV integer value from the token text into n
    t <- tokenGetText token
    n <- readIO t
    -- obtain the source code line and charPosition from the token
    l <- tokenGetLine token
    c <- tokenGetCharPositionInLine token
    -- return the term, which is TmZero, or TmSucc TmZero, or TmSucc (TmSucc (...TmSucc TmZero))
    newStablePtr (intV (Info l c) n)

