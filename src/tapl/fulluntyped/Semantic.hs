-- Copyright (c)2010-2011, Mark Wright.  All rights reserved.

{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, ScopedTypeVariables #-}

module Semantic where

import Prelude hiding (catch)
import Data.Dynamic
import Control.Exception
import C2HS (Ptr, StablePtr, deRefStablePtr, newStablePtr, freeStablePtr)
import Text.Antlrc.Lexer (CommonToken, tokenGetText, tokenGetLine, tokenGetCharPositionInLine)
import FullUntypedInterface
import Text.PrettyPrint.HughesPJ
import Data.List (findIndex)

data Term a =
  TmTrue a
  | TmFalse a
  | TmIf a (Term a) (Term a) (Term a)
  | TmVar a Int Int
  | TmAbs a String (Term a)
  | TmApp a (Term a) (Term a)
  | TmRecord a [(String, Term a)]
  | TmProj a (Term a) String
  | TmFloat a Float
  | TmTimesFloat a (Term a) (Term a)
  | TmString a String
  | TmZero a
  | TmSucc a (Term a)
  | TmPred a (Term a)
  | TmIsZero a (Term a)
  | TmLet a String (Term a) (Term a)
  deriving (Show)

data Binding a =
  NameBind
  | TmAbbBind (Term a)

type ContextElement a = (String, Binding a)

type Context a = [ContextElement a]

data Command a =
  Eval a (Term a)
  | Bind a String (Binding a)

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
-- Context management
--------------------------------------------------------------------------------

emptycontext :: Context a
emptycontext = []

ctxlength :: Context a -> Int
ctxlength = length

addbinding :: Context a -> String -> Binding a -> Context a
addbinding ctx x bind = (x,bind):ctx

addname :: Context a -> String -> Context a
addname ctx x = addbinding ctx x NameBind

isnamebound :: Context a -> String -> Bool
isnamebound [] _ = False
isnamebound ((y,_):rest) x = (y==x) || isnamebound rest x

pickfreshname :: Context a -> String -> (Context a, String)
pickfreshname ctx x = if isnamebound ctx x 
                      then pickfreshname ctx (x ++ "'")
                      else ((x,NameBind):ctx, x)

index2name :: Info -> Context Info -> Int -> Either String String
index2name fi ctx x = 
  if (x >= 0) && (x < length ctx)
  then Right (fst (ctx !! x))
  else Left (show (line fi) ++ ":" ++ show (charPosition fi) ++ ": Variable lookup failure: offset: " ++ show x ++ ", ctx size: " ++ show (length ctx))
       
name2index :: Info -> Context Info -> String -> Either String Int
name2index fi ctx x =
  maybe (Left ("Identifier " ++ x ++ " is unbound")) Right (findIndex ((== x) . fst) ctx)

--------------------------------------------------------------------------------
-- Shifting
--------------------------------------------------------------------------------

tmmap :: (Info -> Int -> Int -> Int -> Term Info) -> Int -> Term Info -> Term Info
tmmap onvar c t =
  walk c t
  where
    walk :: Int -> Term Info -> Term Info
    walk c' t' =
      case t' of
        t''@(TmTrue fi) -> t''
        t''@(TmFalse fi) -> t''
        (TmIf fi t1 t2 t3) -> TmIf fi (walk c' t1) (walk c' t2) (walk c' t3)
        (TmVar fi x n) -> onvar fi c' x n
        (TmAbs fi x t2) -> TmAbs fi x (walk (c+1) t2)
        (TmApp fi t1 t2) -> TmApp fi (walk c t1) (walk c t2)
        (TmProj fi t1 l) -> TmProj fi (walk c t1) l
        (TmRecord fi fields) -> TmRecord fi (map f fields)
          where
            f :: (String, Term Info) -> (String, Term Info)
            f (li, ti) = (li, walk c' ti)
        t''@(TmFloat _ _) -> t''
        (TmTimesFloat fi t1 t2) -> TmTimesFloat fi (walk c' t1) (walk c' t2)
        t''@(TmString _ _) -> t''
        t''@(TmZero _) -> t''
        (TmSucc fi t1) -> TmSucc fi (walk c' t1)
        (TmPred fi t1) -> TmPred fi (walk c' t1)
        (TmIsZero fi t1) -> TmIsZero fi (walk c' t1)
        (TmLet fi x t1 t2) -> TmLet fi x (walk c' t1) (walk (c+1) t2)

termShiftAbove :: Int -> Int -> Term Info -> Term Info
termShiftAbove d c t = 
  tmmap f c t
  where
    f :: Info -> Int -> Int -> Int -> Term Info
    f fi c' x n = 
      if x>=c'
      then TmVar fi (x+d) (n+d)
      else TmVar fi x (n+d)

termShift :: Int -> Term Info -> Term Info
termShift d = termShiftAbove d 0

bindingshift :: Int -> Binding Info -> Binding Info
bindingshift d bind = 
  case bind of
    b@NameBind -> b
    (TmAbbBind t) -> TmAbbBind (termShift d t)

--------------------------------------------------------------------------------
-- Substitution
--------------------------------------------------------------------------------

termSubst :: Int -> Term Info -> Term Info -> Term Info
termSubst j s t =
  tmmap f 0 t
  where
    f :: Info -> Int -> Int -> Int -> Term Info
    f fi c x n =
      if x==(j+c)
      then termShift c s
      else TmVar fi x n

termSubstTop :: Term Info -> Term Info -> Term Info
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

--------------------------------------------------------------------------------
-- Context management continued
--------------------------------------------------------------------------------

getbinding :: Info -> Context Info -> Int -> Either String (Binding Info)
getbinding fi ctx i =
  if (i >= 0) && (i < length ctx)
  then
    Right (bindingshift (i+1) (snd (ctx !! i)))
  else
    Left (show (line fi) ++ ":" ++ show (charPosition fi) ++ ": Variable lookup failure: offset: " ++ show i ++ ", ctx size: " ++ show (length ctx))

--------------------------------------------------------------------------------
-- Extracting file info
--------------------------------------------------------------------------------

tmInfo :: Term a -> a
tmInfo (TmTrue fi) = fi
tmInfo (TmFalse fi) = fi
tmInfo (TmIf fi _ _ _) = fi
tmInfo (TmVar fi _ _) = fi
tmInfo (TmAbs fi _ _) = fi
tmInfo (TmApp fi  _  _) = fi
tmInfo (TmProj fi _ _) = fi
tmInfo (TmRecord fi _) = fi
tmInfo (TmFloat fi _) = fi
tmInfo (TmTimesFloat fi _ _) = fi
tmInfo (TmString fi _) = fi
tmInfo (TmZero fi) = fi
tmInfo (TmSucc fi _) = fi
tmInfo (TmPred fi _) = fi
tmInfo (TmIsZero fi _) = fi
tmInfo (TmLet fi _ _ _) = fi 

--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

printtmTerm :: Bool -> Context Info -> TermInfo -> Doc
printtmTerm outer ctx t = 
  case t of
    (TmIf fi t1 t2 t3) -> 
      text "if " <> printtmTerm False ctx t1 <> space <> text "then " <> printtmTerm False ctx t2 <> space <> text "else " <> printtmTerm False ctx t3
    (TmAbs fi x t2) ->
      let (ctx',x') = (pickfreshname ctx x) in
      text "lambda " <> printtmTerm outer ctx' t2
    (TmLet fi x t1 t2) ->
      text "let " <> printtmTerm False ctx t1 <> space <> printtmTerm False ctx t2
    t' -> 
      printtmAppTerm outer ctx t'

printtmAppTerm :: Bool -> Context Info -> TermInfo -> Doc
printtmAppTerm outer ctx t = 
  case t of
    (TmApp fi t1 t2) ->
      printtmAppTerm False ctx t1 <> space <> printtmATerm False ctx t2
    (TmTimesFloat _ t1 t2) ->
      text "timesfloat " <> printtmATerm False ctx t2 <> space <> printtmATerm False ctx t2
    (TmPred _ t1) ->
      text "pred " <> printtmATerm False ctx t1
    (TmIsZero _ t1) ->
      text "iszero " <> printtmATerm False ctx t1
    t' -> 
      printtmATerm outer ctx t'

printtmPathTerm :: Bool -> Context Info -> TermInfo -> Doc
printtmPathTerm outer ctx t =
  case t of
    (TmProj _ t1 l) ->
      printtmATerm False ctx t1 <> text "." <> text l
    t' -> printtmATerm outer ctx t'

printtmATerm :: Bool -> Context Info -> TermInfo -> Doc
printtmATerm outer ctx t =
  case t of
    (TmTrue _) -> text "true"
    (TmFalse _) -> text "false"
    (TmVar fi x n) ->
      if ctxlength ctx == n
      then 
        either text text (index2name fi ctx x)
      else
        text ("[bad index: " ++ show x ++ "/" ++ show n ++ " in {" ++ foldl f "" ctx)
        where
          f s (x',_) = s ++ " " ++ x'
    (TmRecord fi fields) ->
      let
        pf i (li,ti) =
          d1 <> d2
          where
            d1 :: Doc
            d1 =
              if li /= show i
              then text li <> equals
              else empty
            d2 :: Doc
            d2 = printtmTerm False ctx ti
      in let
         p i l =
           case l of
             [] -> empty
             [f] -> pf i f
             (f:rest) -> pf i f <> comma <> p (i+1) rest
      in braces (p 1 fields)
    (TmFloat _ s) -> text (show s)
    (TmString _ s) -> (doubleQuotes . text) s
    (TmZero fi) -> text "0"
    (TmSucc _ t1) ->
      let f :: Int -> TermInfo -> Doc
          f n (TmZero _) = int n
          f n (TmSucc _ s) = f (n+1) s
          f n _  = parens (text "succ " <> printtmATerm False ctx t1)
      in f 1 t1
    t' -> parens (printtmTerm outer ctx t')

printtm :: Context Info -> TermInfo -> Doc
printtm = printtmTerm True

prbinding :: Context Info -> Binding Info -> Doc
prbinding ctx b =
  case b of
    NameBind -> empty
    (TmAbbBind t) -> equals <> printtm ctx t

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

-- foreign export ccall saEvaluate :: StablePtr TermInfo -> IO ()
-- saEvaluate s =
--   do
--     t <- deRefStablePtr s
--     freeStablePtr s
--     catch  
--       ((print . render . printtm . eval) t)
--       (\(e::NoRuleApplies) -> print e)

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

-- foreign export ccall saEvaluateEx :: StablePtr TermInfo -> IO ()
-- saEvaluateEx s =
--   do
--     t <- deRefStablePtr s
--     freeStablePtr s
--     catch  
--       (do { t' <- evalEx t; (print . render . printtm) t'; })
--       (\(e::NoRuleApplies) -> print e)

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

foreign export ccall saTrue :: 
  Ptr CommonToken -> 
  IO (StablePtr (Context Info -> Either String (Term Info)))
saTrue token =
  do
    -- obtain the source code line and charPosition from the token
    l <- tokenGetLine token
    c <- tokenGetCharPositionInLine token
    -- return the TmTrue term
    let
      f :: Context Info -> Term Info
      f ctx = TmTrue (Info l c)
        where i = Info l c
     in
      newStablePtr (Right . f)


foreign export ccall saFalse :: 
  Ptr CommonToken -> 
  IO (StablePtr (Context Info -> Either String (Term Info)))
saFalse token =
  do
    -- obtain the source code line and charPosition from the token
    l <- tokenGetLine token
    c <- tokenGetCharPositionInLine token
    -- return the TmFalse term
    let
      f :: Context Info -> Term Info
      f ctx = TmFalse (Info l c)
        where i = Info l c
     in
      newStablePtr (Right . f)


foreign export ccall saLcid :: 
  Ptr CommonToken -> 
  IO (StablePtr (Context Info -> Either String (Term Info)))
saLcid token =
  do
    lcid <- tokenGetText token
    -- obtain the source code line and charPosition from the token
    l <- tokenGetLine token
    c <- tokenGetCharPositionInLine token
    -- return the TmFalse term
    let
      f :: Context Info -> Either String (Term Info)
      f ctx =
        either Left (Right . g) (name2index i ctx lcid)
        where
          g :: Int -> Term Info
          g x = TmVar i x (ctxlength ctx)
          i = Info l c
     in
      newStablePtr f
      

-- | { A simple approach to handling errors is to fold the field list into
-- a Maybe String which is either Nothing if there are no Left String errors
-- in any of the fields, or Just String with a concatenated error string.
-- Then it can return Either String (TmRecord Info [(String, Term Info)])
-- foreign export ccall saFieldsInCurlyBraces ::
--   Ptr CommonToken -> 
--   StablePtr (Context Info -> Int -> [(String, (Either String (Term Info), Context Info))]) ->
--   IO (StablePtr (Context Info -> Either String (Term Info)))
-- saFieldsInCurlyBraces leftCurlyToken fs =
--   do
    

foreign export ccall saIntV :: 
  Ptr CommonToken -> 
  IO (StablePtr (Context Info -> Either String (Term Info)))
saIntV token =
  do
    -- read the IntV integer value from the token text into n
    t <- tokenGetText token
    n <- readIO t
    -- obtain the source code line and charPosition from the token
    l <- tokenGetLine token
    c <- tokenGetCharPositionInLine token
    -- return the term, which is TmZero, or TmSucc TmZero, or TmSucc (TmSucc (...TmSucc TmZero))
    let
      f :: Context Info -> Term Info
      f ctx =
        let
          intV :: Int -> Term Info
          intV n'
            | n' <= 0 = TmZero i
            | otherwise = TmSucc i (intV (n' - 1))
              where i = Info l c
        in
          intV n
     in
      newStablePtr (Right . f)


-- | create and return field list fs with one entry f1 in: fields : f1=field (COMMA f2=field)*
foreign export ccall saFieldListNew :: 
  StablePtr (Context Info -> Int -> (String, (Either String (Term Info), Context Info))) ->       -- field function
  IO (StablePtr (Context Info -> Int -> [(String, (Either String (Term Info), Context Info))]))   -- returns field list function
saFieldListNew f1 =
  do
    f1' <- deRefStablePtr f1
    freeStablePtr f1
    let
      f :: Context Info -> Int -> [(String, (Either String (Term Info), Context Info))]
      f ctx i = [f1' ctx i]
      in 
      newStablePtr f

-- | append f2 to field list fs in: f1=field (COMMA f2=field)*
foreign export ccall saFieldListAppend :: 
  StablePtr (Context Info -> Int -> [(String, (Either String (Term Info), Context Info))]) ->   -- field list function
  StablePtr (Context Info -> Int -> [(String, (Either String (Term Info), Context Info))]) ->   -- field list function
  IO (StablePtr (Context Info -> Int -> [(String, (Either String (Term Info), Context Info))])) -- returns field list function
saFieldListAppend fs f2 =
  do
    fs' <- deRefStablePtr fs
    freeStablePtr fs
    f2' <- deRefStablePtr f2
    freeStablePtr f2
    let
      f :: Context Info -> Int -> [(String, (Either String (Term Info), Context Info))]
      f ctx i = (fs' ctx i) ++ (f2' ctx (i+1))
      in
      newStablePtr f

-- | Field : LCID EQ Term
foreign export ccall saLcidEqTerm ::
  Ptr CommonToken ->                                                            -- LCID token text
  StablePtr (Context Info -> (Either String (Term Info), Context Info)) ->                      -- term function
  IO (StablePtr (Context Info -> Int -> (String, (Either String (Term Info), Context Info))))   -- returns field function
saLcidEqTerm token t =
  do
    -- read the LCID token text
    l <- tokenGetText token
    -- obtain the term function
    t' <- deRefStablePtr t
    freeStablePtr t
    let
      f :: Context Info -> Int -> (String, (Either String (Term Info), Context Info))
      f ctx i = (l, t' ctx)
      in
      newStablePtr f

-- | Field : Term
foreign export ccall saFieldTerm :: 
  StablePtr (Context Info -> (Either String (Term Info), Context Info)) ->                      -- term function
  IO (StablePtr (Context Info -> Int -> (String, (Either String (Term Info), Context Info))))   -- returns field function
saFieldTerm t =
  do
    t' <- deRefStablePtr t
    freeStablePtr t
    let
      f :: Context Info -> Int -> (String, (Either String (Term Info), Context Info))
      f ctx i = (show i, t' ctx)
      in
      newStablePtr f

