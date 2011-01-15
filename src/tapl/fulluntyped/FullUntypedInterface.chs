-- Copyright (c)2010-2011, Mark Wright.  All rights reserved.

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
module FullUntypedInterface where

#include "FullUntypedCInterface.h"

{#import Text.Antlrc.Lexer#}

import Prelude hiding (LT, EQ, GT)
import C2HS
import Foreign.Ptr
import System.IO.Unsafe
import Foreign.C
import Control.Monad
import Control.Applicative ((<$>))

{#enum FullUntypedTokens {} deriving (Eq, Show)#}


