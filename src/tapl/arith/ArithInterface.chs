-- Copyright (c)2010-2011, Mark Wright.  All rights reserved.

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
module ArithInterface where

#include "ArithCInterface.h"

{#import Text.Antlrc.Lexer#}

import Prelude hiding (LT, EQ, GT)
import C2HS
import Foreign.Ptr
import System.IO.Unsafe
import Foreign.C
import Control.Monad
import Control.Applicative ((<$>))

{#enum ArithTokens {} deriving (Eq, Show)#}


