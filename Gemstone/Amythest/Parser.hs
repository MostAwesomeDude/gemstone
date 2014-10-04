-- Copyright (C) 2014 Google Inc. All rights reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may not
-- use this file except in compliance with the License. You may obtain a copy
-- of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.

module Gemstone.Amythest.Parser where

import Control.Applicative
import Control.Lens
import Control.Monad
import qualified Data.HashSet as HS
import Data.Ratio
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style

data TimeSlice = TSInt Int
               | TSDouble Double
               | TSFrame (Ratio Int)
    deriving (Show)

data TimeUnit = Second | Millisecond
    deriving (Enum, Eq, Ord, Show)

data Timing = Timing TimeSlice TimeUnit
    deriving (Show)

data Amythest a = Ani String String Timing
                | Cancel String String
    deriving (Show)

equals :: TokenParsing m => m ()
equals = void $ highlight Operator $ symbolic '='

leadsTo :: TokenParsing m => m ()
leadsTo = void $ highlight Operator $ symbol "->"

idents :: TokenParsing m => IdentifierStyle m
idents = emptyIdents & styleReserved .~ HS.fromList ["ani", "cancel", "ms", "s"]

timeSlice :: (Monad m, TokenParsing m) => m TimeSlice
timeSlice = do
    n <- naturalOrDouble
    return $ case n of
        Left i  -> TSInt $ fromIntegral i
        Right d -> TSDouble d

timeUnit :: (Monad m, TokenParsing m) => m TimeUnit
timeUnit = reserve idents "ms" *> pure Millisecond
       <|> reserve idents "s" *> pure Second

timing :: (Monad m, TokenParsing m) => m Timing
timing = Timing <$> timeSlice <*> timeUnit

ani :: (Monad m, TokenParsing m) => m (Amythest a)
ani = do
    reserve idents "ani"
    name <- ident idents
    equals
    filename <- stringLiteral
    void comma
    t <- timing
    return $ Ani name filename t

cancel :: (Monad m, TokenParsing m) => m (Amythest a)
cancel = do
    reserve idents "cancel"
    first <- ident idents
    leadsTo
    second <- ident idents
    return $ Cancel first second

parseAmythest :: (Monad m, TokenParsing m) => m [Amythest a]
parseAmythest = many $ choice [ani, cancel]
