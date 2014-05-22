{-# LANGUAGE TemplateHaskell #-}
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
module Main where

import Control.Lens
import Control.Monad.Trans.Class

import Graphics.Rendering.OpenGL

import Gemstone.Box
import Gemstone.Color
import Gemstone.GL
import Gemstone.Loop
import Gemstone.Main
import Gemstone.Sprite
import Gemstone.Util

data Globals = Globals { _gBoard :: Sprite GLfloat }
    deriving (Show)

makeLenses ''Globals

makeGlobals :: IO Globals
makeGlobals = do
    texobj <- checkerboardTexture
    let s = Sprite (Textured texobj) $ makeXYWHValid 0.1 0.4 0.2 0.2
    return $ Globals s

mainLoop :: Loop Globals
mainLoop = simpleLoop draw
    where
    bg :: Sprite GLfloat
    bg = colored white $ makeXYXYValid 0 0 1 1
    solid :: Sprite GLfloat
    solid = colored blue $ makeXYWHValid 0.4 0.4 0.2 0.2
    transparent :: Sprite GLfloat
    transparent = Sprite (Colored green (Just 127)) $
        makeXYWHValid 0.7 0.4 0.2 0.2
    draw :: Loop Globals
    draw = do
        lift clearScreen
        board <- use $ _2 . gBoard
        lift $ drawSprites [bg, board, solid, transparent]
        lift finishFrame

main :: IO ()
main = gemstoneMain makeGlobals mainLoop
