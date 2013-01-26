{-# LANGUAGE TemplateHaskell #-}
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

mainLoop :: Loop Globals
mainLoop = setup >> simpleLoop draw
    where
    bg :: Sprite GLfloat
    bg = colored white $ makeXYXYValid 0 0 1 1
    solid :: Sprite GLfloat
    solid = colored blue $ makeXYWHValid 0.4 0.4 0.2 0.2
    transparent :: Sprite GLfloat
    transparent = Sprite (Colored green (Just 127)) $
        makeXYWHValid 0.7 0.4 0.2 0.2
    setup :: Loop Globals
    setup = do
        texobj <- lift checkerboardTexture
        let s = Sprite (Textured texobj) $ makeXYWHValid 0.1 0.4 0.2 0.2
        _2 . gBoard .= s
    draw :: Loop Globals
    draw = do
        lift clearScreen
        board <- use $ _2 . gBoard
        lift $ drawSprites [bg, board, solid, transparent]
        lift finishFrame

main :: IO ()
main = do
    gemstoneMain (Globals undefined) mainLoop
