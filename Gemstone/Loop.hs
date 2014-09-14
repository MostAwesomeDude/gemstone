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
module Gemstone.Loop where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Graphics.Rendering.OpenGL
import Graphics.UI.SDL

import Gemstone.GL
import Gemstone.Timers

-- | The type of loops.
--
--   The type 'g' is the game-specific global state, and 'a' is the parameter
--   of the 'Monad'.
type Loop g a = StateT (Gems, g) IO a

data Gems = Gems { _gScreen    :: Surface
                 , _gQuitFlag  :: Bool
                 , _gTimers    :: Timers }
    deriving (Show)

makeLenses ''Gems

gems :: Simple Lens (Gems, a) Gems
gems = _1

resizeScreen :: GLsizei -> GLsizei -> IO Surface
resizeScreen w h = let
    flags = [OpenGL, DoubleBuf, Resizable]
    in do
        screen <- setVideoMode (fromIntegral w) (fromIntegral h) 32 flags
        resizeViewport w h
        return screen

getInitialGems :: IO Gems
getInitialGems = do
    screen <- resizeScreen 1 1
    return $ Gems screen False makeTimers

handlePureCoreEvent :: Event -> Gems -> Gems
handlePureCoreEvent (KeyDown (Keysym SDLK_ESCAPE _ _)) = gQuitFlag .~ True
handlePureCoreEvent _ = id

handleCoreEvent :: Event -> StateT Gems IO ()
handleCoreEvent (VideoResize w h) =
    gScreen <~ lift (resizeScreen (fromIntegral w) (fromIntegral h))
handleCoreEvent event = modify $ handlePureCoreEvent event

handleEvents :: (Event -> StateT g IO ()) -> Loop g ()
handleEvents handler = do
    event <- lift pollEvent
    zoom _1 $ handleCoreEvent event
    zoom _2 $ handler event
    -- Continue until all events have been handled.
    when (event /= NoEvent) $ handleEvents handler

updateTimers :: Loop g ()
updateTimers = do
    ticks <- lift getTicks
    gems . gTimers %= updateTimestamp ticks

elapsedTime :: Loop g Float
elapsedTime = use $ gems . gTimers . tDelta . to fromIntegral . to (/ 1000)

-- | Run a game in a loop indefinitely.
gemstoneLoop :: Loop g () -- ^ The pre-drawing action
             -> Loop g () -- ^ The drawing action
             -> Loop g () -- ^ The post-drawing action
             -> Loop g ()
gemstoneLoop before draw after = do
    updateTimers
    before
    draw
    after
    q <- use $ gems . gQuitFlag
    unless q $ gemstoneLoop before draw after

simpleLoop :: Loop g () -> Loop g ()
simpleLoop draw = gemstoneLoop before draw (return ())
    where before = handleEvents (\_ -> return ())
