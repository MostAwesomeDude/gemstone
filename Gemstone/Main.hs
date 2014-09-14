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
module Gemstone.Main where

import Control.Monad
import Control.Monad.Trans.State
import Graphics.UI.SDL as SDL

import Gemstone.GL
import Gemstone.Loop

-- | Main entrypoint for Gemstone.
--
--   Note that SDL state (and, by extension, GL state) doesn't exist outside
--   of this function. However, SDL will be started before the globals are
--   populated, so it is possible to run SDL and/or GL actions in the setup.
--
--   >>> main = gemstoneMain makeGlobals (setupState >> mainLoop)
gemstoneMain :: IO g -> Loop g () -> IO ()
gemstoneMain setup loop = withInit [InitEverything] $ do
    -- Order of operations matters, doesn't it...
    -- First, grab the gem state. This gets us the surface and enables GL.
    initialGems <- getInitialGems
    -- Now we can check GL extensions and set up our GL state.
    checkExtensions
    prepareGLState
    -- And now the app-specific state can be created.
    initialState <- setup
    void $ runStateT loop (initialGems, initialState)
