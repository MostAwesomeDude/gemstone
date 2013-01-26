module Gemstone.Main where

import Control.Monad
import Control.Monad.Trans.State
import Graphics.UI.SDL as SDL

import Gemstone.GL
import Gemstone.Loop

-- | Main entrypoint for Gemstone.
--
--   Note that SDL state (and, by extension, GL state) doesn't exist outside
--   of this function, so any one-time setup of state needs to be done inside
--   the loop.
--
--   >>> main = gemstoneMain globals (setupState >> mainLoop)
gemstoneMain :: a -> Loop a -> IO ()
gemstoneMain globals loop = withInit [InitEverything] $ do
    initial <- getInitialGems
    checkExtensions
    prepareGLState
    void $ runStateT loop (initial, globals)
