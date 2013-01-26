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
gemstoneMain :: IO a -> Loop a -> IO ()
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
