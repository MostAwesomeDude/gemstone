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
module Gemstone.GL where

import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.UI.SDL (glSwapBuffers)

checkErrors :: IO ()
checkErrors = do
    es <- get errors
    if null es
        then putStrLn "All clear!"
        else putStrLn ("Error: " ++ show es)

checkExtensions :: IO ()
checkExtensions = let
    required = ["ARB_texture_rectangle", "ARB_texture_non_power_of_two"]
    f x = elem $ "GL_" ++ x
    in do
        exts <- get glExtensions
        forM_ required $ \x -> if f x exts
            then putStrLn $ "Found extension " ++ x
            else error $ "Need extension " ++ x

clearScreen :: IO ()
clearScreen = do
    clearColor $= Color4 0.1 0.1 0.1 0.0
    clear [ColorBuffer]

finishFrame :: IO ()
finishFrame = glSwapBuffers

-- | Resize the viewport such that:
--    * The smallest dimension still corresponds to at least [0, 1]
--    * The viewport is centered on (0.5, 0.5)
resizeViewport :: GLsizei -> GLsizei -> IO ()
resizeViewport w h
    | w > h     = viewport $= (Position ((w-3*h) `div` 2) (-h), Size (2*h) (2*h))
    | otherwise = viewport $= (Position (-w) ((h-3*w) `div` 2), Size (2*w) (2*w))

-- | Turn on alpha blending.
enableBlending :: IO ()
enableBlending = do
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

-- | Set some initial GL state that should be set once and only once.
--
--   It doesn't hurt to call this action multiple times.
prepareGLState :: IO ()
prepareGLState = do
    putStrLn "Preparing GL state..."
    checkErrors
    enableBlending
    putStrLn "Finished preparing GL state!"
    checkErrors
