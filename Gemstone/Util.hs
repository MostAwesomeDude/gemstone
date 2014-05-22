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
module Gemstone.Util where

import Data.Word
import Foreign

import Graphics.Rendering.OpenGL

checkerboard :: [Word8]
checkerboard = map (* 255) [ (x + y) `mod` 2 | x <- [0..7], y <- [0..7] ]

withWord8Ptr :: [Word8] -> (Ptr Word8 -> IO b) -> IO b
withWord8Ptr bytes action = let
    len = length bytes
    indexed = zip [0..] bytes
    in allocaBytes len $ \ptr -> do
        mapM_ (uncurry $ pokeElemOff ptr) indexed
        action ptr

-- | Pack a finite list of Word8s into a texture.
word8ToTexture :: Int -> Int -> [Word8] -> IO TextureObject
word8ToTexture width height bytes = do
    [texobj] <- genObjectNames 1
    textureBinding Texture2D $= Just texobj
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    withWord8Ptr bytes $ \ptr -> let
        pd = PixelData Luminance UnsignedByte ptr
        size = TextureSize2D (fromIntegral width) (fromIntegral height)
        in texImage2D Nothing NoProxy 0 Luminance' size 0 pd
    textureBinding Texture2D $= Nothing
    return texobj

checkerboardTexture :: IO TextureObject
checkerboardTexture = word8ToTexture 8 8 checkerboard
