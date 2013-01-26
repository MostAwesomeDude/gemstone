module Gemstone.Util where

import Data.Word
import Foreign

import Graphics.Rendering.OpenGL

checkerboard :: [Word8]
checkerboard = take 64 $ concat $ repeat [0, 255]

withWord8Ptr :: [Word8] -> (Ptr Word8 -> IO b) -> IO b
withWord8Ptr bytes action = let
    len = length bytes
    indexed = zip [0..] bytes
    in allocaBytes len $ \ptr -> do
        mapM_ (\(i, x) -> pokeElemOff ptr i x) indexed
        action ptr

-- | Pack a finite list of Word8s into a texture.
word8ToTexture :: Int -> Int -> [Word8] -> IO TextureObject
word8ToTexture width height bytes = do
    [texobj] <- genObjectNames 1
    textureBinding Texture2D $= Just texobj
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    withWord8Ptr bytes $ \ptr -> let
        pd = PixelData Luminance UnsignedByte ptr
        size = TextureSize2D (fromIntegral width) (fromIntegral height)
        in do
        texImage2D Nothing NoProxy 0 Luminance' size 0 pd
    textureBinding Texture2D $= Nothing
    return texobj

checkerboardTexture :: IO TextureObject
checkerboardTexture = word8ToTexture 8 8 checkerboard
