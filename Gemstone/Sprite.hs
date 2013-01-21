{-# LANGUAGE TemplateHaskell #-}
module Gemstone.Sprite where

import Control.Lens
import Graphics.Rendering.OpenGL

import Gemstone.Box
import Gemstone.Color

data Material = Colored RGB (Maybe GLubyte)
              | Textured TextureObject
    deriving (Show)

data Sprite a = Sprite { _sMaterial :: Material
                       , _sBox :: Box a }
    deriving (Show)

makeLenses ''Sprite

addAlpha :: c -> Color3 c -> Color4 c
addAlpha a (Color3 r g b) = Color4 r g b a

drawSprite :: (Ord c, VertexComponent c) => Sprite c -> IO ()
drawSprite (Sprite material b) = case material of
    Colored c malpha -> do
        case malpha of
            Just alpha -> do
                blend $= Enabled
                blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
                color $ addAlpha alpha c
            Nothing -> do
                blend $= Disabled
                color c
        renderPrimitive Quads $ do
            vertex (Vertex2 x y)
            vertex (Vertex2 x' y)
            vertex (Vertex2 x' y')
            vertex (Vertex2 x y')
    Textured texobj -> do
        texture Texture2D $= Enabled
        activeTexture $= TextureUnit 0
        textureBinding Texture2D $= Just texobj
        textureFunction $= Replace
        renderPrimitive Quads $ do
            texCoord (TexCoord2 r s)
            vertex (Vertex2 x y)
            texCoord (TexCoord2 r' s)
            vertex (Vertex2 x' y)
            texCoord (TexCoord2 r' s')
            vertex (Vertex2 x' y')
            texCoord (TexCoord2 r s')
            vertex (Vertex2 x y')
        texture Texture2D $= Disabled
    where
    rbox = remit box
    x = b ^. rbox . bLeft
    y = b ^. rbox . bBot
    x' = b ^. rbox . bRight
    y' = b ^. rbox . bTop
    r = 0 :: GLfloat
    s = 0 :: GLfloat
    r' = 1
    s' = 1

drawSprites :: [Sprite GLfloat] -> IO ()
drawSprites = mapM_ drawSprite
