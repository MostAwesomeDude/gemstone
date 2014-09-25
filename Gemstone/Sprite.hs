{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}

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

module Gemstone.Sprite where

import Control.Lens
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import Linear

import Gemstone.Color
import Gemstone.Drawable

data Material = Colored RGB (Maybe GLubyte)
              | Textured TextureObject
              | Text Font String RGB
    deriving (Show)

data Sprite a = Sprite { _sMaterial :: Material
                       , _sSize :: V2 a }
    deriving (Functor, Show)

makeLenses ''Sprite

addAlpha :: c -> Color3 c -> Color4 c
addAlpha a (Color3 r g b) = Color4 r g b a

instance Drawable Sprite where
    draw (Sprite material size) offset = case material of
        Colored c malpha -> do
            case malpha of
                Just alpha -> color $ addAlpha alpha c
                Nothing -> color c
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
            color hotPink
            renderPrimitive Quads $ do
                -- For some reason, these need to be wound in the opposite
                -- direction in order to get the texture correctly oriented. Bug
                -- in the image loader?
                texCoord (TexCoord2 s t')
                vertex (Vertex2 x y)
                texCoord (TexCoord2 s' t')
                vertex (Vertex2 x' y)
                texCoord (TexCoord2 s' t)
                vertex (Vertex2 x' y')
                texCoord (TexCoord2 s t)
                vertex (Vertex2 x y')
            texture Texture2D $= Disabled
        Text font string c -> do
            color c
            preservingMatrix $ do
                translate $ Vector3 x y 0
                scale h h 1
                renderFont font string All
        where
        (V2 x y) = offset
        (V2 x' y') = size + offset
        h = y / 2
        s = 0 :: GLfloat
        t = 0 :: GLfloat
        s' = 1
        t' = 1

-- | Small helper for putting together colored sprites.
colored :: RGB -> V2 a -> Sprite a
colored c = Sprite $ Colored c Nothing
