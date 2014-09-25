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
module Gemstone.Particles where

import Control.Lens
import Control.Monad
import Data.Traversable
import Data.Word
import Foreign.C.Types
import Graphics.Rendering.OpenGL
import Linear
import System.Random

import Gemstone.Color
import Gemstone.Drawable
import Gemstone.Random
import Gemstone.Sprite

data Particle a = Particle { _pMaterial :: Material
                           , _pSize :: V2 a
                           , _pLifetime :: Int }

makeLenses ''Particle

data Particles a = Particles { _pGen :: StdGen
                             , _pCenter :: V2 a
                             , _pColor :: RGB
                             , _pColorVariance :: Word8
                             , _pParticles :: [(Particle a, V2 a)] }

makeLenses ''Particles

instance Drawable Particle where
    draw (Particle mat size _) = draw (Sprite mat size)

drawParticles :: Particles GLfloat -> IO ()
drawParticles ps = forM_ (ps ^. pParticles) $ uncurry draw

particleSprite :: Lens (Particle a) (Particle b) (Sprite a) (Sprite b)
particleSprite = lens (\(Particle m s _) -> Sprite m s)
    (\(Particle _ _ l) (Sprite m s) -> Particle m s l)

particleSprites :: Traversal' (Particles a) (Sprite a)
particleSprites = pParticles . traverse . _1 . particleSprite

makeParticles :: Num a => Particles a
makeParticles = Particles (mkStdGen 0) 0 black 0 []

-- | Clear out a list of particles without resetting any of the rest of the
--   state.
clearParticles :: Particles a -> Particles a
clearParticles = pParticles .~ []

-- | Update the lifetimes of all of the particles, and cull the dead ones.
filterParticles :: (Ord a, Num a)
                => Int -> [(Particle a, b)] -> [(Particle a, b)]
filterParticles ticks ps = filter (\p -> p ^. _1 . pLifetime > 0) ps'
    where ps' = ps & traverse . _1 . pLifetime %~ subtract ticks

tickParticles :: (Floating a, Ord a, Random a)
              => Int -> Particles a -> Particles a
tickParticles ticks (Particles g coords@(V2 cx cy) c cvar ps) =
    Particles g''' coords c cvar ps''
    where
    ps' = filterParticles ticks ps
    ps'' = if length ps < 100 then (newParticle, V2 x y):ps' else ps'
    (g', [x, y]) = mapAccumL jitter g [(cx, 0.01), (cy, 0.01)]
    (life, g'') = randomR (50, 1250) g'
    (g''', c') = varyColor (CUChar cvar) g'' c
    newParticle = Particle (Colored c' (Just . fst $ random g)) 0.01 life
