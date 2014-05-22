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
import Data.Traversable
import Data.Word
import System.Random

import Gemstone.Animation
import Gemstone.Box
import Gemstone.Color
import Gemstone.Random
import Gemstone.Sprite

type Particle a = (Animation a, Int)

data Particles a = Particles { _pGen :: StdGen
                             , _pCenter :: (a, a)
                             , _pColor :: RGB
                             , _pColorVariance :: Word8
                             , _pParticles :: [Particle a] }

makeLenses ''Particles

particleSprites :: Simple Traversal (Particles a) (Sprite a)
particleSprites = pParticles . traverse . _1 . aSprite

makeParticles :: Num a => Particles a
makeParticles = Particles (mkStdGen 0) (0, 0) black 0 []

-- | Clear out a list of particles without resetting any of the rest of the
--   state.
clearParticles :: Particles a -> Particles a
clearParticles = pParticles .~ []

makeParticle :: (Floating v, Ord v) => (v, v) -> Int -> RGB -> Particle v
makeParticle (x, y) lifetime c = (animate s, lifetime)
    where s = colored c $ squareAt x y 0.005

-- | Update the lifetimes of all of the particles, and cull the dead ones.
filterParticles :: (Ord v, Num v) => Int -> [Particle v] -> [Particle v]
filterParticles ticks =
    filter (^. _2 . to (> 0)) . over (traverse . _2) (\x -> x - ticks)

tickParticles :: (Floating v, Ord v, Random v)
               => Int -> Particles v -> Particles v
tickParticles ticks (Particles g coords@(cx, cy) c cvar ps) =
    Particles g''' coords c cvar ps''
    where
    ps' = filterParticles ticks ps
    ps'' = if length ps < 100 then newParticle : ps' else ps'
    (g', [x, y]) = mapAccumL jitter g [(cx, 0.01), (cy, 0.01)]
    (life, g'') = randomR (50, 1250) g'
    (g''', c') = varyColor cvar g'' c
    newParticle = makeParticle (x, y) life c' & _1 . aSprite . sMaterial %~ f
    f material = case material of
        Colored c'' _ -> Colored c'' . Just . fst $ random g
        mat -> mat
