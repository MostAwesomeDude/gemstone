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

module Gemstone.Animation where

import Control.Lens
import Data.Fixed

import Gemstone.Sprite

-- | An animation.
--
--   The type parameter represents units over time; for example, pixels per
--   second.
data Animation a = Animation { _aFrameList  :: [Sprite a]
                             , _aFrameIndex :: Int
                             , _aFrameElapsed, _aFrameLength :: a }
    deriving (Functor, Show)

makeLenses ''Animation

-- | Make a simple 'Animation' from a 'Sprite'.
animated :: Num a => Sprite a -> Animation a
animated s = Animation [s] 0 0 0

-- | Traverse the current frame of an animation.
currentFrame :: Traversal' (Animation a) (Sprite a)
currentFrame f a = aFrameList (ix (a ^. aFrameIndex) f) a

-- | Update an 'Animation' frame timer, and possibly advance it.
updateAnimation :: (Real a) => a -> Animation a -> Animation a
updateAnimation delta animation = animation &~ do
    totalElapsed <- uses aFrameElapsed (+ delta)
    frameLength <- use aFrameLength
    let (di, ft) = totalElapsed `divMod'` frameLength
    aFrameIndex .= di
    aFrameElapsed .= ft
