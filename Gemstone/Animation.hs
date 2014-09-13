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
module Gemstone.Animation where

import Control.Applicative
import Control.Lens
import Data.Fixed
import Data.List
import Linear

import Gemstone.Box
import Gemstone.Sprite

-- | An animation.
--
--   The type parameter represents units over time; for example, pixels per
--   second.
data Animation a = Animation { _aSprite      :: Sprite a
                             , _aSpriteList  :: [Sprite a]
                             , _aSpriteIndex :: Integer
                             , _aFrameElapsed, _aFrameLength :: a
                             , _aVelocity    :: V2 a }
    deriving (Show)

makeLenses ''Animation

-- | Make a simple 'Animation' from a 'Sprite'.
animated :: Num a => Sprite a -> Animation a
animated s = Animation s [s] 0 0 0 0

-- | Move an 'Animation' according to a delta.
moveAnimation :: (Num v, Ord v, Show v) => v -> Animation v -> Animation v
moveAnimation delta animation = animation & aSprite . sBox . bXY %~ f
    where
    v' = animation ^. aVelocity * pure delta
    f (x, y) = case V2 x y + v' of V2 x' y' -> (x', y')

-- | Advance an 'Animation' to the current frame.
advanceFrame :: Animation a -> Animation a
advanceFrame a = a & aSprite .~ newFrame
    where
    ss = a ^. aSpriteList
    index = a ^. aSpriteIndex
    newFrame = ss `genericIndex` (index `mod` genericLength ss)

-- | Update an 'Animation' frame timer, and possibly advance it.
updateAnimation :: (Real v, Show v) => v -> Animation v -> Animation v
updateAnimation delta animation = moveAnimation delta animation'
    where
    animation' = animation & aFrameElapsed .~ ft & aSpriteIndex +~ di & advanceFrame
    (di, ft) = (animation ^. aFrameElapsed + delta) `divMod'` (animation ^. aFrameLength)
