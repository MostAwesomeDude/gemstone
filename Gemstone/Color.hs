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
module Gemstone.Color where

import Data.Traversable
import System.Random

import Graphics.Rendering.OpenGL

import Gemstone.Random

type RGB = Color3 GLubyte

-- | A color constructor.
makeColor :: GLubyte -> GLubyte -> GLubyte -> RGB
makeColor = Color3

red, green, blue :: RGB
red = Color3 255 0 0
green = Color3 0 255 0
blue = Color3 0 0 255

black, white :: RGB
black = Color3 0 0 0
white = Color3 255 255 255

skyBlue, grassGreen, stoneGray :: RGB
skyBlue = Color3 127 127 255
grassGreen = Color3 0 255 63
stoneGray = Color3 127 127 127

hotPink :: RGB
hotPink = Color3 255 105 180

-- | Vary a color by a given amount, using the provide random number
--   generator.
varyColor :: GLubyte -> StdGen -> RGB -> (StdGen, RGB)
varyColor variance gen (Color3 r g b) = (gen', Color3 r' g' b')
    where
    (gen', [r', g', b']) = mapAccumL jitter gen zipped
    zipped = zip [r, g, b] $ repeat variance
