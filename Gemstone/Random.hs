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
module Gemstone.Random where

import Data.Tuple
import System.Random

-- | Add some jitter to a number which can be randomized.
--
--   The result is swapped around so that it can be used with mapAccumL or
--   other poor-man's-State functions.
jitter :: (Num a, Random a) => StdGen -> (a, a) -> (StdGen, a)
jitter g (x, j) = swap $ randomR (x - j, x + j) g
