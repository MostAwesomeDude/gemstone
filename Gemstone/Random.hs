module Gemstone.Random where

import Data.Tuple
import System.Random

-- | Add some jitter to a number which can be randomized.
--
--   The result is swapped around so that it can be used with mapAccumL or
--   other poor-man's-State functions.
jitter :: (Num a, Random a) => StdGen -> (a, a) -> (StdGen, a)
jitter g (x, j) = swap $ randomR (x - j, x + j) g
