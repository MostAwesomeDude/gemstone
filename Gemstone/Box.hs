{-# LANGUAGE DeriveFunctor, EmptyDataDecls, TemplateHaskell #-}
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
module Gemstone.Box (
    Box(), BoxLike(..), unBox, box,
    pInter, bInter,
    makeXYWH, makeXYWHValid, makeXYXYValid, squareAt,
    bLeft, bBot, bRight, bTop,
    bW, bH, bW', bH',
    bX, bY, bX', bY', bXY, bXY',
    center,
    scaleBox,
) where

import Control.Applicative
import Control.Lens

data BoxLike v = BoxLike { _bx1, _by1, _bx2, _by2 :: v }
    deriving (Functor, Show)

makeLenses ''BoxLike

newtype Box v = Box { unBox :: BoxLike v }
    deriving (Functor, Show)

-- | Whether two vertices would determine the lower-left and upper-right corners
--   of a rectangle.
--
--   This is the predicate for determining whether a box is valid.
pInter :: Ord v => v -> v -> v -> v -> Bool
pInter x1 y1 x2 y2 = x1 < x2 && y1 < y2

-- | Prism for getting a Box from a BoxLike.
--
--   The Box constructor isn't exported, so this is the only way to obtain a
--   valid Box.
box :: Ord v => Simple Prism (BoxLike v) (Box v)
box = prism unBox f
    where
    f b | predicate b = Right $ Box b
        | otherwise = Left b
    predicate (BoxLike x1 y1 x2 y2) = pInter x1 y1 x2 y2

-- | Whether two boxes intersect.
--
--   The boxes have to be good.
bInter :: Ord v => Box v -> Box v -> Bool
bInter (Box b1) (Box b2) =
    pInter (b1 ^. bx1) (b1 ^. by1) (b2 ^. bx2) (b2 ^. by2) &&
    pInter (b2 ^. bx1) (b2 ^. by1) (b1 ^. bx2) (b1 ^. by2)

-- | Make a box with width and height.
makeXYWH :: (Ord v, Num v) => v -> v -> v -> v -> BoxLike v
makeXYWH x y w h = BoxLike x y (x + w) (y + h)

-- | Like makeXYWH, but valid.
--
--   Includes a slap on the face if the box is not valid.
makeXYWHValid :: (Ord v, Num v) => v -> v -> v -> v -> Box v
makeXYWHValid x y w h = makeXYWH x y w h ^?! box

-- | Make a valid box.
--
--   Same signature as BoxLike, but making a Box.
makeXYXYValid :: (Ord v, Num v) => v -> v -> v -> v -> Box v
makeXYXYValid x1 y1 x2 y2 = BoxLike x1 y1 x2 y2 ^?! box

-- | Put a square around a point.
-- 
--   The square is described by the 'x' and 'y' coordinates of the center, and
--   the radius 'r'.
--
--   >>> squareAt 1 1 1
--   Box {unBox = BoxLike {_bx1 = 0, _by1 = 0, _bx2 = 2, _by2 = 2}}
squareAt :: (Num a, Ord a) => a -> a -> a -> Box a
squareAt x y r = makeXYXYValid (x - r) (y - r) (x + r) (y + r)

-- | Resize a box by moving an edge.
--
--   Inherently unsafe.
bLeft, bBot, bRight, bTop :: Simple Lens (BoxLike a) a
bLeft   = bx1
bBot    = by1
bRight  = bx2
bTop    = by2

-- | Resize a box by changing its width or height.
--
--   Inherently unsafe.
bW, bH, bW', bH' :: Num a => Simple Lens (BoxLike a) a
bW  f (BoxLike x1 y1 x2 y2) =
    fmap (\w -> BoxLike x1 y1 (x2 + w) y2) (f (x2 - x1))
bH  f (BoxLike x1 y1 x2 y2) =
    fmap (\h -> BoxLike x1 y1 x2 (y2 + h)) (f (y2 - y1))
bW' f (BoxLike x1 y1 x2 y2) =
    fmap (\w -> BoxLike (x1 - w) y1 x2 y2) (f (x2 - x1))
bH' f (BoxLike x1 y1 x2 y2) =
    fmap (\h -> BoxLike x1 (y1 - h) x2 y2) (f (y2 - y1))

-- | Move a box.
bX, bY, bX', bY' :: Num a => Simple Lens (Box a) a
bX f (Box (BoxLike x1 y1 x2 y2)) =
    fmap (\x' -> Box (BoxLike x' y1 (x' + x2 - x1) y2)) (f x1)
bY f (Box (BoxLike x1 y1 x2 y2)) =
    fmap (\y' -> Box (BoxLike x1 y' x2 (y' + y2 - y1))) (f y1)
bX' f (Box (BoxLike x1 y1 x2 y2)) =
    fmap (\x' -> Box (BoxLike (x' + x1 - x2) y1 x' y2)) (f x2)
bY' f (Box (BoxLike x1 y1 x2 y2)) =
    fmap (\y' -> Box (BoxLike x1 (y' + y1 - y2) x2 y')) (f y2)

-- Move a box more efficiently.
bXY, bXY' :: Num a => Simple Lens (Box a) (a, a)
bXY f (Box (BoxLike x1 y1 x2 y2)) = let
    f' (w, h) = Box $ BoxLike w h (w + x2 - x1) (h + y2 - y1)
    in f' <$> f (x1, y1)
bXY' f (Box (BoxLike x1 y1 x2 y2)) = let
    f' (w, h) = Box $ BoxLike (w + x1 - x2) (h + y1 - y2) w h
    in f' <$> f (x2, y2)

-- The center of a box. Read-only.
center :: Fractional a => Box a -> (a, a)
center (Box (BoxLike x1 y1 x2 y2)) = ((x1 + x2) / 2, (y1 + y2) / 2)

-- Scale a box.
scaleBox :: (Eq v, Num v) => v -> v -> Box v -> Box v
scaleBox 0 _ _ = error "scaleBox: Zero width"
scaleBox _ 0 _ = error "scaleBox: Zero height"
scaleBox sx sy (Box (BoxLike x1 y1 x2 y2)) =
    Box $ BoxLike (x1 * sx) (y1 * sy) (x2 * sx) (y2 * sy)
