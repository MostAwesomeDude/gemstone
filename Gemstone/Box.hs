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

module Gemstone.Box (
    Box(), BoxLike(..), unBox, box,
    pInter, bInter,
    makeXYWH, makeXYWHValid, makeXYXYValid, squareAt,
    bLeft, bBot, bRight, bTop,
    bW, bH, -- bW', bH',
    bX, bY, bX', bY',
    bXY, bXY',
    center,
    scaleBox,
) where

import Control.Applicative
import Control.Lens
import Linear

data BoxLike a = BoxLike { _bl, _tr :: V2 a }
    deriving (Functor, Show)

makeLenses ''BoxLike

newtype Box a = Box { unBox :: BoxLike a }
    deriving (Functor, Show)

-- | Whether two vertices would determine the lower-left and upper-right corners
--   of a rectangle.
--
--   This is the predicate for determining whether a box is valid.
pInter :: Ord a => V2 a -> V2 a -> Bool
pInter (V2 x1 y1) (V2 x2 y2) = x1 < x2 && y1 < y2

-- | Prism for getting a Box from a BoxLike.
--
--   The Box constructor isn't exported, so this is the only way to obtain a
--   valid Box.
box :: Ord a => Simple Prism (BoxLike a) (Box a)
box = prism unBox f
    where
    f b | predicate b = Right $ Box b
        | otherwise = Left b
    predicate (BoxLike x y) = pInter x y

-- | Whether two boxes intersect.
--
--   The boxes have to be good.
bInter :: Ord a => Box a -> Box a -> Bool
bInter (Box b1) (Box b2) =
    pInter (b1 ^. bl) (b2 ^. tr) && pInter (b2 ^. bl) (b1 ^. tr)

-- | Make a box with width and height.
makeXYWH :: (Ord a, Num a) => a -> a -> a -> a -> BoxLike a
makeXYWH x y w h = BoxLike (V2 x y) (V2 (x + w) (y + h))

-- | Like makeXYWH, but valid.
--
--   Includes a slap on the face if the box is not valid.
makeXYWHValid :: (Ord a, Num a) => a -> a -> a -> a -> Box a
makeXYWHValid x y w h = makeXYWH x y w h ^?! box

-- | Make a valid box.
--
--   Same signature as BoxLike, but making a Box.
makeXYXYValid :: (Ord a, Num a) => a -> a -> a -> a -> Box a
makeXYXYValid x1 y1 x2 y2 = BoxLike (V2 x1 y1) (V2 x2 y2) ^?! box

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
bLeft, bBot, bRight, bTop :: Lens' (BoxLike a) a
bLeft   = bl . _x
bBot    = bl . _y
bRight  = tr . _x
bTop    = tr . _y

-- | Resize a box by changing its width or height.
--
--   Inherently unsafe.
bW, bH :: Num a => Getter (BoxLike a) a
bW = to $ \(BoxLike (V2 x _) (V2 x' _)) -> x' - x
bH = to $ \(BoxLike (V2 _ y) (V2 _ y')) -> y' - y
-- bW' f (BoxLike x1 y1 x2 y2) =
--     fmap (\w -> BoxLike (x1 - w) y1 x2 y2) (f (x2 - x1))
-- bH' f (BoxLike x1 y1 x2 y2) =
--     fmap (\h -> BoxLike x1 (y1 - h) x2 y2) (f (y2 - y1))

-- | Move a box.
-- bX, bY, bX', bY' :: Num a => Lens' (Box a) a
bX, bY, bX', bY' :: (Ord a, Num a) => Fold (Box a) a
bX = re box . lens (\(BoxLike (V2 x _) _) -> x)
    (\(BoxLike (V2 x y) (V2 x' y')) x'' -> BoxLike (V2 x'' y) (V2 (x' + x'' - x) y'))
bY = re box . lens (\(BoxLike (V2 _ y) _) -> y)
    (\(BoxLike (V2 x y) (V2 x' y')) y'' -> BoxLike (V2 x y'') (V2 x' (y' + y'' - y)))
bX' = re box . lens (\(BoxLike _ (V2 x _)) -> x)
    (\(BoxLike (V2 x y) (V2 x' y')) x'' -> BoxLike (V2 (x + x'' - x') y) (V2 x'' y'))
bY' = re box . lens (\(BoxLike _ (V2 _ y)) -> y)
    (\(BoxLike (V2 x y) (V2 x' y')) y'' -> BoxLike (V2 x (y + y'' - y')) (V2 x' y''))

-- Move a box more efficiently.
bXY, bXY' :: Num a => Lens' (Box a) (V2 a)
bXY f (Box (BoxLike x y)) = let
    f' x' = Box $ BoxLike x' (y + x' - x)
    in f' <$> f x
bXY' f (Box (BoxLike x y)) = let
    f' y' = Box $ BoxLike (x + y' - y) y'
    in f' <$> f y

-- The center of a box. Read-only.
center :: Fractional a => Box a -> V2 a
center (Box (BoxLike x y)) = (x + y) / 2

-- Scale a box.
scaleBox :: (Eq a, Num a) => a -> a -> Box a -> Box a
scaleBox 0 _ _ = error "scaleBox: Zero width"
scaleBox _ 0 _ = error "scaleBox: Zero height"
scaleBox sx sy (Box (BoxLike x y)) =
    Box $ BoxLike (x * v) (y * v)
    where v = V2 sx sy
