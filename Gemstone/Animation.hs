{-# LANGUAGE TemplateHaskell #-}
module Gemstone.Animation where

import Control.Applicative
import Control.Lens
import Linear

import Gemstone.Box
import Gemstone.Sprite

-- | An animation.
--
--   The type parameter represents units over time; for example, pixels per
--   second.
data Animation a = Animation { _aSprite   :: Sprite a
                             , _aVelocity :: V2 a }
    deriving (Show)

makeLenses ''Animation

-- | Make a simple 'Animation' from a 'Sprite'.
animate :: Num a => Sprite a -> Animation a
animate s = Animation s 0

-- | Move an 'Animation' according to a delta.
moveAnimation :: (Num v, Ord v, Show v) => v -> Animation v -> Animation v
moveAnimation delta animation = animation & aSprite . sBox . bXY %~ f
    where
    v' = animation ^. aVelocity * pure delta
    f (x, y) = case V2 x y + v' of V2 x' y' -> (x', y')
