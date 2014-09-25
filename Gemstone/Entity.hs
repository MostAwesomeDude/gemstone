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

module Gemstone.Entity where

import Control.Applicative
import Control.Lens
import qualified Data.Map as M
import Graphics.Rendering.OpenGL
import Linear

import Gemstone.Animation
import Gemstone.Drawable

-- | An entity.
--
--   The type parameter 's' is the current state of the entity.
--   The type parameter 'a' represents units over time; for example, pixels
--   per second.
data Entity s a = Entity { _eAnimations :: M.Map s (Animation a)
                         , _eState      :: s
                         , _eFrameElapsed, _eFrameLength :: a
                         , _eAcceleration, _eVelocity, _ePosition :: V2 a }
    deriving (Functor, Show)

makeLenses ''Entity

entity :: Num a => s -> Animation a -> Entity s a
entity s a = Entity (M.singleton s a) s 0 0 0 0 0

mapEntityState :: Ord t => (s -> t) -> Entity s a -> Entity t a
mapEntityState f (Entity as s fe fl a v e) =
    Entity (M.mapKeys f as) (f s) fe fl a v e

-- | Integrate acceleration into velocity and velocity into position.
integrate :: Num a => a -> Entity s a -> Entity s a
integrate a e = e &~ do
    acceleration <- use eAcceleration
    velocity <- eVelocity <+= acceleration * dt
    ePosition += velocity * dt
    where dt = pure a

currentAnimation :: Ord s => Traversal' (Entity s a) (Animation a)
currentAnimation f e = eAnimations (ix (e ^. eState) f) e

drawEntity :: Ord s => Entity s GLfloat -> IO ()
drawEntity e = case e ^? currentAnimation . currentFrame of
    Just s  -> draw s (e ^. ePosition)
    Nothing -> putStrLn "drawEntity: Warning: No-op"

-- | Move an 'Entity' according to a delta.
moveEntity :: Num a => a -> Entity s a -> Entity s a
moveEntity delta e = e & ePosition +~ pure delta
