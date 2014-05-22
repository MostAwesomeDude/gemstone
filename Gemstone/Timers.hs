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
module Gemstone.Timers where

import Control.Lens
import Data.Word

import Gemstone.Maths

data Timers = Timers { _tTimestamp :: Word32
                     , _tDelta     :: Word32
                     , _tFps       :: Float }
    deriving (Show)

makeLenses ''Timers

makeTimers :: Timers
makeTimers = Timers 0 0 0

updateTimestamp :: Word32 -> Timers -> Timers
updateTimestamp w t = let
    delta = w - (t ^. tTimestamp)
    fps = 1000 / fromIntegral delta
    in tTimestamp .~ w $ tDelta .~ delta $ tFps %~ mma fps $ t
