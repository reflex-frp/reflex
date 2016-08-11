-- | This module exports all of the user-facing functionality of the 'Spider'
-- 'Reflex' engine
module Reflex.Spider
       ( Spider
       , SpiderTimeline
       , Global
       , SpiderHost
       , runSpiderHost
       , runSpiderHostForTimeline
       , newSpiderTimeline
       , askSpiderTimeline
         -- * Deprecated
       , SpiderEnv
       ) where

import Reflex.Spider.Internal
