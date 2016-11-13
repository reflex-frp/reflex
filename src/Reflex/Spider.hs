{-# LANGUAGE CPP #-}
-- | This module exports all of the user-facing functionality of the 'Spider'
-- 'Reflex' engine
module Reflex.Spider
       ( Spider
       , SpiderTimeline
       , Global
       , SpiderHost
       , runSpiderHost
#ifndef SPECIALIZE_TO_SPIDERTIMELINE_GLOBAL
       , runSpiderHostForTimeline
       , newSpiderTimeline
       , withSpiderTimeline
#endif
         -- * Deprecated
       , SpiderEnv
       ) where

#ifdef SPECIALIZE_TO_SPIDERTIMELINE_GLOBAL
import Reflex.Class
#endif
import Reflex.Spider.Internal
