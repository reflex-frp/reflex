{-# LANGUAGE CPP #-}

-- |
-- Module:
--   Reflex.Spider
-- Description:
--   This module exports all of the user-facing functionality of the 'Spider' 'Reflex' engine
module Reflex.Spider
  ( Spider
  , SpiderTimeline
  , Global
  , SpiderHost
  , runSpiderHost
  , runSpiderHostForTimeline
  , newSpiderTimeline
  , withSpiderTimeline
  ) where

import Reflex.Spider.Internal
