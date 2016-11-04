module Reflex.FunctorMaybe where

import Data.Maybe

--TODO: See if there's a better class in the standard libraries already
-- | A class for values that combines filtering and mapping using 'Maybe'.
class FunctorMaybe f where
  -- | Combined mapping and filtering function.
  fmapMaybe :: (a -> Maybe b) -> f a -> f b

instance FunctorMaybe [] where
  fmapMaybe f = catMaybes . fmap f

