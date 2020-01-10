{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif

module Reflex.Widget.Basic where

import Control.Monad.Fix (MonadFix)
import Data.Map (Map)

import Reflex.Class
import Reflex.Adjustable.Class
import Data.Patch.MapWithMove


-- | Build sortable content in such a way that re-sorting it can cause minimal
-- disruption to an existing context.
--
-- Naively re-sorting a list of images would destroy every image and add them back
-- in the new order. This framework is able to avoid that by preserving the
-- identity of each image and simply moving it to the new location.
--
-- Example:
--
-- > let sortByFst = buttonA $> comparing fst
-- >     sortBySnd = buttonB $> comparing snd
-- >     sortEvent = leftmost [sortByFst, sortBySnd]
-- > sortableList
-- >   (\k v -> text $ "\n" ++ show k ++ " " ++ v)  -- show each element on a new line
-- >   (Map.fromList $ zip [0..] [(3, "a"), (2, "b"), (1, "c")])
-- >   sortEvent
sortableList :: forall t m k v a. (MonadHold t m, MonadFix m, Adjustable t m, Ord k)
             => (k -> v -> m a) -- ^ Function to render the content for each key/value pair
             -> Map k v -- ^ The sortable list with an initial ordering determined by the @Map@ keys in ascending order
             -> Event t (v -> v -> Ordering) -- ^ An event carrying a sort function for the list
             -> m (Map k a)
sortableList f m0 reSortFunc = do
  rec let reSortPatch = attachWith (flip patchThatSortsMapWith) (currentIncremental m) reSortFunc
      m <- holdIncremental m0 reSortPatch
  (results, _) <- mapMapWithAdjustWithMove f m0 reSortPatch
  pure results
