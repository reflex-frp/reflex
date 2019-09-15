{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.FanTag
  ( EventSelectorTag
  , unEventSelectorTag
  , fanTag
  , selectTag
  ) where

import Data.Unique.Tag.Local
import Data.TagMap
import Reflex.Class

import GHC.Exts (Any)
import Unsafe.Coerce

newtype EventSelectorTag t x (v :: k -> *) = EventSelectorTag { unEventSelectorTag :: EventSelectorInt t Any }

fanTag :: Reflex t => Event t (TagMap x v) -> EventSelectorTag t x v
fanTag = EventSelectorTag . fanInt . fmapCheap unTagMap

selectTag :: forall t x v a. Reflex t => EventSelectorTag t x v -> Tag x a -> Event t (v a)
selectTag (EventSelectorTag s) = fmapCheap (unsafeCoerce :: Any -> v a) . selectInt s . tagId
