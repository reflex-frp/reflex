module Data.Unique.Tag.Local
  ( Tag
  , TagGen (..)
  , TagGenT
  , tagId
  , unsafeTagFromId
  , runTagGenT
  , mapTagGenT
  ) where

import Data.Unique.Tag.Local.Internal
