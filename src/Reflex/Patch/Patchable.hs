{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- The derived instances are undecidable in the case of a pathological instance like
-- instance Patch x where
--   type PatchTarget x = Patchable x
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Patch.Patchable where

-- import Data.Aeson
import GHC.Generics
import Reflex.Patch

-- | Like SemiMap/PartialMap but for anything patchable
data Patchable p
  = Patchable_Patch p
  | Patchable_Complete (PatchTarget p)
  deriving (Generic)

completePatchable :: Patchable p -> Maybe (PatchTarget p)
completePatchable = \case
  Patchable_Complete t -> Just t
  Patchable_Patch _ -> Nothing

deriving instance (Eq p, Eq (PatchTarget p)) => Eq (Patchable p)
deriving instance (Ord p, Ord (PatchTarget p)) => Ord (Patchable p)
deriving instance (Show p, Show (PatchTarget p)) => Show (Patchable p)
deriving instance (Read p, Read (PatchTarget p)) => Read (Patchable p)
-- instance (ToJSON p, ToJSON (PatchTarget p)) => ToJSON (Patchable p)
-- instance (FromJSON p, FromJSON (PatchTarget p)) => FromJSON (Patchable p)

instance (Monoid p, Patch p) => Monoid (Patchable p) where
  mempty = Patchable_Patch mempty
  mappend = (<>)

instance (Semigroup p, Patch p) => Semigroup (Patchable p) where
  (<>) = curry $ \case
    (Patchable_Patch a, Patchable_Patch b) -> Patchable_Patch $ a <> b
    (Patchable_Patch a, Patchable_Complete b) -> Patchable_Complete $ applyAlways a b
    (Patchable_Complete a, _) -> Patchable_Complete a
