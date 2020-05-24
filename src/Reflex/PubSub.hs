{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TypeApplications      #-}

module Reflex.PubSub where

import Prelude                hiding (lookup)

import Control.Monad.Reader
import Data.Constraint.Extras
import Data.Dependent.Map
import Data.Dependent.Sum
import Data.Functor.Identity

import Reflex

type PubSubT t tag m = EventWriterT t [DSum tag Identity] (ReaderT (Event t (DMap tag Identity)) m)

-- | 'PubSub' enables you to broadcast events from anywhere in your reflex network to
-- any location that is subscribed to the topic they are published to. Topics are best
-- representated as constructors of some custom GADT which carries a phantom type to
-- constrain the messages that can be published to it.
class (Reflex t, Monad m, Semigroup a) => PubSub t tag m a where
  publish :: tag a -> Event t a -> m ()
  subscribe :: tag a -> m (Event t a)

runPubSubT :: (MonadFix m, Has Semigroup tag, GCompare tag, Reflex t) => PubSubT t tag m a -> m a
runPubSubT pubSub = mdo
  (result, publishE) <- runReaderT (runEventWriterT pubSub) $
    fromListWithKey (\k v1 v2 -> has @Semigroup k (v1 <> v2)) <$> publishE
  pure result

instance (Reflex t, Monad m, GCompare tag, Semigroup a) => PubSub t tag (PubSubT t tag m) a where
  publish topic payloadE =
    tellEvent $ (\pl -> [topic ==> pl]) <$> payloadE

  subscribe topic = do
    topicPublishE <- asks $ fmap (lookup topic)
    pure $ fmapMaybe (fmap runIdentity) topicPublishE
