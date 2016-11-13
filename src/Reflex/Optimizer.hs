{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.Optimizer (plugin) where

#ifdef ghcjs_HOST_OS
import Plugins
#else
import Control.Arrow
import CoreMonad
import Data.String
import GhcPlugins
#endif

#ifdef ghcjs_HOST_OS

plugin :: Plugin
plugin = undefined

#else

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install [] p = return $ makeInlinable : p
install options@(_:_) p = do
  warnMsg $ "Reflex.Optimizer: ignoring " <> fromString (show $ length options) <> " command-line options"
  install [] p

makeInlinable :: CoreToDo
makeInlinable = CoreDoPluginPass "MakeInlinable" $ \modGuts -> do
  let f v = setIdInfo v $ let i = idInfo v in
        setInlinePragInfo i $ let p = inlinePragInfo i in
        if isDefaultInlinePragma p
        then defaultInlinePragma { inl_inline = Inlinable }
        else p
      newBinds = flip map (mg_binds modGuts) $ \case
        NonRec b e -> NonRec (f b) e
        Rec bes -> Rec $ map (first f) bes
  return $ modGuts { mg_binds = newBinds }

#endif
