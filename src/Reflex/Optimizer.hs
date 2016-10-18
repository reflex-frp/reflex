{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Reflex.Optimizer (plugin) where

#ifndef ghcjs_HOST_OS

import GhcPlugins
import Control.Arrow

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install [] p = return $ makeInlinable : p

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

#else

plugin :: ()
plugin = ()

#endif
