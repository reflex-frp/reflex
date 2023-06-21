{-# LANGUAGE TypeFamilies #-}
module Reflex.Spider.Ref.Types where

class RefCtx ctx where
  data RefName ctx :: *
  traceRef :: RefName ctx -> RefAction -> IO ()

data RefAction
   = RefAction_Write
   | RefAction_Modify
   | RefAction_Modify'
   deriving (Show)
