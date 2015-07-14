module Util(module Stream
           ,module List
           ,execT
           ,v2max,v2min
           ) where

import Stream
import List

import Linear
import Control.Monad.IO.Class
import Control.Monad.State.Strict

--execT :: Monad m => a -> StateT a (StateT outerstate m) forget -> StateT outerstate m a
--execT :: Monad m => a -> StateT a m () -> StateT outside m a
execT ms donext = do
  s <- ms
  (_,ret) <- runStateT donext s
  return ret

v2max (V2 x1 y1) (V2 x2 y2) = V2 (max x1 x2) (max y1 y2)
v2min (V2 x1 y1) (V2 x2 y2) = V2 (min x1 x2) (min y1 y2)
