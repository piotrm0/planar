module Util(module Stream
           ,module List
--           ,execT
           ,v2max,v2min
           ,withIndex
           ,withinIndex
           ,withIndexT
           ,withinIndexT
           ,frunState
           ,frunStateT
           ,fexecState
           ,fexecStateT
           ,fevalState
           ,fevalStateT
           ) where

import Stream
import List

import Linear
import Control.Monad.IO.Class
import Control.Monad.State.Strict

--execT :: Monad m => a -> StateT a (StateT outerstate m) forget -> StateT outerstate m a
--execT :: Monad m => a -> StateT a m () -> StateT a m a
--execT ms donext = do
----  s <- ms
--  (_,next_state) <- runStateT donext ms
--  return next_state

v2max (V2 x1 y1) (V2 x2 y2) = V2 (max x1 x2) (max y1 y2)
v2min (V2 x1 y1) (V2 x2 y2) = V2 (min x1 x2) (min y1 y2)

withIndex :: Int -> State a r -> State [a] r
withIndex i comp = do
  (alist :: [a]) <- get
  let (prefix,rest) = splitAt i alist
  case rest of
    e : postfix -> let (ret,new_e) = runState comp (e :: a) in do
      put $ prefix ++ [new_e] ++ postfix
      return ret

withinIndex :: Int -> State [a] r -> State [a] r
withinIndex i comp = do
  alist <- get
  let (prefix,rest) = splitAt i alist
  case rest of
    e : postfix -> let (ret,new_es) = runState comp [e] in do
      put $ prefix ++ new_es ++ postfix
      return ret

withIndexT :: (Monad m) => Int -> StateT a m r -> StateT [a] m r
withIndexT i comp = do
  (alist :: [a]) <- get
  let (prefix,rest) = splitAt i alist
  case rest of
    e : postfix -> do (ret,new_e) <- lift $ runStateT comp (e :: a)
                      put $ prefix ++ [new_e] ++ postfix
                      return ret

withinIndexT :: (Monad m) => Int -> StateT [a] m r -> StateT [a] m r
withinIndexT i comp = do
  alist <- get
  let (prefix,rest) = splitAt i alist
  case rest of
    e : postfix -> do (ret,new_es) <- lift $ runStateT comp [e]
                      put $ prefix ++ new_es ++ postfix
                      return ret

frunStateT = (flip runStateT)
frunState = (flip runState)

fexecStateT :: Monad m => s -> StateT s m () -> m s
fexecStateT = (flip execStateT)

fevalStateT :: Monad m => s -> StateT s m r -> m r
fevalStateT = (flip evalStateT)

fexecState = (flip execState)
fevalState = (flip evalState)