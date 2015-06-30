{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}

module Util where

import qualified Data.Vector.Mutable as MV

import Control.Monad.Primitive
import Control.Monad.State.Strict

import Data.Array.IO
import Data.Ratio
import Data.Monoid

class (Monad m)
      => FixedWindow container m ele ret | container -> ele, container -> ret where
  create_fixed :: Int -> m container
  push_fixed :: ele -> StateT container m ()
  query_fixed :: StateT container m ret

--instance Num e => Monoid e where
--  mempty = 0
--  mappend = (+)
--  mconcat = foldl (+) 0

data (Num ele) => SumWindow ele = SumWindow {
  current_sum :: ele
  ,current_size :: Int
  ,current_p :: Int
  ,values :: IOArray Int ele
  }

data (Num ele) => AvgWindow ele = AvgWindow {
  average :: Ratio ele
  ,sumwindow :: SumWindow ele}
  
instance (MonadIO monad
         ,Monad monad
         ,Num ele) => FixedWindow (SumWindow ele) monad ele ele where
  create_fixed size = do
    v <- liftIO $ newArray (0, size) 0
    return $ SumWindow {current_sum = 0
                       ,current_size = size
                       ,current_p = 0
                       ,values = v}

  push_fixed new_val = do
    w <- get
    let SumWindow old_sum size current_p vector = w
    let next_p = mod (current_p+1) size
    old_val <- liftIO $ do
      old <- readArray vector next_p
      writeArray vector next_p new_val
      return old
    put $ w {current_sum = old_sum + new_val - old_val
            ,current_p = next_p}

  query_fixed = do
    SumWindow ret _ _ _ <- get
    return ret

instance (MonadIO monad
         ,Num ele, Integral ele)
         => FixedWindow (AvgWindow ele) monad ele (Ratio ele) where  
  create_fixed size = do
    initial_sumwindow <- create_fixed size
    return $ AvgWindow {average = 0
                       ,sumwindow = initial_sumwindow}
  push_fixed new_val = do
    w@(AvgWindow avg old_window) <- get
    (_, new_window) <- (flip runStateT) old_window $ push_fixed new_val
    let c_sum = fromIntegral $ current_sum new_window
    let c_size = fromIntegral $ current_size new_window
    put $ w {average = c_sum / c_size
            ,sumwindow = new_window
            }
  query_fixed = do
    AvgWindow ret _ <- get
    return ret

instance (Monad m, Monoid e) => MonoidWindow e m e e where
  create_monoid = do
    return mempty
  push_monoid ele = do
    old <- get
    put $ mappend old ele
  query_monoid = get

class (Monad m, Monoid ele)
      => MonoidWindow container m ele ret | container -> ele, container -> ret where
  create_monoid :: m container
  push_monoid :: ele -> StateT container m ()
  query_monoid :: StateT container m ret

--instance (Bounded e, Ord e) => Monoid e where
--  mempty = minBound
--  mappend = max
--instance (Bounded e, Ord e) => Monoid e where
--  mempty = maxBound
--  mappend = min

class (Monad m, Monoid ele)
      => MaxWindow container m ele ret | container -> ele, container -> ret where
  create_max :: m container
  push_max :: ele -> StateT container m ()
  query_max :: StateT container m ele
class (Monad m, Monoid ele)
      => MinWindow container m ele ret | container -> ele, container -> ret where
  create_min :: m container
  push_min :: ele -> StateT container m ()
  query_min :: StateT container m ret

instance (Functor m, Monad m, Monoid e, Ord e, Bounded e) => MaxWindow e m e e where
  create_max = return minBound
  push_max e2 = (fmap (max e2) $ get) >>= put
  query_max = get
instance (Functor m, Monad m, Monoid e, Ord e, Bounded e) => MinWindow e m e e where
  create_min = return maxBound
  push_min e2 = (fmap (min e2) $ get) >>= put
  query_min = get

forM_flat :: Monad m => [a] -> (a -> m [b]) -> m [b]
forM_flat items f = do
  foldM (\accum item -> do
             more_items <- f item
             return $ accum ++ more_items) [] items
