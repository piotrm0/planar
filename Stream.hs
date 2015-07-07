{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Stream where

import qualified Data.Vector.Mutable as MV

import Control.Monad.Primitive
import Control.Monad.State.Strict
import Control.Applicative
import Data.Functor.Identity

import Data.Array.IO
import Data.Ratio
import Data.Monoid

-- Streams: push a stream of values into a "window" and query
-- something about the pushed values.

class Stream m w a b | w a b -> a, w a b -> b where
  push :: a -> StateT (w a b) m b
  query :: StateT (w a b) m b

---- FixedWindow : a window for quering something about the last m
---- pushed values.

class FixedStream m w a b | w a b -> a, w a b -> b where
  create_fixed :: Int -> m (w a b)

data (Num e) => SumWindow e v = SumWindow {
  current_sum :: v
  ,current_size :: Int
  ,current_p :: Int
  ,values :: IOArray Int e
  }

instance (MonadIO m, Num e) => FixedStream m SumWindow e e where
  create_fixed size = do
    v <- liftIO $ newArray (0, size) 0
    return $ SumWindow {current_sum = 0
                       ,current_size = size
                       ,current_p = 0
                       ,values = v}

instance (MonadIO m, Num e) => Stream m SumWindow e e where
  push new_val = do
    w <- get
    let SumWindow old_sum size current_p vector = w
    let next_p = mod (current_p+1) size
    old_val <- liftIO $ do
      old <- readArray vector next_p
      writeArray vector next_p new_val
      return old
    let new_sum = old_sum + new_val - old_val
    put $ w {current_sum = old_sum + new_val - old_val
            ,current_p = next_p}
    return new_sum

  query = do
    SumWindow ret _ _ _ <- get
    return ret

data (Num e) => AvgWindow e v = AvgWindow {
  average :: v
  ,sumwindow :: SumWindow e e}

instance (MonadIO m, Num e, Integral e) => FixedStream m AvgWindow e (Ratio e) where
  create_fixed size = do
    initial_sumwindow <- create_fixed size
    return $ AvgWindow {average = 0
                       ,sumwindow = initial_sumwindow}

instance (MonadIO m, Integral e, Num e) => Stream m AvgWindow e (Ratio e) where
  push new_val = do
    w@(AvgWindow avg old_window) <- get
    (_, new_window) <- (flip runStateT) old_window $ push new_val
    let c_sum = fromIntegral $ current_sum new_window
    let c_size = fromIntegral $ current_size new_window
    let new_average = c_sum / c_size
    put $ w {average = new_average
            ,sumwindow = new_window
            }
    return new_average
  query = do
    AvgWindow ret _ <- get
    return ret

-- Monoid Streams, push an element which is mappend-ed with what is
-- initially mempty.

---- Wrap two types. The first is physically wrapped and unwrappable
---- in a box while the second is phantom. Used in the monad stream below.

class Wrap2 w e p | w e p -> e, w e p -> p where
  wrap2 :: e -> w e p
  unwrap2 :: w e p -> e

class (Monad m) => MonoidStream m ew e | ew e -> e where
  create_monoid :: m (ew e)

instance (Monad m, Monoid (ew e)) => MonoidStream m ew e where
  create_monoid = return mempty

type MinMonoid e = WrappedMinMonoid e e
type MaxMonoid e = WrappedMaxMonoid e e

newtype WrappedMinMonoid e p = MinMonoid e
newtype WrappedMaxMonoid e p = MaxMonoid e

instance Wrap2 WrappedMinMonoid t t where
  wrap2 e = MinMonoid e
  unwrap2 (MinMonoid e) = e
instance Wrap2 WrappedMaxMonoid t t where
  wrap2 e = MaxMonoid e
  unwrap2 (MaxMonoid e) = e

instance (Bounded e, Ord e) => Monoid (MinMonoid e) where
  mempty = MinMonoid maxBound
  mappend (MinMonoid e1) (MinMonoid e2) = MinMonoid $ min e1 e2
instance (Bounded e, Ord e) => Monoid (MaxMonoid e) where
  mempty = MaxMonoid minBound
  mappend (MaxMonoid e1) (MaxMonoid e2) = MaxMonoid $ max e1 e2

instance (Monad m, Monoid (ew e e), Wrap2 ew e e)
      => Stream m ew e e where
  push e2 = do
    e1 <- get
    let new_e = mappend e1 (wrap2 e2)
    put new_e
    return $ unwrap2 new_e
  query = do
    e <- get
    return $ unwrap2 e

