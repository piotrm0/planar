module Util where

import qualified Data.Vector.Mutable as MV
import Data.STRef
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.State.Lazy
import Data.Array.IO
--import Data.Array.MArray

data SumWindow a = SumWindow {
  current_sum :: a
  ,size :: Int
  ,current_p :: Int
  ,values :: IOArray Int a
  }

sumwindow_add :: (Num a, MonadIO m) =>
                 a -> StateT (SumWindow a) m ()
sumwindow_add new_val = do
  w@(SumWindow old_sum size current_p vector) <- get
  let next_p = mod (current_p+1) size 
  old_val <- liftIO $ readArray vector next_p
  liftIO $ writeArray vector next_p new_val
  put $ w {current_sum = old_sum + new_val - old_val
          ,current_p = next_p}
sumwindow_new :: (Num a, MonadIO m) => Int -> m (SumWindow a)
sumwindow_new size = do
  v <- liftIO $ newArray (0, size) 0
  return $ SumWindow 0 size 0 v

sumwindow_print :: (Show a, Num a, MonadIO m) => SumWindow a -> m ()
sumwindow_print w@(SumWindow sum size current_p vector) = do
  liftIO $ putStrLn $ "sumwindow: "
    ++ "size = " ++ (show size)
    ++ ", sum  = " ++ (show sum)
           
data AvgWindow a = AvgWindow {
  average :: Float
  ,sumwindow :: (SumWindow a)}

avgwindow_add :: (Real a, Num a, MonadIO m)
                 => a -> StateT (AvgWindow a) m ()
avgwindow_add new_val = do
  w@(AvgWindow avg old_window) <- get
  (_, new_window) <- (flip runStateT) old_window $ sumwindow_add new_val
  put $ w {average = fromRational ((toRational (current_sum new_window)) / (toRational (size new_window))) :: Float,
           sumwindow = new_window
          }

avgwindow_new :: (Real a, Num a, MonadIO m)
                 => Int -> m (AvgWindow a)
avgwindow_new size = do
  w <- sumwindow_new size
  return $ AvgWindow 1.0 w

avgwindow_get :: (Real a, Num a)
                 => AvgWindow a -> Float
avgwindow_get (AvgWindow avg _) = avg

avgwindow_print :: (Show a, Num a, MonadIO m) => AvgWindow a -> m ()
avgwindow_print w@(AvgWindow avg window) = do
  liftIO $ putStrLn $ "avgwindow: " ++ "avg = " ++ (show avg)
  sumwindow_print window

forM_flat :: Monad m => [a] -> (a -> m [b]) -> m [b]
forM_flat items f = do
  foldM (\accum item -> do
             more_items <- f item
             return $ accum ++ more_items) [] items
  