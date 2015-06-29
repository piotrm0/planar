module GfxUtil where

import System.Environment
import SDL
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Data.Functor.Identity
import Foreign.C.Types
import Data.Word
import Data.Bits
import Data.Int

import qualified SDL.Raw.Timer as Raw

import Util

counter_of_seconds :: (MonadIO m) => Float -> m (Int64)
counter_of_seconds s =
  if s < 0 then return 0 else do
    f <- Raw.getPerformanceFrequency
    return $ truncate (s * (fromRational (toRational f)))

seconds_of_counter :: (MonadIO m) => Int64 -> m (Float)
seconds_of_counter c = do
  f <- Raw.getPerformanceFrequency
  return $ (realToFrac c) / (fromRational (toRational f))

data FrameNext =
  FrameMark (Float)
  | FrameWait (Float)

data FrameTimer = FrameTimer {target_dt :: Int64
                             ,avgwindow_dt :: AvgWindow Int64
                             ,avgwindow_dts :: AvgWindow Float
                             ,last_mark :: Int64
                             ,next_mark :: Int64
                             ,fps :: Float}

frame_timer_with_field :: (MonadIO m)
                          => (FrameTimer -> b)
                          -> (b -> FrameTimer -> FrameTimer)
                          -> StateT b m a
                          -> StateT FrameTimer m a
frame_timer_with_field get_field set_field fcomp = do
  s <- get
  (out, next_value) <- lift (runStateT fcomp (get_field s))
  put $ set_field next_value s
  return out

frame_timer_with_avgwindow_dt :: (MonadIO m) => StateT (AvgWindow Int64) m a -> StateT FrameTimer m a
frame_timer_with_avgwindow_dt = frame_timer_with_field avgwindow_dt (\v s -> s {avgwindow_dt = v})
frame_timer_with_avgwindow_dts :: (MonadIO m) => StateT (AvgWindow Float) m a -> StateT FrameTimer m a
frame_timer_with_avgwindow_dts = frame_timer_with_field avgwindow_dts (\v s -> s {avgwindow_dts = v})

frame_timer_get_fps :: FrameTimer -> Float
frame_timer_get_fps = fps

frame_timer_new :: MonadIO m => Float -> m FrameTimer
frame_timer_new fps = do
  tdt <- counter_of_seconds (1 / fps)
  avgdt <- avgwindow_new (truncate fps)
  avgdts <- avgwindow_new (truncate fps)
  return $ FrameTimer {target_dt = tdt
                      ,avgwindow_dt = avgdt
                      ,avgwindow_dts = avgdts
                      ,last_mark = 0
                      ,next_mark = 0
                      ,fps = 0.0}

frame_timer_next :: (Functor m, MonadIO m) => StateT FrameTimer m FrameNext
frame_timer_next = do
  f <- get
  now_time <- fmap fromIntegral Raw.getPerformanceCounter

  if now_time >= (next_mark f) then do
    last_mark <- gets last_mark
    target_dt <- gets target_dt
    let dt = now_time - last_mark

    dts <- seconds_of_counter (fromIntegral dt)

    avg <- frame_timer_with_avgwindow_dt $ do
      avgwindow_add dt
      gets average
    avgs <- frame_timer_with_avgwindow_dts $ do
      avgwindow_add dts
      gets average

    let new_fps = if avgs == 0 then 0.0 else 1/avgs
    
    s <- get
    put $ s {last_mark = now_time
            ,next_mark = now_time + (target_dt + (target_dt - (truncate avg)))
            ,fps = new_fps}
    
    return $ FrameMark dts
    
    else do ret <- seconds_of_counter $ (next_mark f) - now_time
            return $ FrameWait ret
