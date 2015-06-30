{-# LANGUAGE TemplateHaskell #-}

module GfxUtil where

import System.Environment
import SDL
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Functor.Identity
import Foreign.C.Types
import Data.Word
import Data.Bits
import Data.Int

import qualified SDL.Raw.Timer as Raw

import qualified Util as U
import Lens

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
                             ,avgwindow_dt :: U.AvgWindow Int64
                             ,last_mark :: Int64
                             ,next_mark :: Int64
                             ,fps :: Float}

make_lenses_record "frametimer" ''FrameTimer

frame_timer_new :: MonadIO m => Float -> m FrameTimer
frame_timer_new fps = do
  tdt <- counter_of_seconds (1 / fps)
  avgdt <- U.create_fixed (truncate fps)
  return $ FrameTimer {target_dt = tdt
                      ,avgwindow_dt = avgdt
                      ,last_mark = 0
                      ,next_mark = 0
                      ,fps = 0.0}

frame_timer_next :: (Functor m, MonadIO m) => StateT FrameTimer m FrameNext
frame_timer_next = do
  f <- get
  now_time <- fmap fromIntegral Raw.getPerformanceCounter

  let remain_time = now_time - (next_mark f)

  remain_time_s <- seconds_of_counter remain_time

  liftIO $ do
    putStrLn $ "time remaining: " ++ (show remain_time_s)

  if now_time >= (next_mark f) then do
    last_mark <- gets last_mark
    target_dt <- gets target_dt
    let dt = now_time - last_mark

    dts <- seconds_of_counter (fromIntegral dt)

    avg <- with_lens frametimer_avgwindow_dt $ do
      U.push_fixed dt
      U.query_fixed

    avgs <- seconds_of_counter (truncate avg)
    
    let new_fps = if avgs == 0 then 0.0 else 1/avgs
    
    s <- get
    put $ s {last_mark = now_time
            ,next_mark = now_time + (target_dt + (target_dt - (truncate avg)))
            ,fps = new_fps}
    
    return $ FrameMark dts
    
    else do ret <- seconds_of_counter $ (next_mark f) - now_time
            return $ FrameWait ret
