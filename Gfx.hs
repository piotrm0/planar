{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gfx where

import System.Environment
import SDL
import SDL.Input.Keyboard
import Linear
import Linear.Affine ( Point(P) )
import Control.Monad
import Control.Monad.State.Lazy
import Data.Functor.Identity
import Foreign.C.Types
import Data.Word
import Data.Bits
import Data.Int

import qualified SDL.Raw.Timer as Raw
import qualified Config
import Util

data GfxState = GfxState {
      window :: SDL.Window
    ,renderer :: SDL.Renderer
    ,key_handler :: SDL.Keycode -> StateT GfxState IO ()
    ,draw_handler :: Float -> StateT GfxState IO ()
    ,target_dt :: Int64
    ,avgwindow_dt :: AvgWindow IO Float
    }

counter_of_seconds :: Float -> IO (Int64)
counter_of_seconds s =
    if s < 0 then return 0 else do
        f <- Raw.getPerformanceFrequency
        return $ truncate (s * (fromRational (toRational f)))

seconds_of_counter :: Int64 -> IO (Float)
seconds_of_counter c = do
  f <- Raw.getPerformanceFrequency
  return $ (realToFrac c) / (fromRational (toRational f))

init :: IO (GfxState)
init = do
  SDL.initialize [SDL.InitEverything]

  let winConfig =
          SDL.defaultWindow {SDL.windowPosition = SDL.Absolute (P (V2 100 100))
                            ,SDL.windowSize     = Config.window_size Config.config}
                     
  let rdrConfig =
          SDL.RendererConfig {SDL.rendererSoftware      = False
                             ,SDL.rendererAccelerated   = True
                             ,SDL.rendererPresentVSync  = False
                             ,SDL.rendererTargetTexture = True }
                     
  window <- SDL.createWindow (Config.window_title Config.config) winConfig
  renderer <- SDL.createRenderer window (-1) rdrConfig

  tdt <- counter_of_seconds (1 / 5)
  stime <- SDL.time
  avgdt <- avgwindow_new 20
              
  return (GfxState {window = window
                   ,renderer = renderer,
                    key_handler = \ _ -> return (),
                    draw_handler = \ _ -> return (),
                    target_dt = tdt,
                    avgwindow_dt = avgdt
                   })

finish :: StateT GfxState IO ()
finish = do
  s <- get
  lift $ do
    SDL.destroyRenderer (renderer s)
    SDL.destroyWindow (window s)
    SDL.quit
  return ()

loop :: StateT GfxState IO ()
loop = do
  GfxState {window = window
           ,renderer = renderer
           ,key_handler = key_handler
           ,draw_handler = draw_handler
           ,target_dt = target_dt
           } <- get

  let looper (last_dt :: Int64) (mod_target_dt :: Int64) (timeout :: Int64) (last_time :: Int64) = do
         _ <- lift $ SDL.renderClear renderer
         _ <- lift $ SDL.renderPresent renderer

         quit <- do
           --lift $ putStrLn $ "timing out in " ++ (show timeout)
           ftimeout <- lift $ seconds_of_counter timeout
           let atimeout = if ftimeout < 0 then 0 else ftimeout
           --lift $ putStrLn $ "timing out in " ++ (show atimeout)
           --perf <- Raw.getPerformanceCounter 
           --lift $ putStrLn $ "perf count = " ++ (show perf)
           m_ev <- SDL.waitEventTimeout
                   (fromIntegral (truncate (1000 * atimeout)))
           case m_ev of
             Nothing -> return False
             Just ev ->
                     case SDL.eventPayload ev of
                       SDL.QuitEvent -> return True
                       SDL.KeyboardEvent _ SDL.KeyDown _ _ (SDL.Keysym _ KeycodeEscape _) -> return True
                       SDL.KeyboardEvent _ SDL.KeyDown _ _ (SDL.Keysym _ kc _) -> do
                                                () <- key_handler kc
                                                return False
                       --(SDL.MouseButtonEvent _ SDL.MouseButtonDown _ _ _ _ _) -> True
                       _ -> return False

         unless quit $ do
           new_time <- fromIntegral <$> Raw.getPerformanceCounter
           let dt = (new_time - last_time)
           if dt >= mod_target_dt then
               do
                 ctime <- SDL.time
                 s <- get

                 dts <- lift $ seconds_of_counter (fromIntegral dt)
                 new_avgwindow_dt@(AvgWindow avg _) <- lift $ avgwindow_add (avgwindow_dt s) dts
                 put s {avgwindow_dt = new_avgwindow_dt}
                 lift $ putStrLn $ "fps=" ++ (show (fromRational (1 / avg)::Float))

                 () <- draw_handler dts
                 looper
                       dt
                       (mod_target_dt + (target_dt - dt))
                       (mod_target_dt + (target_dt - dt))
                       new_time
           else do looper
                       last_dt
                       mod_target_dt
                       (mod_target_dt - dt)
                       last_time

  start_time <- fromIntegral <$> Raw.getPerformanceCounter
  looper target_dt target_dt target_dt start_time

  return ()
