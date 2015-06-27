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
import Control.Monad.Loops
import Control.Monad.Primitive
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Data.Functor.Identity
import Foreign.C.Types
import Data.Word
import Data.Bits
import Data.Int

import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL.Raw.Timer as Raw
import qualified Config
import Util
import GfxUtil

data MonadIO m => GfxState cs m = GfxState {
  window :: SDL.Window
  ,renderer :: SDL.Renderer
  ,key_handler :: SDL.Keycode -> StateT (cs, GfxState cs m) m ()
  ,draw_handler :: Float -> StateT (cs, GfxState cs m) m ()
  ,framer :: FrameTimer
  ,glcontext :: SDL.GLContext
  }

with_field :: (MonadIO m) => (GfxState cs m -> b)
              -> (b -> GfxState cs m -> GfxState cs m)
              -> StateT b m a -> StateT (GfxState cs m) m a
with_field get_field set_field ftcomp = do
  s <- get
  (out , next_value) <- lift (runStateT ftcomp $ get_field s)
  put $ set_field next_value s
  return out

with_framer :: (MonadIO m) => StateT FrameTimer m a -> StateT (GfxState cs m) m a
with_framer = with_field framer $ \n s -> s {framer = n}
                
init :: (MonadIO m, Functor m) => cs -> m (cs, GfxState cs m)
init client_state = do
  SDL.initialize [SDL.InitVideo]

  let winConfig =
          SDL.defaultWindow {SDL.windowPosition = SDL.Absolute (P (V2 100 100))
                            ,SDL.windowSize     = Config.window_size Config.config
                            ,SDL.windowOpenGL   = Just (Config.opengl Config.config)}
                     
  let rdrConfig =
          SDL.RendererConfig {SDL.rendererSoftware      = False
                             ,SDL.rendererAccelerated   = True
                             ,SDL.rendererPresentVSync  = False
                             ,SDL.rendererTargetTexture = True}

  window <- liftIO $ SDL.createWindow (Config.window_title Config.config) winConfig
  renderer <- liftIO $ SDL.createRenderer window (-1) rdrConfig
  gl <- SDL.glCreateContext window 

  SDL.glMakeCurrent window gl
  SDL.glSetSwapInterval SDL.ImmediateUpdates

  framer <- frame_timer_new 60

  return (client_state, GfxState {window = window
                                 ,renderer = renderer
                                 ,key_handler = \ _ -> return ()
                                 ,draw_handler = \ _ -> return ()
                                 ,framer = framer
                                 ,glcontext = gl
                                 })

finish :: MonadIO m => StateT (cs, GfxState cs m) m ()
finish = do
  (client_state, gfx_state) <- get
  liftIO $ do
    SDL.destroyRenderer (renderer gfx_state)
    SDL.destroyWindow (window gfx_state)
    SDL.quit
  return ()

loop :: (Functor m, MonadIO m) => StateT (cs, GfxState cs m) m ()
loop = do
  (client_state, gfx_state) <- get

  let gfx_renderer = renderer gfx_state
  let gfx_draw_handler = draw_handler gfx_state
  let gfx_window = window gfx_state

  _ <- SDL.renderClear gfx_renderer
  _ <- SDL.renderPresent gfx_renderer

  iterateUntil id $ do
    
    gfx_state <- gets snd
    (next, new_gfx_state) <- lift $ runStateT (with_framer frame_timer_next) gfx_state
    put (client_state, new_gfx_state)

    case next of
      FrameMark dts -> do
        gfx_draw_handler dts
        SDL.glSwapWindow gfx_window
        return False

      FrameWait t -> do
        SDL.pumpEvents
        process_events_wait t


  gfx_state <- gets snd
  liftIO $ putStrLn $ "overall fps=" ++ (show (fps (framer gfx_state)))
  return ()

collect_events_timeout :: (Functor m, MonadIO m) => Float -> m [SDL.Event]
collect_events_timeout t = do
  m_ev <- SDL.waitEventTimeout (fromIntegral (truncate (1000 * t)))
  case m_ev of
    Nothing -> return []
    Just ev -> fmap (ev :) collect_events
collect_events :: (Functor m, MonadIO m) => m [SDL.Event]
collect_events = do
  m_ev <- SDL.pollEvent
  case m_ev of
    Nothing -> return []
    Just ev -> fmap (ev :) collect_events

process_events :: (Functor m, MonadIO m) => StateT (cs, GfxState cs m) m Bool
process_events = do
  events <- collect_events
  anyM process_event events

process_event :: MonadIO m => SDL.Event -> StateT (cs, GfxState cs m) m Bool
process_event ev = do
  (client_state, gfx_state) <- get
  case SDL.eventPayload ev of
    SDL.QuitEvent -> return True
    SDL.KeyboardEvent _ SDL.KeyDown _ _ (SDL.Keysym _ KeycodeEscape _) -> return True
    SDL.KeyboardEvent _ SDL.KeyDown _ _ (SDL.Keysym _ kc _) ->
      do () <- (key_handler gfx_state) kc
         return False
      --(SDL.MouseButtonEvent _ SDL.MouseButtonDown _ _ _ _ _) -> True
    _ -> return False

process_events_wait :: (MonadIO m, Functor m) => Float -> StateT (cs, GfxState cs m) m Bool
process_events_wait t = do
  events <- collect_events_timeout t
  anyM process_event events
