{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}

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
import Control.Monad.State.Strict
import Data.Functor.Identity
import Foreign.C.Types
import Data.Word
import Data.Bits
import Data.Int

import Data.StateVar(($=))

import Prelude hiding ((.))
import Control.Category

import Graphics.Rendering.FTGL

import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL.Raw.Timer as Raw
import qualified Config
import Lens
import Util
import GLUtil
import GfxUtil

type AllState cs m = (cs, GfxState cs m)
type AllStateT cs m r = StateT (AllState cs m) m r

data MonadIO m => GfxState cs m = GfxState {
  window :: SDL.Window
  ,renderer :: SDL.Renderer
  ,key_handler :: SDL.Keycode -> StateT (AllState cs m) m ()
  ,draw_handler :: Float -> StateT (AllState cs m) m ()
  ,frame_timer :: FrameTimer
  ,glcontext :: SDL.GLContext
  ,font :: Font
  }

make_lenses_tuple "allstate" ("client", "gfx")
make_lenses_record "gfx" ''Gfx.GfxState

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

  font <- liftIO $ createTextureFont "estre.ttf"
  liftIO $ setFontFaceSize font 24 72

  return (client_state, GfxState {window = window
                                 ,renderer = renderer
                                 ,key_handler = \ _ -> return ()
                                 ,draw_handler = \ _ -> return ()
                                 ,frame_timer = framer
                                 ,glcontext = gl
                                 ,font = font
                                 })

finish :: MonadIO m => StateT (AllState cs m) m ()
finish = do
  (client_state, gfx_state) <- get
  liftIO $ do
    SDL.destroyRenderer (renderer gfx_state)
    SDL.destroyWindow (window gfx_state)
    SDL.quit
  return ()

loop :: (Functor m, MonadIO m) => StateT (AllState cs m) m ()
loop = do
  (client_state, gfx_state) <- get

  let gfx_renderer = renderer gfx_state
  let gfx_draw_handler = draw_handler gfx_state
  let gfx_window = window gfx_state

  _ <- SDL.renderClear gfx_renderer
  _ <- SDL.renderPresent gfx_renderer

  iterateUntil Prelude.id $ do
    gfx_state <- gets snd
    waittime <- with_lens (frame_timer_in_gfx . gfx_in_allstate) $ frame_timer_wait

    SDL.delay (fromIntegral waittime)
    dt <- with_lens (frame_timer_in_gfx . gfx_in_allstate) $ frame_timer_mark
    dts <- seconds_of_counter (fromIntegral dt)

    liftIO $ GL.clear [GL.ColorBuffer]

    window_dims <- get_window_size
    viewport_center window_dims

    gfx_draw_handler dts

    SDL.glSwapWindow gfx_window

    process_events

  gfx_state <- gets snd
  liftIO $ putStrLn $ "overall fps=" ++ (show (fps (frame_timer gfx_state)))
  return ()

draw_text :: (MonadIO m) => String -> AllStateT cs m ()
draw_text s =
  with_lens gfx_in_allstate $ do
  font <- get_lens font_in_gfx
  liftIO $ do
    renderFont font s All

collect_events_timeout :: (Functor m, MonadIO m) => Float -> m [SDL.Event]
collect_events_timeout t = do
  m_ev <- SDL.waitEventTimeout $ truncate (1000 * t)
  case m_ev of
    Nothing -> return []
    Just ev -> fmap (ev :) collect_events
collect_events :: (Functor m, MonadIO m) => m [SDL.Event]
collect_events = do
  m_ev <- SDL.pollEvent
  case m_ev of
    Nothing -> return []
    Just ev -> fmap (ev :) collect_events

process_events :: (Functor m, MonadIO m) => StateT (AllState cs m) m Bool
process_events = do
  events <- collect_events
  anyM process_event events

process_event :: MonadIO m => SDL.Event -> StateT (AllState cs m) m Bool
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

process_events_wait :: (MonadIO m, Functor m) => Float -> StateT (AllState cs m) m Bool
process_events_wait t = do
  events <- collect_events_timeout t
  anyM process_event events

get_window_size :: MonadIO m => StateT (AllState cs m) m (V2 CInt)
get_window_size =
  with_lens (window_in_gfx . gfx_in_allstate) $ do
    w <- get
    ws <- liftIO $ SDL.getWindowSize w
    return ws

setup_viewport :: MonadIO m => StateT (AllState cs m) m ()
setup_viewport = do
  V2 width height <- get_window_size
  with_lens gfx_in_allstate $ do
    return ()
