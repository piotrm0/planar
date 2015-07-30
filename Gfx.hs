{-# LANGUAGE ScopedTypeVariables #-}

module Gfx (module GfxPP,Gfx.init,finish,loop,draw_text_default,add_log,viewport_default_2d) where

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
import Foreign.C.String
import Data.Word
import Data.Bits
import Data.Int

import qualified System.Directory

import Data.StateVar(($=))

import Prelude hiding ((.), log)
import Control.Category

import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL.Raw as SDLR

import qualified Config
import Lens
import Util
import GLUtil
import GfxUtil
import GLLog

import GfxPP

init :: (Monad m, MonadIO m, Functor m) => cs -> m (AllData cs m)
init client_state = do
--  let empty_handler = ( \ _ -> (return :: a -> m a) ())
    
  SDL.initialize [SDL.InitVideo]
  
  old_state <- SDLR.eventState SDLR.SDL_DROPFILE (-1)
  _ <- SDLR.eventState SDLR.SDL_DROPFILE 1
  new_state <- SDLR.eventState SDLR.SDL_DROPFILE (-1)

  liftIO $ do
    putStrLn $ "changed event state from " ++ (show old_state) ++ " to " ++ (show new_state)

  let wsize = Config.window_size Config.config

  let winConfig =
          SDL.defaultWindow {SDL.windowPosition = SDL.Absolute (P (V2 100 100))
                            ,SDL.windowSize     = wsize
                            ,SDL.windowOpenGL   = Just (Config.opengl Config.config)
                            ,SDL.windowResizable = True}
                     
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

  log <- create 1000

  return (client_state, GfxData {window = window
                                ,renderer = renderer
  --                              ,key_handler  = KeyHandler  ( \ _ -> return () )
    --                            ,draw_handler = DrawHandler ( \ _ -> return () )
      --                          ,drop_handler = DropHandler ( \ _ -> return () )
                                ,frame_timer = framer
                                ,glcontext = gl
                                ,log = log
                                ,window_size = wsize
                                ,bg_rot = 0.0
                                })

finish :: MonadIO m => AllStateT cs m ()
finish = do
  (client_state, gfx_state) <- get
  liftIO $ do
    SDL.destroyRenderer (renderer gfx_state)
    SDL.destroyWindow (window gfx_state)
    SDL.quit
  return ()

loop :: (Functor m, MonadIO m) => AllStateT cs m ()
loop = do
  gfx_renderer <- gets $ renderer . snd
  
  _ <- SDL.renderClear gfx_renderer
  _ <- SDL.renderPresent gfx_renderer

  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

  GL.clearColor $= GL.Color4 0.5 0.5 0.5 (1.0 :: GL.GLfloat)

  iterateUntil Prelude.id $ do
    dts <- withLensT gfx_in_allstate $ do
      
      dts <- withLensT frame_timer_in_gfx $ do
        waittime <- frame_timer_wait
        SDL.delay $ fromIntegral waittime
        dt <- frame_timer_mark
        dts <- seconds_of_counter $ fromIntegral dt
        return dts

      liftIO $
        GL.clear [GL.ColorBuffer,GL.DepthBuffer]

      window_dims@(V2 width height) <- gets window_size

      draw_bg

      viewport_2d window_dims

      liftIO $ do
        GL.matrixMode $= GL.Modelview 0
        GL.loadIdentity
--        GL.translate $ GL.Vector3 0 (fromIntegral height) ((-0.5) :: GL.GLfloat)

      withLensT log_in_gfx $ GLLog.render window_dims

      viewport_center window_dims

      return dts

    liftIO $ do
      GL.matrixMode $= GL.Modelview 0
      GL.loadIdentity    

    gfx_draw_handler <- gets (draw_handler . snd)
    gfx_draw_handler dts

    gfx_window <- gets (window . snd)
    SDL.glSwapWindow gfx_window

    process_events

  gfx_state <- gets snd
  liftIO $ putStrLn $ "overall fps=" ++ (show (fps (frame_timer gfx_state)))
  return ()

draw_bg :: (MonadIO m, Functor m) => GfxStateT cs m ()
draw_bg = do
  rot  <- gets bg_rot
  size <- gets window_size
  bg_rot_in_gfx != rot + 0.037

  viewport_center size

--  GL.matrixMode $= GL.Projection
--  GL.loadIdentity

  liftIO $ do
    GL.color $ GL.Color4 1 1 1 (0.25 :: GL.GLfloat)

    GL.rotate 30 $ GL.Vector3 1 0 (0 :: GL.GLfloat)
    GL.translate $ GL.Vector3 0 (-200) (-200 :: GL.GLfloat)
    GL.rotate rot $ GL.Vector3 0 1 (0 :: GL.GLfloat)

    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity

    GL.renderPrimitive GL.Lines $ do
      forM_ [-10..10] $ \ x -> do
        vertex_float3 (x*10, 0, -100)
        vertex_float3 (x*10, 0, 100)
        vertex_float3 (-100, 0, x*10)
        vertex_float3 (100, 0, x*10)

window_resize :: (MonadIO m, Functor m, Integral e) => V2 e -> AllStateT cs m ()
window_resize (new_size@(V2 new_width new_height)) = do
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral new_width) (fromIntegral new_height))
  withLensT gfx_in_allstate $ do
    window_size_in_gfx != V2 (fromIntegral new_width) (fromIntegral new_height)
    return ()
  
draw_text_default :: (MonadIO m, Functor m) => String -> AllStateT cs m ()
draw_text_default s =
  let font = Config.default_font in do
  draw_text font [s] Nothing

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

process_events :: (Functor m, MonadIO m) => AllStateT cs m Bool
process_events = do
  events <- collect_events
  anyM process_event events

process_event :: (Functor m, MonadIO m) => SDL.Event -> AllStateT cs m Bool
process_event ev = do
  (client_state, gfx_state) <- get
  case SDL.eventPayload ev of
    SDL.QuitEvent -> return True
    SDL.DropEvent cs_string -> do
      s <- liftIO $ peekCAString cs_string
      add_log $ Item Message $ "drop event: " ++ (show s)
      drop_handler gfx_state s
      return False
    SDL.KeyboardEvent _ SDL.KeyDown _ _ (SDL.Keysym _ KeycodeEscape _) ->
      return True
    SDL.KeyboardEvent _ SDL.KeyDown _ _ thing ->
      do GfxPP.key_handler gfx_state thing
         return False
--         add_log $ Item Message $ "key press event: " ++ (show kc)
--         return False
--    SDL.KeyboardEvent _ SDL.KeyDown _ _ (SDL.Keysym _ kc _) ->
--      do GfxPP.key_handler gfx_state kc
--         add_log $ Item Message $ "key press event: " ++ (show kc)
--         return False
--
--    SDL.KeyboardEvent _ SDL.KeyDown _ _ (SDL.Keysym sc _ _) ->
--      do GfxPP.key_handler gfx_state sc
--         add_log $ Item Message $ "key press event: " ++ (show sc)
--         return False

    e@(SDL.WindowResized _ ns) -> do
      window_resize ns
      add_log $ Item Message $ "window resize: " ++ (show e)
      liftIO $ putStrLn $ "window resize: " ++ (show e)
      return False
    SDL.MouseMotionEvent _ _ _ _ _ -> return False
    SDL.MouseButtonEvent _ SDL.MouseButtonDown _ _ _ _ _ -> return False
    SDL.TouchFingerEvent _ _ _ _ _ -> return False
    SDL.MultiGestureEvent _ _ _ _ _ -> return False
    e -> do
      add_log $ Item Message $ "unhandled event: " ++ (show e)
      liftIO $ putStrLn $ "unhandled event: " ++ (show e)
      return False

process_events_wait :: (MonadIO m, Functor m) => Float -> AllStateT cs m Bool
process_events_wait t = do
  events <- collect_events_timeout t
  anyM process_event events

--get_window_size :: MonadIO m => AllStateT cs m (V2 CInt)
--get_window_size =
--  withLens (window_in_gfx . gfx_in_allstate) $ do
--    w <- get
--    ws <- liftIO $ SDL.getWindowSize w
--    return ws

setup_viewport :: MonadIO m => AllStateT cs m ()
setup_viewport = do
  V2 width height <- gets $ window_size . snd
  withLensT gfx_in_allstate $ do
    return ()

add_log :: (MonadIO m) => Item -> AllStateT cs m ()
add_log i =
  withLensT (gfx_in_allstate . log_in_gfx) $ add i

viewport_default_2d :: (MonadIO m) => AllStateT cs m ()
viewport_default_2d = do
  s <- gets $ window_size . snd
  viewport_2d s