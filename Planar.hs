{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment
import SDL
import SDL.Input.Keyboard
import Linear
import Linear.Affine ( Point(P) )
import Control.Monad
import Prelude hiding ((.))
import Control.Category((.))
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Word
import Data.Int

import Util

import qualified Graphics.Rendering.OpenGL as GL
import qualified Gfx as G
import qualified GfxUtil as GU
import qualified SDL.Raw.Timer as Raw
import Lens
import qualified Stream
import Util
import GLUtil

import Data.StateVar(($=))

data ClientState = ClientState {
  rot :: GL.GLfloat
}

make_lenses_record "client" ''ClientState

main :: IO ()
main =
  let cs = ClientState {rot = 0.0} in
  do
  s <- G.init cs
  (flip evalStateT) s $ do
    with_lens G.gfx_in_allstate $ do
      gfx_state <- get
      put $ gfx_state {G.key_handler = handle_key
                       ,G.draw_handler = handle_draw}
    G.loop
    G.finish
    return ()

handle_key :: MonadIO m => Keycode -> StateT (ClientState, G.GfxState cs m) m ()
handle_key kc =
  liftIO $ putStrLn (show kc) >>= return

handle_draw :: (MonadIO m, Functor m) => Float -> StateT (ClientState, G.GfxState cs m) m ()
handle_draw dt = do
  mindt <- with_lens (GU.min_dt_in_frametimer . G.frame_timer_in_gfx . G.gfx_in_allstate) $ Stream.query
  fps <- get_lens (GU.fps_in_frametimer . G.frame_timer_in_gfx . G.gfx_in_allstate)

  with_lens G.client_in_allstate $ do
    rot <- rot_in_client !~ (+ 0.1)
--    rot <- with_lens cs_rot $ do
--      temp <- get
--      put $ temp + 0.1
--      return temp

    liftIO $ do
      putStrLn $ "fps=" ++ (show fps)
      --    putStrLn $ "min_dt=" ++ (show mindt)
      GL.clear [GL.ColorBuffer]

      GL.matrixMode $= GL.Modelview 0
      GL.loadIdentity

      GL.rotate rot $ GL.Vector3 0 0 1
      GL.translate $ ((GL.Vector3 (-0.5) (-0.5) 0.0) :: GL.Vector3 GL.GLfloat)

      GL.renderPrimitive GL.Quads $ do
        vertex_float3 (0,0,0)
        vertex_float3 (1,0,0)
        vertex_float3 (1,1,0)
        vertex_float3 (0,1,0)

--  lift $ avgwindow_print (G.avgwindow_dt state)
  return ()
