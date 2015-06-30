{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

import System.Environment
import SDL
import SDL.Input.Keyboard
import Linear
import Linear.Affine ( Point(P) )
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Word
import Util

import qualified Graphics.Rendering.OpenGL as GL
import qualified Gfx as G
import qualified GfxUtil as GU
import qualified SDL.Raw.Timer as Raw
import Lens

data ClientState = ClientState {
  rot :: Float
}

main :: IO ()
main =
  let cs = ClientState {rot = 0.0} in
  do
  s <- G.init cs
  (flip evalStateT) s $ do
    with_lens G.allstate_gfx $ do
      gfx_state <- get
      put $ gfx_state {G.key_handler = handle_key
                       ,G.draw_handler = handle_draw}
    G.loop
    G.finish
    return ()

handle_key :: MonadIO m => Keycode -> StateT (cs, G.GfxState cs m) m ()
handle_key kc =
  liftIO $ putStrLn (show kc) >>= return

handle_draw :: MonadIO m => Float -> StateT (cs, G.GfxState cs m) m ()
handle_draw dt = do
  (client_state, gfx_state) <- get
  liftIO $ putStrLn $ "fps=" ++ (show (GU.fps (G.framer gfx_state)))
  liftIO $ GL.clear [GL.ColorBuffer]

--  lift $ avgwindow_print (G.avgwindow_dt state)
  return ()
