import PlanarPP

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

import Data.Set

import System.IO
import System.Environment
import System.Console.Haskeline

import qualified LLVM.General.AST as AST
import LLVM.General.PrettyPrint

import Util
import Config

import qualified Text.PrettyPrint as Pretty

import qualified Graphics.Rendering.OpenGL as GL
import qualified Gfx as G
import qualified GfxUtil as GU
import qualified SDL.Raw.Timer as Raw
import Lens
import qualified Stream
import Util
import GLUtil

import Parser
import Lang
import Emit
import Codegen
import JIT

import qualified GLLog as Log
--import qualified GLText as Text
--import qualified GLPanel as Panel

import GLWidget as GLW

import Data.StateVar(($=))

main :: IO ()
main = do
  p <- execT create $ do
    position != V2 0 0

  let cs = ClientState {rot = 0.0
                       ,panel = p
                       }
  s <- G.init cs
  (flip evalStateT) s $ do
    with_lens G.gfx_in_allstate $ do
      gfx_state <- get
      put $ gfx_state {G.key_handler = handle_key
                      ,G.draw_handler = handle_draw
                      ,G.drop_handler = handle_drop}
    G.loop
    G.finish
    return ()

--add_panel :: (MonadIO m) => GLW.Text -> AllStateT m ()
--add_panel p =
--  with_lens (panel_in_client . G.client_in_allstate) $ do
--    old_panels <- get
--    put $ insert p old_panels

handle_drop :: MonadIO m => String -> AllStateT m ()
handle_drop s = do
  font <- Config.default_font

  V2 _ height' <- gets (G.window_size . snd)
  let height = fromIntegral height'

  (content, maybe_exprs) <- liftIO $ do
    putStrLn $ "drop event with s=" ++ (show s)
    r@(content, maybe_exprs) <- read_file s
    putStrLn $ content
    return r

  with_lens (panel_in_client . G.client_in_allstate) $ do
    GLW.padding != 5
    GLW.arrange_in_panel != GLW.Horizontal
    
    raw_text <- execT create $ do
      GLW.padding != 5
      GLW.set_content content

    GLW.add_element (GLW.GLText raw_text)
 
    case maybe_exprs of
      Nothing -> return ()
      Just exprs -> do
        panel_parsed <- execT create $ do
          GLW.arrange_in_panel != GLW.Vertical
          forM_ exprs (\ e -> do
                           let pretty_text = Pretty.render (pretty e)
                           new_panel <- execT GLW.create $ do
                             GLW.padding != 5
                             GLW.set_content pretty_text
                           GLW.add_element $ (GLW.GLText new_panel))

        add_element (GLW.GLPanel panel_parsed)

        (llvm_raw, llvm_opt) <- liftIO $ do
          llvm <- llvm_of_exprs exprs
          JIT.getAssembly llvm

        panel_llvm_raw <- execT GLW.create $ do
          GLW.padding != 5
          GLW.set_content llvm_raw

        panel_llvm_opt <- execT GLW.create $ do
          GLW.padding != 5
          GLW.set_content llvm_opt

        add_element (GLW.GLText panel_llvm_raw)
        add_element (GLW.GLText panel_llvm_opt)

    refit

--    liftIO $ putStrLn "\n\n\n\n\n"
--    refit

handle_key :: MonadIO m => Keycode -> AllStateT m ()
handle_key kc =
  liftIO $ putStrLn (show kc) >>= return

handle_draw :: (MonadIO m, Functor m) => Float -> AllStateT m ()
handle_draw dt = do
  mindt <- with_lens (GU.min_dt_in_frametimer . G.frame_timer_in_gfx . G.gfx_in_allstate) $ Stream.query
  fps <- get_lens (GU.fps_in_frametimer . G.frame_timer_in_gfx . G.gfx_in_allstate)

  p <- gets $ panel . fst

  G.viewport_default_2d

  liftIO $ do
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity

  runStateT GLW.render p

  G.draw_text_default (show fps)

  return ()

initModule :: AST.Module
initModule = emptyModule "llvm tutorial jit"

process_llvm :: AST.Module -> [Expr] -> IO (AST.Module)
process_llvm modo es = do
  ast <- codegen modo es
  return $ ast

llvm_of_exprs :: [Expr] -> IO (AST.Module)
llvm_of_exprs e = process_llvm initModule e

read_file :: String -> IO (String, Maybe [Expr])
read_file fname = do
  content <- readFile fname
  let me = parseToplevel content
  case me of
    Left pe -> return (content, Nothing)
    Right e -> return (content, Just e)
