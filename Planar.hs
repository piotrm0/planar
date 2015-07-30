module Main where

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

import qualified Control.Monad.Identity as I

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

import GLWidgetPP
import GLWidget

import Data.StateVar(($=))

main :: IO ()
main =
  widget_create >>= ( execStateT $ do
                          panel_base_in_panel . position_in_base != V2 0 0 )
  >>= \ p -> ( G.init (ClientData {rot = 0.0
                                  ,panel = p}) )
             >>= ( evalStateT $ do
                       withLensT G.gfx_in_allstate $ do
                         G.key_handler_in_gfx  != planar_handle_key
                         G.draw_handler_in_gfx != handle_draw
                         G.drop_handler_in_gfx != handle_drop
                       G.loop
                       G.finish
                       return () )

handle_drop :: (MonadIO m, Functor m) => String -> AllStateT m ()
handle_drop s = let font = Config.default_font in
  (liftIO $ read_file s)
  >>= \ (content, maybe_exprs) ->
  withLensT (G.client_in_allstate . panel_in_client)
  ( panel_wrap_focus_in_panel != True
    >> panel_arrange_in_panel    != Horizontal
    >> panel_base_in_panel . focused_in_base != True
    >> panel_base_in_panel . padding_in_base != 5

    >> ( widget_create >>= execStateT ( text_base_in_text . padding_in_base != 5
                                        >> set_content content ) )
    >>= ( \ e -> add_element (GLWidget e) )

    >> ( widget_create >>= execStateT ( text_base_in_text . padding_in_base != 5
                                        >> set_content content ) )
    >>= ( \ e -> add_element (GLWidget e ) )
  
    >> case maybe_exprs of
    Nothing -> return ()
    Just exprs ->
      ( widget_create >>= execStateT
        ( panel_arrange_in_panel != Vertical
          >> forM_ exprs (\ e ->let pretty_text = Pretty.render (pretty e) in
                            ( widget_create >>= execStateT ( text_base_in_text . padding_in_base != 5
                                                             >> set_content pretty_text ) )
                            >>= \ e -> add_element (GLWidget e) ) )
        >>= ( \e -> add_element (GLWidget e) ) )

      >> liftIO ( llvm_of_exprs exprs >>= JIT.getAssembly )
      >>= \ (llvm_raw, llvm_opt) ->
      widget_create >>= ( execStateT $
                          text_base_in_text . padding_in_base != 5 
                          >> set_content llvm_raw )
      >>= ( \ e -> add_element (GLWidget e) )
      >> widget_create >>= ( execStateT $
                             text_base_in_text . padding_in_base != 5
                             >> set_content llvm_opt )
      >>= ( \ e -> add_element (GLWidget e) )

    >> widget_refit )

planar_handle_key :: Monad m => Keysym -> AllStateT m ()
planar_handle_key kc =
--  let wlens = G.client_in_allstate . panel_in_client in
--  withLensT (panel_in_client . G.client_in_allstate) $ do
  case kc of
  Keysym {keysymKeycode = kk} ->
    case kk of KeycodeTab -> withLensT (G.client_in_allstate . panel_in_client) $ widget_next_focus >> return ()
               kc -> widget_handle_key (Left kc) (G.client_in_allstate . panel_in_client)
  Keysym {keysymScancode = sc} ->
    widget_handle_key (Right sc) (G.client_in_allstate . panel_in_client)

--      _ -> do
--        liftIO $ putStrLn ("planar_handle_key: unhandled key event: " ++ (show kc))
--        return ()

handle_draw :: (MonadIO m, Functor m) => Float -> AllStateT m ()
handle_draw dt = do
  mindt <- withLensT (G.gfx_in_allstate . G.frame_timer_in_gfx . GU.min_dt_in_frametimer) $ Stream.query
  fps   <- getLensT  (G.gfx_in_allstate . G.frame_timer_in_gfx . GU.fps_in_frametimer)

  p <- gets $ panel . fst

  G.viewport_default_2d

  liftIO $ do
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity

  runStateT widget_render p

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
