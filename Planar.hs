{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

import System.Environment
import SDL
import SDL.Input.Keyboard
import Linear
import Linear.Affine ( Point(P) )
import Control.Monad
import Control.Monad.State.Lazy
import Data.Word

import qualified Gfx as G
import qualified SDL.Raw.Timer as Raw

main :: IO ()
main = do
  s <- G.init
  f <- Raw.getPerformanceFrequency
  putStrLn $ "raw freq = " ++ (show f)
  (flip evalStateT) s $ do
         put s {G.key_handler = handle_key}
         put s {G.draw_handler = handle_draw}
         G.loop
         G.finish

  return ()

handle_key :: Keycode -> StateT G.GfxState IO ()
handle_key kc = lift $ putStrLn (show kc) >>= return

handle_draw :: Float -> StateT G.GfxState IO ()
handle_draw dt = do
  lift $ putStrLn (show dt)
  return ()
    