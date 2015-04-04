{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

import System.Environment
import SDL
import SDL.Input.Keyboard
import Linear
import Linear.Affine ( Point(P) )
import Control.Monad
import Control.Monad.State.Lazy

import qualified Gfx as G

main :: IO ()
main = do
  s <- G.init
  (flip evalStateT) s $ do
         put s {G.key_handler = handle_key}
         G.loop
         G.finish

  return ()

handle_key :: Keycode -> StateT G.GfxState IO ()
handle_key kc = lift $ putStrLn (show kc) >>= return
