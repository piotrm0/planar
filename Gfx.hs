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

import qualified Config

data GfxState = GfxState {
      window :: SDL.Window
    ,renderer :: SDL.Renderer
    ,key_handler :: SDL.Keycode -> StateT GfxState IO ()}

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
              
  return (GfxState {window = window
                   ,renderer = renderer,
                    key_handler = \ _ -> return () })

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
           ,key_handler = key_handler} <- get

  let looper = do
         _ <- lift $ SDL.renderClear renderer
         _ <- lift $ SDL.renderPresent renderer

         quit <- do
           ev <- SDL.waitEvent
           case SDL.eventPayload ev of
             SDL.QuitEvent -> return True
             SDL.KeyboardEvent _ SDL.KeyDown _ _ (SDL.Keysym _ KeycodeEscape _) -> return True
             SDL.KeyboardEvent _ SDL.KeyDown _ _ (SDL.Keysym _ kc _) -> do
                                      () <- key_handler kc
                                      return False
             --(SDL.MouseButtonEvent _ SDL.MouseButtonDown _ _ _ _ _) -> True
             _ -> return False

         unless quit looper

  looper

  return ()
