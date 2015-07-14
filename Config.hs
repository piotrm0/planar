{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Config where

import System.Environment
import SDL
import SDL.Input.Keyboard
import Linear
import Linear.Affine ( Point(P) )
import Control.Monad
import Control.Monad.State.Lazy
import Data.Functor.Identity
import Foreign.C.Types
import Data.Text

import qualified System.Environment.FindBin
import qualified Graphics.Rendering.FTGL as FTGL

import GLUtil

data Config = Config {
      window_title :: Text
    , window_size :: V2 CInt
    , opengl :: SDL.OpenGLConfig
    , font_size :: Int
    }

config :: Config
config = Config {window_title = "planar"
                ,window_size = V2 800 600
                ,font_size = 16
                ,opengl = SDL.defaultOpenGL {glProfile = Compatibility Normal 3 2}}

dir_bin :: MonadIO m => m String
dir_bin = liftIO System.Environment.FindBin.getProgPath

dir_res :: MonadIO m => m String
dir_res = do
  bin <- dir_bin
  return $ bin ++ "/../Resources"

default_font :: MonadIO m => m FTGL.Font
default_font = do
  res <- dir_res
  create_font (res ++ "/estre.ttf") (font_size config) 72
