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

import System.IO.Unsafe

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
                ,window_size  = V2 800 600
                ,font_size    = 16
                ,opengl       = SDL.defaultOpenGL {glProfile = Compatibility Normal 3 2}}

dir_bin :: String
dir_bin = unsafePerformIO $ System.Environment.FindBin.getProgPath

dir_res :: String
dir_res = dir_bin ++ "/../Resources"

default_font :: FTGL.Font
default_font = get_font (dir_res ++ "/estre.ttf") (font_size config) 72
