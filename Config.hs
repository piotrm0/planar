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

data Config = Config {
      window_title :: Text
    , window_size :: V2 CInt 
    }

config :: Config
config = Config {window_title = "planar"
                ,window_size = V2 800 600}
