module GfxUtilPP where

import Prelude hiding ((.))
import Control.Category

import System.Environment
import SDL
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Functor.Identity
import Foreign.C.Types
import Data.Word
import Data.Bits
import Data.Int
import Data.Ratio

import qualified SDL.Raw.Timer as Raw

import qualified Stream
import Lens


data FrameNext =
  FrameMark (Float)
  | FrameWait (Float)

data FrameTimer = FrameTimer {target_dt :: Int64
                             ,avgwindow_dt :: Stream.AvgWindow Int64 (Ratio Int64)
                             ,last_mark :: Int64
                             ,next_mark :: Int64
                             ,min_dt :: Stream.MinMonoid Int64
                             ,fps :: Float}

make_lenses_record "frametimer" ''FrameTimer
