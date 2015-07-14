module GfxPP where

import System.Environment
import SDL
import SDL.Input.Keyboard
import Linear
import Linear.Affine ( Point(P) )
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Primitive
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Functor.Identity
import Foreign.C.Types
import Foreign.C.String
import Data.Word
import Data.Bits
import Data.Int
import qualified System.Environment.FindBin
import qualified System.Directory

import Data.StateVar(($=))

import Prelude hiding ((.), log)
import Control.Category

import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL.Raw as SDLR

import qualified Config
import Lens
import Util
import GLUtil
import GfxUtil
import GLLog

import Lens

type GfxStateT cs m r = StateT (GfxState cs m) m r
type AllState cs m = (cs, GfxState cs m)
type AllStateT cs m r = StateT (AllState cs m) m r

data MonadIO m => GfxState cs m = GfxState {
  window :: SDL.Window
  ,renderer :: SDL.Renderer
  ,key_handler :: SDL.Keycode -> AllStateT cs m ()
  ,draw_handler :: Float -> AllStateT cs m ()
  ,drop_handler :: String -> AllStateT cs m ()
  ,frame_timer :: FrameTimer
  ,glcontext :: SDL.GLContext
  ,log :: Log
  ,window_size :: V2 CInt
  ,bg_rot :: GL.GLfloat
  }

make_lenses_tuple "allstate" ("client", "gfx")
make_lenses_record "gfx" ''GfxPP.GfxState
