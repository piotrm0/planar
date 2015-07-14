module PlanarPP where
import qualified GfxPP as G
import qualified GfxUtil as GU
import Util

import Lens
import qualified Stream
import Util
import GLUtil

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

import qualified Graphics.Rendering.OpenGL as GL

--import qualified GLText as Text
--import qualified GLPanel

import GLWidget as GLW

data ClientState =
  ClientState {rot :: GL.GLfloat
              ,panel :: GLW.Panel}


type AllStateT m r = G.AllStateT ClientState m r

make_lenses_record "client" ''ClientState
