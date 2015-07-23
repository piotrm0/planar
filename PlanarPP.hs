-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ImpredicativeTypes #-}

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

import qualified Control.Monad.Identity as I

import Data.Set

import qualified Graphics.Rendering.OpenGL as GL

import GLWidgetPP as GLW
import qualified Gfx as G

type AllData m     = G.AllData   (ClientData m) m
type AllStateT m r = G.AllStateT (ClientData m) m r
type AllState r    = AllStateT I.Identity r
--type AllState r = G.AllStateT (ClientState m) m r

--type AllState = (ClientState, G.GfxState)

data ClientData m =
  ClientData {rot :: GL.GLfloat
             ,nothing :: m Int
             ,panel :: GLW.Panel (AllData m)}

make_lenses_record "client" ''ClientData
