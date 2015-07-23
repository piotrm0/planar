{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}

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

import qualified Control.Monad.Identity as I

import qualified Config
import Lens
import Util
import GLUtil
import GfxUtil
import GLLog

import Lens

--type AllDataT cs m = (cs, GfxDataT cs m)

type KeyHandler  cs m = SDL.Keysym -> AllStateT cs m ()
type DrawHandler cs m = Float      -> AllStateT cs m ()
type DropHandler cs m = String     -> AllStateT cs m ()

data GfxData cs m where
  GfxData :: {
    window :: SDL.Window
    ,renderer :: SDL.Renderer
    ,key_handler  :: KeyHandler cs m
    ,draw_handler :: DrawHandler cs m
    ,drop_handler :: DropHandler cs m
    ,frame_timer :: FrameTimer
    ,glcontext :: SDL.GLContext
    ,log :: Log
    ,window_size :: V2 CInt
    ,bg_rot :: GL.GLfloat
    } -> GfxData cs m

type GfxStateT cs m r = StateT (GfxData cs m) m r
type GfxState cs r = GfxStateT cs I.Identity r

type AllData cs m = (cs, GfxData cs m)
type AllStateT cs m r = StateT (AllData cs m) m r
type AllState cs r = AllStateT cs I.Identity r

make_lenses_tuple "allstate" ("client", "gfx")

--drop_handler_in_gfx :: Lens (GfxData cs) DropHandler
--drop_handler_in_gfx =
--  Lens
--  ( \ GfxData {drop_handler = x} -> x )
--  (\ old_data
--   -> \ new_value
--      -> old_data {drop_handler = new_value})

make_lenses_record "gfx" ''GfxPP.GfxData
