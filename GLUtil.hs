{-# LANGUAGE TemplateHaskell #-}

module GLUtil where

--import Prelude hiding ((.))
--import Control.Category
--
--import System.Environment
--import SDL
--import Control.Monad
--import Control.Monad.Primitive
import Control.Monad.IO.Class
--import Control.Monad.State.Strict
--import Data.Functor.Identity
--import Foreign.C.Types
--import Data.Word
--import Data.Bits
--import Data.Int
--import qualified SDL.Raw.Timer as Raw
--import qualified Util as U
--import Lens

import Linear (V2, V2(V2))

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GLU

-- Viewport setups

viewport_center :: (MonadIO m, Real e) => V2 e -> m ()
viewport_center (V2 width height) =
  let aspect = fromRational (50 * (toRational height) / (toRational width)) in liftIO $ do
--  let half_width = (fromIntegral width) / 2 in
--  let half_height = (fromIntegral height) / 2 in liftIO $ do
    matrixMode $= Projection
    loadIdentity
    frustum (-100) (100) (-aspect) aspect 0.1 100
    matrixMode $= Modelview 0
    loadIdentity

viewport_2d :: (MonadIO m, Real e) => V2 e -> m ()
viewport_2d (V2 width height) = liftIO $ do
  matrixMode $= Projection
  loadIdentity
  ortho2D 0 (realToFrac width) (realToFrac height) 0

  matrixMode $= Modelview 0
  loadIdentity

--- Other utilities
  
vertex_float3 :: (GLfloat,GLfloat,GLfloat) -> IO ()
vertex_float3 (a,b,c) = vertex (Vertex3 a b c)

