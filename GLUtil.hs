{-# LANGUAGE TemplateHaskell #-}

module GLUtil where

--import Prelude hiding ((.))
--import Control.Category
--
--import System.Environment
--import SDL
--import Control.Monad
--import Control.Monad.Primitive
--import Control.Monad.IO.Class
--import Control.Monad.State.Strict
--import Data.Functor.Identity
--import Foreign.C.Types
--import Data.Word
--import Data.Bits
--import Data.Int
--import qualified SDL.Raw.Timer as Raw
--import qualified Util as U
--import Lens

import Graphics.Rendering.OpenGL

vertex_float3 :: (GLfloat,GLfloat,GLfloat) -> IO ()
vertex_float3 (a,b,c) = vertex (Vertex3 a b c)

