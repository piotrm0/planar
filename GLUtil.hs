{-# LANGUAGE TemplateHaskell #-}

module GLUtil where

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.IO.Class
import Linear (V2, V2(V2))
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GLU

import Data.List.Split

import qualified Graphics.Rendering.FTGL as FTGL
import qualified Graphics.Rendering.OpenGL.Raw as GLR

type Font = FTGL.Font

-- Viewport setups

viewport_center :: (MonadIO m, Real e) => V2 e -> m ()
viewport_center (V2 width height) =
--  let aspect = fromRational (0.05 * (toRational height) / (toRational width)) in liftIO $ do
  let aspect = fromRational ((toRational height) / (toRational width)) in liftIO $ do
--  let half_width = (fromIntegral width) / 2 in
--  let half_height = (fromIntegral height) / 2 in liftIO $ do
    matrixMode $= Projection
    loadIdentity
--    frustum (-0.1) (0.1) (-aspect) aspect 0.1 1000
    perspective 80 aspect 1 1000
--    matrixMode $= Modelview 0
--    loadIdentity

viewport_2d :: (MonadIO m, Real e) => V2 e -> m ()
viewport_2d (V2 width height) = liftIO $ do
  matrixMode $= Projection
  loadIdentity
  ortho2D 0 (realToFrac width) (realToFrac height) 0
--  matrixMode $= Modelview 0
--  loadIdentity

--- Other utilities
  
vertex_float3 :: (GLfloat,GLfloat,GLfloat) -> IO ()
vertex_float3 (a,b,c) = vertex (Vertex3 a b c)

-- Text related

create_font :: (MonadIO m) => String -> Int -> Int -> m Font
create_font fontfile spacing size =
  liftIO $ do
    font <- FTGL.createTextureFont $ fontfile
    FTGL.setFontFaceSize font spacing size
    FTGL.setCharMap font FTGL.EncodingNone
    return font

draw_text :: (MonadIO m) => FTGL.Font -> [String] -> m ()
draw_text font lines = do
  let line_height = realToFrac (FTGL.getFontLineHeight font)
  font_height <- liftIO $ fmap realToFrac (FTGL.getFontFaceSize font)
  liftIO $ do rotate 180 $ Vector3 1 0 (0 :: GLfloat)
              translate $ Vector3 0 (-font_height) (0 :: GLfloat)
  forM_ lines $ \s ->
    liftIO $ do FTGL.renderFont font s FTGL.Front
                translate $ Vector3 0 (-line_height) (0 :: GLfloat)

--- matrix transformations
preservedMatrix :: (MonadIO m) => StateT c m r -> StateT c m r
preservedMatrix comp = do
  liftIO $ GLR.glPushMatrix
  r <- comp
  liftIO $ GLR.glPopMatrix
  return r

v2vector3 (V2 x y) = Vector3 x y 0