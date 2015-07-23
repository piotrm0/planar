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

import System.IO.Unsafe

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

get_font :: String -> Int -> Int -> Font
get_font fontfile spacing size =
  unsafePerformIO $ do
    font <- FTGL.createTextureFont $ fontfile
    FTGL.setFontFaceSize font spacing size
    FTGL.setCharMap font FTGL.EncodingNone
    return font

draw_text :: (MonadIO m) => FTGL.Font -> [String] -> Maybe (V2 Int) -> StateT s m ()
draw_text font lines mc =
  let line_height = realToFrac (FTGL.getFontLineHeight font) in
  let font_height = realToFrac (fontFaceSize font) in
    let line_height  = realToFrac $ FTGL.getFontLineHeight font in
    let line_ascend  = realToFrac $ FTGL.fgetFontAscender font in
    let line_descend = realToFrac $ FTGL.fgetFontDescender font in do
    preservedMatrix $ do
      liftIO $ do rotate 180 $ Vector3 1 0 (0 :: GLfloat)
                  translate $ Vector3 0 (-line_ascend) (0 :: GLfloat)
      foldM_ (\ i s -> do
                  width <- liftIO $ FTGL.getFontAdvance font s
                  case mc of
                     Just (V2 cx cy) ->
                       if i == cy then do
                         liftIO $ color $ Color4 0.0 0.0 0.0 (0.25 :: GLfloat)
                         v2solid_box (V2 0 line_ascend) (V2 (realToFrac width) line_descend)
                         let before_cursor = take (cx) s
                         width_before_cursor <- liftIO $ FTGL.getFontAdvance font before_cursor
                         let after_cursor = take (cx+1) s
                         width_after_cursor <- liftIO $ FTGL.getFontAdvance font after_cursor
                         liftIO $ color $ Color4 0.0 1.0 0.0 (1.0 :: GLfloat)
                         v2box
                           (V2 (realToFrac width_before_cursor) line_ascend)
                           (V2 (realToFrac width_after_cursor) line_descend)
                         return ()
                       else return ()
                     Nothing -> return ()
                  liftIO $ do color $ Color4 0.0 0.0 0.0 (1.0 :: GLfloat)
                              FTGL.renderFont font s FTGL.Front
                              translate $ Vector3 0 (-line_height) (0 :: GLfloat)
                  return (i + 1)
             ) 0 lines

--- matrix transformations

preservedMatrix :: (MonadIO m) => StateT s m r -> StateT s m r
preservedMatrix comp = do
  liftIO $ GLR.glPushMatrix
  r <- comp
  liftIO $ GLR.glPopMatrix
  return r

v2vector3 (V2 x y) = Vector3 x y 0

v2box :: (MonadIO m) => V2 GLfloat -> V2 GLfloat -> m ()
v2box (V2 x1 y1) (V2 x2 y2) =
  liftIO $ renderPrimitive LineLoop $ do
    vertex $ Vertex3 x1 y1 0
    vertex $ Vertex3 x1 y2 0
    vertex $ Vertex3 x2 y2 0
    vertex $ Vertex3 x2 y1 0

v2solid_box :: (MonadIO m) => V2 GLfloat -> V2 GLfloat -> m ()
v2solid_box (V2 x1 y1) (V2 x2 y2) =
  liftIO $ renderPrimitive Quads $ do
    vertex $ Vertex3 x1 y1 0
    vertex $ Vertex3 x1 y2 0
    vertex $ Vertex3 x2 y2 0
    vertex $ Vertex3 x2 y1 0

--- font things

fontStringLength :: Fractional r => Font -> String -> r
fontStringLength f s =
  realToFrac $ unsafePerformIO $ FTGL.getFontAdvance f s

fontFaceSize :: Fractional r => Font -> r
fontFaceSize f = realToFrac $ unsafePerformIO $ FTGL.getFontFaceSize f
