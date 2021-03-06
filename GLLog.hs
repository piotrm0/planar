module GLLog where

import Prelude hiding (length,drop)

import Data.Traversable
import Data.Sequence
import Linear
import Control.Monad.IO.Class
import Control.Monad.State.Strict hiding (forM)

import GLUtil
import Config

import Graphics.Rendering.OpenGL hiding (Level, get)
import qualified Graphics.Rendering.FTGL as FTGL

data Level = Critical
           | Debug
           | Message
             
data Item = Item {level :: Level,
                  text :: String}

data Log = Log {log_max_items :: Int
               ,log_font :: Font
               ,log_items :: Seq Item}

create :: (MonadIO m) => Int -> m Log
create size =
  let font = Config.default_font in
  return $ Log {log_max_items = size
               ,log_font = font
               ,log_items = empty}

add :: (MonadIO m) => Item -> StateT Log m ()
add i = do
  l@(Log size _ items) <- get
  let items' =
        if length items >= size then
          drop 1 items
        else
          items
  put $ l {log_items = items' |> i}
  
render :: (MonadIO m, Integral e, Functor m) => V2 e -> StateT Log m ()
render bounds@(V2 width height) = do
  let width' = fromIntegral width
  let height' = fromIntegral height
  font <- gets log_font
  font_size <- liftIO $ fmap fromIntegral $ FTGL.getFontFaceSize font
  items <- gets log_items
  let max_render = truncate $ (fromIntegral height) / font_size / 2
  let num_items = length items
  let to_render = drop (num_items - max_render) items 

  liftIO $ matrixMode $= Modelview 0
  preservedMatrix $ do
    liftIO $ do loadIdentity
                color $ Color4 0.0 0.0 0.0 (0.5 :: GLfloat)
                renderPrimitive Quads $ do
                  vertex_float3 (0,0,0)
                  vertex_float3 (width',0,0)
                  vertex_float3 (width',height'/2,0)
                  vertex_float3 (0,height'/2,0)

    forM to_render $ \(Item level text) -> do
        liftIO $ do color $ Color4 1.0 1.0 1.0 (0.5 :: GLfloat)
        draw_text font [text] Nothing
        liftIO $ do translate $ Vector3 0 font_size (0 :: GLfloat)
                    return ()

  return ()
