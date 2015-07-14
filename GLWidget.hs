{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module GLWidget where

import Linear
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict hiding (forM)

import Graphics.Rendering.OpenGL hiding (position,get)
import Graphics.Rendering.OpenGL.GLU

import qualified Graphics.Rendering.FTGL as FTGL
import Foreign.C.Types

import GHC.Generics

import Data.List.Split

import Prelude hiding ((.))
import Control.Category

import GLUtil
import Util

import qualified Config

import Lens

class WidgetClass a where
  create :: MonadIO m => m a
  render :: MonadIO m => StateT a m ()
  refit :: MonadIO m => StateT a m ()
  position :: Lens a (V2 CFloat)
  padding :: Lens a (V2 CFloat)
  size :: Lens a (V2 CFloat)
  content_size :: Lens a (V2 CFloat)
  focused :: Lens a Bool
  bounds :: Lens a (V2 CFloat, V2 CFloat)

--data GLWidgetType = BaseType
--                  | TextType
--                  | PaneType

data Base = BaseC {base_focused :: Bool
                  ,base_position :: V2 CFloat
                  ,base_padding :: V2 CFloat
                  ,base_size :: V2 CFloat
                  ,base_bounds :: (V2 CFloat, V2 CFloat)
                  ,base_content_size :: V2 CFloat}
          deriving (Ord, Eq, Show)

make_lenses_record "base" ''GLWidget.Base

instance WidgetClass Base where
  create = return $ BaseC {base_focused = False
                          ,base_position = V2 0 0
                          ,base_padding = V2 0 0
                          ,base_size = V2 0 0
                          ,base_bounds = (V2 0 0, V2 0 0)
                          ,base_content_size = V2 0 0}

  render = return ()

  refit = do
    pad <- get_lens padding
    pos <- get_lens position
    csize <- get_lens content_size
    bounds != (pos, pos + csize + 2*pad)
    size != csize + 2 * pad

  size = base_size_in_base
  content_size = base_content_size_in_base
  focused = base_focused_in_base
  padding = base_padding_in_base
  position = base_position_in_base
  bounds = base_bounds_in_base

--deriving instance Show GLText.Text
--deriving instance Eq GLText.Text
--deriving instance Eq GLPanel.Panel

data Arrangement = Horizontal
                 | Vertical
                 | None
                 deriving (Eq, Ord, Show, Generic)

data Panel = Panel {base :: Base
                   ,arrange :: Arrangement
                   ,content :: [GLWidget]}
           deriving (Eq, Ord, Show, Generic)

data Text = Text {text_base :: Base
                 ,text_content :: [String]
                 ,text_font :: FTGL.Font
                 }
          deriving (Eq, Ord, Show, Generic)

data GLWidget = GLBase Base
              | GLText Text
              | GLPanel Panel
              deriving (Ord, Eq, Show, Generic)

make_lenses_record "text" ''GLWidget.Text
make_lenses_record "panel" ''GLWidget.Panel

base_in_widget = Lens {
  getter = (\ e -> case e of
--              GLBase b -> b
              GLText b -> (getter text_base_in_text) b
              GLPanel b -> (getter base_in_panel) b),
  putter = (\ e v -> case e of
--              GLBase b -> GLBase v
              GLText b -> GLText $ (putter text_base_in_text) b v
              GLPanel b -> GLPanel $ (putter base_in_panel) b v)
  }

instance WidgetClass GLWidget where
  create = return $ GLBase $ BaseC {base_focused = False}
  refit = do
    b <- get
    case b of
      GLBase b -> do
        (_, nb) <- (flip runStateT) b refit
        put $ GLBase nb
      GLText b -> do
        (_, nb) <- (flip runStateT) b refit
        put $ GLText nb
      GLPanel b -> do
        (_, nb) <- (flip runStateT) b refit
        put $ GLPanel nb

  content_size = base_content_size_in_base . base_in_widget
  size = base_size_in_base . base_in_widget
  position = base_position_in_base . base_in_widget
  bounds = base_bounds_in_base . base_in_widget

  render = do
    b <- get
    case b of
      GLBase b -> do
        (_, nb) <- (flip runStateT) b render
        put $ GLBase nb
      GLText b -> do
        (_, nb) <- (flip runStateT) b render
        put $ GLText nb
      GLPanel b -> do
        (_, nb) <- (flip runStateT) b render
        put $ GLPanel nb

instance WidgetClass Text where
  size = base_size_in_base . text_base_in_text
  padding = base_padding_in_base . text_base_in_text
  content_size = base_content_size_in_base . text_base_in_text
  focused = base_focused_in_base . text_base_in_text
  position = base_position_in_base . text_base_in_text
  bounds = base_bounds_in_base . text_base_in_text
  
  create = do
    font <- Config.default_font
    base_widget <- create
    return $ Text {text_base = base_widget,
                   text_content = [],
                   text_font = font
                  }
  refit = with_lens text_base_in_text refit

  render = do
    p <- get
    font <- gets text_font
    pad <- get_lens padding

    let font_size = FTGL.fgetFontLineHeight font

    content <- gets text_content
    
    apos <- get_lens position
    asize@(V2 width height) <- get_lens size

    liftIO $ do
      matrixMode $= Modelview 0
      preservingMatrix $ do
        color $ Color4 0.5 0.0 0.0 (0.25 :: GLfloat)
        translate $ v2vector3 $ apos
        renderPrimitive Quads $ do
          vertex_float3 (0,0,0)
          vertex_float3 (width,0,0)
          vertex_float3 (width,height,0)
          vertex_float3 (0,height,0)

        color $ Color4 1.0 1.0 1.0 (1.0 :: GLfloat)

        translate $ v2vector3 pad
        draw_text font content

        return ()

set_content :: MonadIO m => String -> StateT Text m ()
set_content c = do
  p <- get
  font <- gets text_font
  let pieces = splitOn "\n" c
  (max_width, total_height) <-
    foldM ( \(max_width,total_height) s -> do
                width <- liftIO $ FTGL.getFontAdvance font s
                let height = FTGL.getFontLineHeight font
                return (max max_width width, total_height + height) ) (0,0) pieces

  text_content_in_text != pieces
  content_size !=  V2 (realToFrac max_width) (realToFrac total_height)

  refit

add_element :: (MonadIO m) => GLWidget -> StateT Panel m ()
add_element e = do
  old <- gets content
  content_in_panel != old ++ [e]

instance WidgetClass Panel where
  size = base_size_in_base . base_in_panel
  content_size = base_content_size_in_base . base_in_panel
  focused = base_focused_in_base . base_in_panel
  padding = base_padding_in_base . base_in_panel
  position = base_position_in_base . base_in_panel
  bounds = base_bounds_in_base . base_in_panel

  create = do
    base <- create
    return $ Panel {base = base
                   ,arrange = Horizontal
                   ,content = []}
  render = do
    p <- get
    apos@(V2 x y) <- get_lens position
    asize@(V2 w h) <- get_lens size
    V2 cw ch <- get_lens content_size

    pad <- get_lens padding

    preservedMatrix $ do

      liftIO $ do
        translate $ v2vector3 apos
        color $ Color4 0 1.0 0 (0.25 :: GLfloat)
        renderPrimitive Quads $ do
          vertex_float3 (0,0,0)
          vertex_float3 (w,0,0)
          vertex_float3 (w,h,0)
          vertex_float3 (0,h,0)

        translate $ v2vector3 pad

        color $ Color4 0 0.0 1.0 (0.25 :: GLfloat)
        renderPrimitive Quads $ do
          vertex_float3 (0,0,0)
          vertex_float3 (cw,0,0)
          vertex_float3 (cw,ch,0)
          vertex_float3 (0,ch,0)

      with_lens content_in_panel $ do
        old <- get
        new_content <- forM old (\ c -> execT (return c) render)
        put new_content
        return ()

  refit = do
    (p :: Panel) <- get
    with_lens content_in_panel $ do
      (old :: [GLWidget]) <- get
      new_content <- forM old (\ c -> execT (return c) refit)
      put new_content
      return ()

    pad <- get_lens padding
    arr <- gets arrange

    let scale = case arr of
          None -> V2 0 0
          Horizontal -> V2 1 0
          Vertical -> V2 0 1

    (final_min,final_max) <- with_lens content_in_panel $ do
      (old :: [GLWidget]) <- get
      (_, final_bounds, new_content) <-
        (foldM (\ (old_cursor,(old_edge1,old_edge2),n) old_p -> do
                    
                    ((new_cursor, new_bounds), new_p) <- (flip runStateT) old_p $ do
                      position != old_cursor
                      refit
                      asize <- get_lens size
                      (edge1, edge2) <- get_lens bounds
                      let next_bounds = (v2min edge1 old_edge1,
                                         v2max edge2 old_edge2)
                      let next_cursor = old_cursor + scale * (asize + pad)
                      return (next_cursor, next_bounds)
                    
                    return (new_cursor,new_bounds, new_p : n))
         (V2 0 0
         ,(V2 1000 1000
          ,V2 (-1000) (-1000))
         ,[]) old)

      put new_content
      return final_bounds

    content_size != final_max - final_min
    size != (final_max + pad) - (final_min - pad)
    bounds != (final_min - pad, final_max + pad)

