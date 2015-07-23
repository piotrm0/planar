--{-# LANGUAGE DataKinds #-}
--{-# LANGUAGE GADTs #-}
--{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
--{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ImpredicativeTypes #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}

module GLWidget (module GLWidgetPP
                ,set_content
                ,add_element
                ,basic_create
                ,basic_refit
                ,basic_next_focus
--                ,basic_handle_key
--                ,basic_render
--                ,widgetPackM
                ) where

import Data.Char

import Linear

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict hiding (forM)

import Graphics.Rendering.OpenGL hiding (position,get)
import Graphics.Rendering.OpenGL.GLU

import qualified Graphics.Rendering.FTGL as FTGL
import Foreign.C.Types

import SDL.Input.Keyboard

import GHC.Generics

import Data.List.Split

--import Data.List

import Prelude hiding ((.))
import Control.Category

import GLUtil
import Util

import qualified Config

import Lens

import GLWidgetPP

handlerNull :: Monad m => aninput -> alens -> m ()
handlerNull _ _ = return ()

basic_create :: Monad m => m (Base s w)
basic_create =
  return $ Base {handler_key    = handlerNull
                ,handler_change = handlerNull
                ,focused        = False
                ,position       = V2 0 0
                ,padding        = V2 0 0
                ,size           = V2 0 0
                ,bounds         = (V2 0 0,
                                   V2 0 0)
                ,content_size   = V2 0 0}

--  basic_handle_change = ChangeHandler $ return ()
--  basic_handle_key = KeyHandler $ \ k -> return ()

basic_refit ::  Monad m => StateT (Base s w) m ()
basic_refit = do
  pad   <- getLensT padding_in_base
  pos   <- getLensT position_in_base
  csize <- getLensT content_size_in_base
  bounds_in_base != (pos, pos + csize + 2*pad)
  size_in_base   != csize + 2 * pad

basic_next_focus :: Monad m => StateT (Base s w) m (Maybe Int)
basic_next_focus = do
  already <- getLensT focused_in_base
  if already
    then do focused_in_base != False
            return Nothing
    else do focused_in_base != True
            return $ Just 0

--widgetPackM :: forall m w . (Monad m, Widget w) => w -> m (GLWidget)
--widgetPackM p = return $ WidgetPack p

underGLWidget f w = case w of
  GLText t -> GLText (f t)
  GLPanel p -> GLPanel (f p)

instance Widget GLWidget where
  widget_handler_key =
    Lens
    (underGLWidget widget_handler_key)
    ( \ w s -> w )

--    ( Lens
--      ( \ (WidgetPack (w::a)) -> (lens_getter basic_handler_key) w )
--      ( \ (WidgetPack (w::a)) s -> WidgetPack ( (lens_putter basic_handler_key) w s ) ) ) :: Lens GLWidget (KeyHandler a)
--
--  widget_handler_change = Lens
--               ( \ (WidgetPack w) -> (lens_getter basic_handler_change) w )
--               ( \ (WidgetPack w) s -> WidgetPack ( (lens_putter basic_handler_change) w s ) )
--
--  widget_position = Lens
--               ( \ (WidgetPack w) -> (lens_getter basic_position) w )
--               ( \ (WidgetPack w) s -> WidgetPack ( (lens_putter basic_position) w s ) )
--
--  widget_size = Lens
--               ( \ (WidgetPack w) -> (lens_getter basic_size) w )
--               ( \ (WidgetPack w) s -> WidgetPack ( (lens_putter basic_size) w s ) )
--  
--  basic_content_size = Lens
--               ( \ (WidgetPack w) -> (lens_getter basic_content_size) w )
--               ( \ (WidgetPack w) s -> WidgetPack ( (lens_putter basic_content_size) w s ) )
--
--  basic_bounds = Lens
--               ( \ (WidgetPack w) -> (lens_getter basic_bounds) w )
--               ( \ (WidgetPack w) s -> WidgetPack ( (lens_putter basic_bounds) w s ) )
--
--  basic_render =
--    get >>= \ (WidgetPack w) -> execStateT basic_render w
--                                >>= \ nw -> put $ WidgetPack nw
--
--  basic_refit =
--    get >>= \ (WidgetPack w) -> execStateT basic_refit w
--                                >>= \ nw -> put $ WidgetPack nw
--
--  basic_has_focus =
--    get >>= \ (WidgetPack w) -> runStateT basic_has_focus w
--                                >>= \ (ret, nw) -> put (WidgetPack nw)
--                                                   >> return ret
--
--  basic_next_focus =
--    get >>= \ (WidgetPack w) -> runStateT basic_next_focus w
--                                >>= \ (ret, nw) -> put (WidgetPack nw)
--                                                   >> return ret
--
--  basic_handle_key i =
--    get >>= \ (WidgetPack w) -> runStateT (basic_handle_key i) w
--                                >>= \ (ret, nw) -> put (WidgetPack nw)
--                                                   >> return ret
   
instance Widget Text where
--  basic_handle_change = do
--    pieces <- gets text_content
--    ch <- gets text_change_handler
--    let s = concat pieces
----    ch s
--    return ()
--  basic_handler_key    = handler_key_in_base    . text_base_in_text
--  basic_handler_change = handler_change_in_base . text_base_in_text
  
  widget_position     = position_in_base     . text_base_in_text
  widget_size         = size_in_base         . text_base_in_text
  widget_content_size = content_size_in_base . text_base_in_text
  widget_bounds       = bounds_in_base       . text_base_in_text

  widget_handle_key k w = withLensT w $ do
    case k of
      Left KeycodeUp        -> offset_cursor (V2   0(-1))
      Left KeycodeDown      -> offset_cursor (V2   0  1)
      Left KeycodeLeft      -> offset_cursor (V2 (-1) 0)
      Left KeycodeRight     -> offset_cursor (V2   1  0)
      Left KeycodeReturn    -> linebreak_cursor
      Left KeycodeBackspace -> backspace_cursor
      Left kc -> do
        let akey = unwrapKeycode kc
        write_cursor (chr (fromIntegral akey))
  --liftIO $ putStrLn $ "unhandled keycode: " ++ (show kc)
      Right sc ->
        let akey = unwrapScancode sc in
        write_cursor (chr (fromIntegral akey))

  widget_next_focus = do
    already <- getLensT (focused_in_base . text_base_in_text)
    if already
      then do focused_in_base . text_base_in_text != False
              return Nothing
      else do focused_in_base . text_base_in_text != True
              return $ Just 0

  widget_create =
    let font = Config.default_font in do
      w <- basic_create
      return $ Text {text_base = w
                    ,text_content = []
                    ,text_font = font
                    ,text_cursor = (V2 0 0)
                    }

  widget_refit = do
    pieces <- gets text_content
    font   <- gets text_font
    let font_ascend  = FTGL.getFontAscender font
    let font_descend = FTGL.getFontDescender font
    let height       = FTGL.getFontLineHeight font
    (max_width, total_height) <-
      foldM ( \(max_width,total_height) s -> do
                  let width :: GLfloat = fontStringLength font s
                  return (max max_width width, total_height + height) ) (0,font_ascend) pieces

    text_content_in_text != pieces
    content_size_in_base . text_base_in_text
      != V2 (realToFrac max_width) (realToFrac (total_height - height - font_descend))

    withLensT text_base_in_text basic_refit

  widget_render = do
    p <- get
    font <- gets text_font
    pad <- getLensT (padding_in_base . text_base_in_text)

    let font_size = FTGL.fgetFontLineHeight font

    content <- gets text_content
    
    apos  <- getLensT (position_in_base . text_base_in_text)
    asize <- getLensT (size_in_base     . text_base_in_text)
    csize <- getLensT (content_size_in_base . text_base_in_text)
    c     <- getLensT text_cursor_in_text

    is_focused <- getLensT (focused_in_base . text_base_in_text)

    liftIO $ do matrixMode $= Modelview 0
    preservedMatrix $ do
      liftIO $ do translate $ v2vector3 $ apos
                  color $ Color4 1.0 1.0 1.00 (0.9 :: GLfloat)
      v2solid_box 0 asize

      liftIO $ do translate $ v2vector3 pad
                  color $ Color4 0.0 0.0 0.0 (0.25 :: GLfloat)
      v2box 0 csize

      liftIO $ do color $ Color4 0.0 0.0 0.0 (1.0 :: GLfloat)

      if is_focused
        then draw_text font content (Just c)
        else draw_text font content Nothing

--clip_cursor :: [Int] -> V2 Int -> V2 Int
--clip_cursor mask (V2 x y) =
--  let (x',y') = (if y < 0
--                 then (0,0)
--                 else if y >= length mask
--                      then (mask !! ((length mask) - 1), length mask - 1)
--                      else (x,y)) in
--  let row = mask !! y' in
--  let (x'',y'') = (if x' < 0
--                   then
--                     let ytemp = max 0 (y'-1) in
--                     let xtemp = max 0 ((mask !! ytemp) - 1) in
--                     (xtemp, ytemp)
--                   else
--                     if x' >= row then
--                       let ytemp = min ((length mask) - 1) (y'+1) in
--                       let xtemp = 0 in
--                       (xtemp,ytemp)
--                     else (x',y')) in
--  V2 x'' y''

offset_cursor :: Monad m => V2 Int -> StateT (Text s) m ()
offset_cursor coff = do
  c_current <- gets text_cursor
--  liftIO $ putStrLn $ "offsetting cursor: " ++ (show c_current) ++ " by " ++ (show coff)
  lines <- gets text_content
  let n = length lines
  let sizes = map length lines 
  let c_next = case (c_current, coff) of
        (V2 x 0, V2 0 (-1)) -> V2 0 0
        (V2 x y, V2 0 (-1)) -> V2 (min ((sizes !! (y-1)) - 1) x) (y-1)
        (V2 0 0, V2 (-1) 0) -> V2 0 0
        (V2 0 y, V2 (-1) 0) -> V2 ((sizes !! (y-1)) - 1) (y-1)
        (V2 x y, V2 (-1) 0) -> V2 (x-1) y
        (V2 x y, V2 1 0) -> if x >= (sizes !! y) - 1
                            then if y >= n - 1
                                 then V2 x y
                                 else V2 0 (y+1)
                            else V2 (x+1) y
        (V2 x y, V2 0 1) -> if y >= n - 1
                            then V2 ((sizes !! y)-1) y
                            else V2 (min ((sizes !! (y+1)) - 1) x) (y+1)
  text_cursor_in_text != c_next
  withLensT text_base_in_text basic_refit

write_cursor :: Monad m => Char -> StateT (Text s) m ()
write_cursor c = do
  V2 x y <- gets text_cursor
  text_cursor_in_text != V2 (x+1) y
  withLensT text_content_in_text $ do
    withIndexT y $ do
      withinIndexT x $ do
        [current_char] <- get
        put [current_char, c]

  withLensT text_base_in_text basic_refit

backspace_cursor :: Monad m => StateT (Text s) m ()
backspace_cursor = do
  V2 x y <- gets text_cursor
  lines <- gets text_content
  case (x, y) of
    (0,0) -> return ()
    (0,1) ->
      let before_line' : cline : after_lines = lines in
      let before_line = take ((length before_line') - 1) before_line' in
      do text_content_in_text != (before_line ++ cline) : after_lines
         text_cursor_in_text != V2 (max 0 (length before_line)) 0
    (0,y) ->
      let (before_lines, cline : after_lines) = splitAt y lines in
      let (parts1, [before_line']) = splitAt (y-1) before_lines in
      let before_line = take ((length before_line') - 1) before_line' in
      do text_content_in_text != parts1 ++ [before_line ++ cline] ++ after_lines
         text_cursor_in_text != V2 (max 0 (length before_line)) (y-1)
    (x,y) -> do
      text_cursor_in_text != V2 (max 0 (x-1)) y
      withLensT text_content_in_text $ do
        withIndexT y $ do
          line <- get
          let temp = case splitAt (x-1) line of
                (before_chars, []) -> before_chars
                (before_chars, cchar : after_chars) -> before_chars ++ after_chars
          put $ temp

linebreak_cursor :: Monad m => StateT (Text s) m ()
linebreak_cursor = do
  V2 x y <- gets text_cursor
  withLensT text_content_in_text $ do
    withinIndexT y $ do
      [line] <- get
      let (line1,line2) = splitAt x line
      put [line1 ++ "\n",line2]
  text_cursor_in_text != V2 0 (y+1)

cursor = text_cursor_in_text

set_content :: Monad m => String -> StateT (Text s) m ()
set_content s = do
  text_content_in_text != (map (++ "\n") $ splitOn "\n" s)
  withLensT text_base_in_text basic_refit

add_element :: Monad m => GLWidget s -> StateT (Panel s) m ()
add_element e = do
  old <- gets panel_content
  panel_content_in_panel != old ++ [e]

inc_focus :: Monad m => StateT (Panel s) m (Maybe Int)
inc_focus = do
  focused_in_base . panel_base_in_panel != True
  findex <- getLensT panel_focus_index_in_panel
  pieces <- getLensT panel_content_in_panel
  let num_pieces = length pieces
  got_subnext <- withLensT panel_content_in_panel $ do
    withIndexT findex $ do
      temp_findex <- widget_next_focus
      case temp_findex of
        Nothing -> return False
        Just _ -> return True
  if got_subnext
    then return $ Just findex
    else if findex >= num_pieces - 1
         then do focused_in_base . panel_base_in_panel != False
                 panel_focus_index_in_panel != 0
                 return Nothing
         else do panel_focus_index_in_panel != findex + 1
                 inc_focus

instance Widget Panel where
  widget_handler_key    = handler_key_in_base    . panel_base_in_panel
  widget_handler_change = handler_change_in_base . panel_base_in_panel
  widget_position     = position_in_base     . panel_base_in_panel
  widget_size         = size_in_base         . panel_base_in_panel
  widget_content_size = content_size_in_base . panel_base_in_panel
  widget_bounds       = bounds_in_base       . panel_base_in_panel
  
  widget_next_focus = inc_focus

  widget_handle_key k w = do
--    liftIO $ do putStrLn $ "Panel handle_key: " ++ (show k)
    findex <- withLensT w $ gets panel_focus_index
    cs     <- withLensT w $ getLensT panel_content_in_panel
    if findex < length cs
      then do let lens_index = (lensIndex findex) . panel_content_in_panel . w
              widget_handle_key k lens_index
      else return ()

  widget_create = do
    base <- basic_create
    return $ Panel {panel_base = base
                   ,panel_wrap_focus = False
                   ,panel_focus_index = 0
                   ,panel_arrange = Horizontal
                   ,panel_content = []}

  widget_render = do
    p <- get
    apos  <- getLensT $ widget_position
    asize <- getLensT $ widget_size
    csize <- getLensT $ widget_content_size

    pad <- getLensT (padding_in_base . panel_base_in_panel)

    preservedMatrix $ do
      liftIO $ do
        translate $ v2vector3 apos
        color $ Color4 0 0 0 (0.25 :: GLfloat)
        v2box 0 asize

        translate $ v2vector3 pad
        color $ Color4 0 0 0 (0.25 :: GLfloat)
        v2box 0 csize

      withLensT panel_content_in_panel $
        get >>= mapM (execStateT widget_render) >>= put

  widget_refit = do
    p <- get
    withLensT panel_content_in_panel $ do
      widgets <- get
      new_content <- mapM (execStateT widget_refit) widgets
      put new_content
      return ()

    pad <- getLensT $ padding_in_base . panel_base_in_panel
    arr <- gets panel_arrange

    let scale = case arr of
          None       -> V2 0 0
          Horizontal -> V2 1 0
          Vertical   -> V2 0 1

    (final_min,final_max) <- withLensT panel_content_in_panel $ do
      old <- get
      (_, final_bounds, new_content) <-
        (foldM (\ (old_cursor,(old_edge1,old_edge2),n) subwidget -> do
                    ((new_cursor, new_bounds), new_p) <- (flip runStateT) subwidget $ do
                      widget_position != old_cursor
                      widget_refit
                      asize <- getLensT $ widget_size
                      (edge1, edge2) <- getLensT $ widget_bounds
                      let next_bounds = (v2min edge1 old_edge1,
                                         v2max edge2 old_edge2)
                      let next_cursor = old_cursor + scale * (asize + pad)
                      return (next_cursor, next_bounds)
                    
                    return (new_cursor, new_bounds, new_p : n) )
         (V2 0 0, (V2 1000 1000, V2 (-1000) (-1000)), []) old)

      put new_content
      return final_bounds

    widget_content_size != final_max - final_min
    widget_size         != (final_max + pad) - (final_min - pad)
    widget_bounds       != (final_min - pad, final_max + pad)

    return ()