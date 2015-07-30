{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
--{-# LANGUAGE OverloadedRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module GLWidgetPP where

import GHC.Generics

import Linear
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict hiding (forM)

import Language.Haskell.TH

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

import SDL.Input.Keyboard

import qualified Control.Monad.Identity as I

import Lens

--data GLWidget s = forall w . Widget w => GLWidget (w s)
--data GLWidget s = GLText (Text s)
--                | GLPanel (Panel s)
data GLWidget = forall w . Widget w => GLWidget w

--  WidgetPack :: Widget a => a -> GLWidget
--withGLWidget :: (Monad m, Widget a) => StateT (a s) m r -> StateT GLWidget m r
--withGLWidget comp = do
--  GLWidget w <- get
--  (ret, new_w) <- lift $ runStateT comp w
--  put $ GLWidget new_w
--  return ret

type WidgetHandlerT statetype widgettype eventtype m =
  eventtype -> Lens statetype widgettype -> StateT statetype m ()
type WidgetHandler s w e = WidgetHandlerT s w e I.Identity

type KeyInput  = Either Keycode Scancode
type TextInput = String

type KeyHandlerT  statetype widgettype m = WidgetHandlerT statetype widgettype KeyInput m
type TextHandlerT statetype widgettype m = WidgetHandlerT statetype widgettype TextInput m
type KeyHandler  s w = KeyHandlerT  s w I.Identity
type TextHandler s w = TextHandlerT s w I.Identity

data KeyHandlerD w  = MkKeyHandlerD  (Monad m => KeyInput  -> Lens s w -> StateT s m ())
data TextHandlerD w = MkTextHandlerD (Monad m => TextInput -> Lens s w -> StateT s m ())

--type KeyHandler w    = forall m . Monad m => Either Keycode Scancode -> StateT w m ()
--type ChangeHandler w = forall m . Monad m => StateT w m ()

data Base w = Base {focused      :: Bool
                   ,position     :: V2 CFloat
                   ,padding      :: V2 CFloat
                   ,size         :: V2 CFloat
                   ,bounds       :: (V2 CFloat, V2 CFloat)
                   ,content_size :: V2 CFloat
                   ,handler_key    :: KeyHandlerD  w
                   ,handler_change :: TextHandlerD w
                   }

--temp :: Lens Base (ChangeHandler Base)
--temp = Lens
--       (handler_change :: Base -> ChangeHandler Base)
--       (\ old_data
--        -> \ (new_value :: ChangeHandler Base)
--           -> old_data {handler_change = new_value})

data Text = Text {text_base           :: Base Text
                 ,text_content        :: [String]
                 ,text_font           :: FTGL.Font
                 ,text_cursor         :: V2 Int
                 }

data Arrangement = Horizontal
                 | Vertical
                 | None
                 deriving (Eq, Ord, Show, Generic)

data Panel = Panel {panel_base        :: Base Panel
                   ,panel_focus_index :: Int
                   ,panel_wrap_focus  :: Bool
                   ,panel_arrange     :: Arrangement
                   ,panel_content     :: [GLWidget]}

--data GLWidget s = GLText (Text s)
--                | GLPanel (Panel s)
--                deriving (Generic)

class Widget t where
  widget_super :: t -> GLWidget
  
  widget_position       :: Lens t (V2 GLfloat)
  widget_size           :: Lens t (V2 GLfloat)
  widget_content_size   :: Lens t (V2 GLfloat)
  widget_bounds         :: Lens t (V2 GLfloat, V2 GLfloat)
  widget_handler_key    :: Lens t (KeyHandlerD  t)
  widget_handler_change :: Lens t (TextHandlerD t)

  widget_render :: MonadIO m => StateT t m ()
  widget_refit  :: Monad m   => StateT t m ()
  widget_create :: Monad m   => m t

  widget_has_focus  :: Monad m => StateT t m Bool
  widget_next_focus :: Monad m => StateT t m (Maybe Int)

  widget_handle_key    :: (Monad m) => KeyHandlerT  s t m
  widget_handle_change :: (Monad m) => TextHandlerT s t m

--inWidget :: forall s . Lens (GLWidget s) (Widget w => (w s))
--inWidget f = Lens
--           (\ (GLWidget w) -> (f w))
--           (\ (GLWidget w) w' -> GLWidget (f w'))


makeInWidget f =
  [e| do
       GLWidget w <- get
       (ret, w') <- runStateT $(varE f) w
       put $ GLWidget w'
       return ret
    |]

-- Lens w r -> Lens GLWidget r
--makeInWidgetLens l =
--  [| \ f glw -> fmap ( 
--     ( \ (GLWidget w) -> (lens_getter $(varE l)) w )
--     ( \ (GLWidget w) p -> GLWidget ((lens_putter $(varE l)) w p) )
--   |]

---- Lens s (w s) -> Lens s (GLWidget s)
--makeOutWidgetLens l =
--  [| Lens
--     ( \ s -> widget_super ((lens_getter $(varE l)) s ) )
--     ( \ s p -> widget_super ((lens_putter $(varE l)) s p) )
--   |]

-- Lens s (GLWidget s) -> Lens s (w s)
--makeOutWidgetLens l t = do
--  s <- newName "s"
--  p <- newName "p"
--  w <- newName "s"
--  casein1 <- [e| (lens_getter $(varE l)) $(varE s) |]
--  casein2 <- [e| (lens_putter $(varE l)) $(varE s) $(varE p) |]
--  let caseexp1 = return $ LamE [VarP s] ( CaseE casein1
--                                 [Match (ConP t [VarP w]) (NormalB (VarE w)) []] )
--  let caseexp2 = return $ LamE [VarP s, VarP p] ( CaseE casein1
--                                 [Match (ConP t [VarP w]) (NormalB (VarE w)) []] )
--  [| Lens $(caseexp1) $(caseexp2) |]

-- Lens s (w s) -> Lens s (GLWidget s)
--makeOutHandlerLens l t =
--  [| Lens
--     ( \ s -> widget_super ((lens_getter $(varE l)) s) )
--     ( \ s w -> (lens_putter $(varE l)) s (widget_super w) ) |]

-- HandlerT (w s) -> HandlerT (GLWidget s)
--makeInHandler h t et =
--  [| (\ e l -> $(varE h) e $(makeOutWidgetLens 'l t)) :: (WidgetHandler s (GLWidget s) $(conT et)) |]

-- HandlerT (GLWidget s) -> HandlerT (t s)
--makeOutHandler h t = do
--  s <- newName "s"
--  [| (\ e linside -> $(varE h) e $(makeOutHandlerLens 'linside t)) :: (WidgetHandler s ($(conT t) s) e) |]
--  [| (\ e linside -> $(varE h) e $(makeOutHandlerLens 'linside t)) |]

--     ( \ s -> case ((lens_getter $(varE l)) s ) of
--         GLText w -> w
--         GLPanel w -> w
--         )
--     ( \ s p -> case ((lens_putter $(varE l)) s p) of
--         GLText w -> w
--         GLPanel w -> w
--         )) :: Lens s ($(conT t) w)
--   |]

--makeInWidget f =
--  [e| do
--       GLWidget w <- get
--       (ret, w') <- runStateT $(varE f) w
--       put $ GLWidget w'
--       return ret
--    |]
--
---- Lens (w s) r -> Lens (GLWidget s) r
--makeInWidgetLens l =
--  [| Lens
--     ( \ (GLWidget w) -> (lens_getter $(varE l)) w )
--     ( \ (GLWidget w) p -> GLWidget ((lens_putter $(varE l)) w p) )
--   |]
--
---- Lens (GLWidget w) r -> Lens (w s) r
--makeOutWidgetLens l =
--  [| Lens
--     ( \ w -> (lens_getter $(varE l)) (GLWidget w) )
--     ( \ w p -> case ((lens_putter $(varE l)) (GLWidget w) p) of
--         GLWidget w' -> w'
--     )
--   |]

--inWidget f = do
--  GLWidget w <- get
--  (ret, w') <- runStateT f w
--  put $ GLWidget w'
--  return ret
  
--handler_change_in_base :: Lens (Base s) (TextHandler s (Base s))
--handler_change_in_base = Lens
--                         ( \ (b :: (Base s)) -> handler_change b )
--                         (\ old_data_arZS
--                          -> \ new_value_arZT
--                             -> old_data_arZS {handler_change = new_value_arZT})

--instance (Widget a, Widget b) => Widget (a :+: b) where
--  widget_position (L1 x) = widget_position x

--handler_key_in_base :: Lens (Base s w) (TestKeyHandlerT w)
--handler_key_in_base = Lens
--                         ( \ b -> handler_key b )
--                         (\ old_data_arZS
--                          -> \ new_value_arZT
--                             -> old_data_arZS {handler_key = new_value_arZT})
                      
make_lenses_record "base"  ''GLWidgetPP.Base
make_lenses_record "panel" ''GLWidgetPP.Panel
make_lenses_record "text"  ''GLWidgetPP.Text
