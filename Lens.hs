{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Lens where

import Control.Category
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Language.Haskell.TH
import Util

--import Data.StateVar (HasSetter,($=))

infixl 9 #@
class Apply a where
  (#@) :: a -> a -> a
instance Apply Type where
  (#@) = AppT
instance Apply Exp where
  (#@) = AppE

type Getter intype fieldtype = intype -> fieldtype
type Putter intype fieldtype = intype -> fieldtype -> intype

--type Modifier intype fieldtype = (fieldtype -> fieldtype) -> intype -> intype

data Lens intype fieldtype = Lens {getter :: Getter intype fieldtype
                                  ,putter :: Putter intype fieldtype}

compose_lenses :: Lens b c -> Lens a b -> Lens a c
compose_lenses lens_bc lens_ab
  = Lens {getter = \ cont_a -> getter lens_bc $ getter lens_ab $ cont_a
                               
         ,putter = \ cont_a val_c -> (putter lens_ab) cont_a $ (putter lens_bc) ((getter lens_ab) cont_a) val_c
         }

instance Category Lens where
  id = Lens {getter = \ x -> x,
             putter = \ _ x -> x}
  (.) = compose_lenses 

with_lens :: forall intype fieldtype rettype m
             . (MonadIO m)
             => Lens intype fieldtype
             -> StateT fieldtype m rettype -> StateT intype m rettype
with_lens l field_comp = do
  inval <- get
  let fieldval = (getter l) inval
  (out, next_val) <- lift (runStateT field_comp fieldval)
  put $ (putter l) inval next_val
  return out

--instance Monad (Lens a b) where
--  return x = put x
--  (>>=) f c = with_lens  

put_lens :: (MonadIO m) => Lens a b -> b -> StateT a m ()
put_lens l field_val =
  with_lens l $ put field_val

get_lens :: (MonadIO m) => Lens a b -> StateT a m b
get_lens l =
  with_lens l $ get

--instance HasSetter (Lens a b) b where
--  ($=) :: Lens a b -> b -> StateT a m ()
--  ($=) l fieldval = put_lens l fieldval

infixr 2 !=
(!=) :: (MonadIO m) => Lens a b -> b -> StateT a m ()
(!=) = put_lens

infixr 2 !~
(!~) :: (MonadIO m) => Lens a b -> (b -> b) -> StateT a m b
(!~) l f = do
  v <- get_lens l
  put_lens l (f v)
  get_lens l

make_with :: String -> Name -> Q [Dec]
make_with field_name lens = do
  lens_type <- reify lens
  let lens_cons_name = mkName "Lens.Lens"
  let VarI _ (ForallT for_alls _ (AppT (AppT (ConT lens_cons_name) in_type) field_type)) _ _ = lens_type
  let fun_name = mkName $ "with_" ++ field_name  
  let ret_type = mkName "ret_type"
  let monad_type = mkName "m"
  let monadio = mkName "MonadIO"
  let statet = mkName "StateT"
  let ctx = [ClassP monadio [ VarT monad_type ]]
  let type_declare = SigD fun_name $ ForallT (for_alls ++ [PlainTV monad_type, PlainTV ret_type]) ctx
                                     (ArrowT #@
                                      ((ConT statet) #@ field_type #@ (VarT monad_type) #@ (VarT ret_type)) #@
                                      ((ConT statet) #@ in_type #@ (VarT monad_type) #@ (VarT ret_type)))
  let def_declare = FunD fun_name [Clause [] (NormalB ((VarE (mkName "with_lens")) #@ (VarE lens))) []]
  return [type_declare,def_declare]

make_lenses_tuple :: String -> (String, String) -> Q [Dec]
make_lenses_tuple postfix (fstname, sndname) = do
  lens_fst <- [e| Lens.Lens {getter = fst
                               ,putter = \(_, old_snd) new_fst -> (new_fst, old_snd)} |]
  lens_snd <- [e| Lens.Lens {getter = snd
                               ,putter = \(old_fst, _) new_snd -> (old_fst, new_snd)} |]
  return [ValD
          (VarP (mkName (fstname ++ "_in_" ++ postfix)))
          (NormalB lens_fst)
          []
         ,ValD
          (VarP (mkName (sndname ++ "_in_" ++ postfix)))
          (NormalB lens_snd)
          []
         ]

-- When using this, make sure you double quote the data/type name as
-- in make_lenses_record "some_prefix" ''SomeType .
-- Adapted from Data.Lens.Template
make_lenses_record :: String -> Name -> Q [Dec]
make_lenses_record postfix rec_type_name = do
  info <- reify rec_type_name
  let TyConI (DataD context _ params constructors _) = info
  let params' = map (\ x -> case x of (PlainTV n) -> n; (KindedTV n _) -> n) params
  let appliedT = foldl AppT (ConT rec_type_name) (map VarT params')
  forM_flat constructors $ \cons -> do
    let RecC cons_name fields = cons
    forM_flat fields $ \field -> do
      let (field_name, _, field_type) = field
      let lens_name = mkName ((nameBase field_name) ++ "_in_" ++ postfix)
      old_data <- newName "old_data"
      new_value <- newName "new_value"
      let lens_sig = SigD lens_name ((ForallT params)
                                     context
                                     (AppT
                                      (AppT
                                       (ConT
                                        (mkName "Lens"))
                                       appliedT) field_type))
      let getter_body = VarE field_name
      let putter_body = LamE [VarP old_data]
                        (LamE [VarP new_value]
                         (RecUpdE (VarE old_data) [(field_name, VarE new_value)]))
      return $ [lens_sig,
                ValD
                (VarP lens_name)
                (NormalB (RecConE (mkName "Lens") [(mkName "getter", getter_body)
                                                  ,(mkName "putter", putter_body)])) []]

print_type t = do
  tv <- t
  runIO $ do
    putStrLn $ "type=" ++ (show tv)
  return []
