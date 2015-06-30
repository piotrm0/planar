{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lens where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Language.Haskell.TH
import Util

infixl 9 #@
class Apply a where
  (#@) :: a -> a -> a
instance Apply Type where
  (#@) = AppT
instance Apply Exp where
  (#@) = AppE

type Getter intype fieldtype = intype -> fieldtype
type Putter intype fieldtype = intype -> fieldtype -> intype

data Lens intype fieldtype = Lens {getter :: Getter intype fieldtype
                                  ,putter :: Putter intype fieldtype}

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
make_lenses_tuple prefix (fstname, sndname) = do
  lens_fst <- [e| Lens.Lens {getter = fst
                               ,putter = \(_, old_snd) new_fst -> (new_fst, old_snd)} |]
  lens_snd <- [e| Lens.Lens {getter = snd
                               ,putter = \(old_fst, _) new_snd -> (old_fst, new_snd)} |]
  return [ValD
          (VarP (mkName (prefix ++ "_" ++ fstname)))
          (NormalB lens_fst)
          []
         ,ValD
          (VarP (mkName (prefix ++ "_" ++ sndname)))
          (NormalB lens_snd)
          []
         ]

-- When using this, make sure you double quote the data/type name as
-- in make_lenses_record "some_prefix" ''SomeType .
-- Adapted from Data.Lens.Template
make_lenses_record :: String -> Name -> Q [Dec]
make_lenses_record prefix rec_type_name = do
  info <- reify rec_type_name
  let TyConI (DataD context _ params constructors _) = info
  let params' = map (\ x -> case x of (PlainTV n) -> n; (KindedTV n _) -> n) params
  let appliedT = foldl AppT (ConT rec_type_name) (map VarT params')
  forM_flat constructors $ \cons -> do
    let RecC cons_name fields = cons
    forM_flat fields $ \field -> do
      let (field_name, _, field_type) = field
      let lens_name = mkName (prefix ++ "_" ++ (nameBase field_name))
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
