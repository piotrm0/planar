{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Lens where

import Control.Category
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Language.Haskell.TH
import Util

import Language.Haskell.TH.Ppr

--import Data.StateVar (HasSetter,($=))

infixl 9 #@
class Apply a where
  (#@) :: a -> a -> a
instance Apply Type where
  (#@) = AppT
instance Apply Exp where
  (#@) = AppE

type Getter a b = a -> b
type Putter a b = a -> b -> a

--type Modifier intype fieldtype = (fieldtype -> fieldtype) -> intype -> intype

--data Lens a b = Lens {getter :: Getter a b
--                     ,putter :: Putter a b}

data Lens a b where {Lens :: Getter a b -> Putter a b -> Lens a b}

--of_getput :: Getter a b -> Putter a b -> Lens f a b
--of_getput fget fput = L fget ( \ bmapper -> 

compose_lenses :: Lens b c -> Lens a b -> Lens a c
compose_lenses (Lens get_bc put_bc) (Lens get_ab put_ab)
  = Lens
    (\ cont_a -> get_bc (get_ab cont_a))
    (\ cont_a val_c -> put_ab cont_a $ put_bc (get_ab cont_a) val_c)

instance Category Lens where
  id = Lens ( \ x -> x)
            ( \ _ x -> x )
  (.) = compose_lenses

execLens :: Lens a b -> State b () -> State a b
execLens (Lens getter putter) inner_comp = do
  outer <- get
  let inner = getter outer
  let ((), inner') = runState inner_comp inner
  return $ inner'

withLens :: Lens a b -> State b r -> State a r
withLens (Lens getter putter) inner_comp = do
  outer <- get
  let inner = getter outer
  let (ret, inner') = runState inner_comp inner
  put $ putter outer inner'
  return ret

withLensT :: Monad m => Lens a b -> StateT b m r -> StateT a m r
withLensT (Lens getter putter) inner_comp = do
  outer <- get
  let inner = getter outer
  (ret, inner') <- lift $ runStateT inner_comp inner
  put $ putter outer inner'
  return ret
  
--instance Monad (Lens a b) where
--  return x = put x
--  (>>=) f c = withLens  

lens_getter (Lens x _) = x
lens_putter (Lens _ x) = x

putLens :: Lens a b -> b -> State a ()
putLens l field_val =
  withLens l $ put field_val

putLensT :: Monad m => Lens a b -> b -> StateT a m ()
putLensT l field_val =
  withLensT l $ put field_val

getLens :: Lens a b -> State a b
getLens l = withLens l $ get

getLensT :: (Monad m) => Lens a b -> StateT a m b
getLensT l = withLensT l $ get

--instance HasSetter (Lens a b) b where
--  ($=) :: Lens a b -> b -> StateT a m ()
--  ($=) l fieldval = put_lens l fieldval

infixr 2 !=
(!=) :: Monad m => Lens a b -> b -> StateT a m ()
(!=) = putLensT

infixr 2 !~
(!~) :: Lens a b -> (b -> b) -> State a b
(!~) l f = do
  v <- getLens l
  putLens l (f v)
  getLens l

lensIndex :: Int -> Lens [a] a
lensIndex i = Lens
              (\ l -> l !! i)
              (\ l e -> let (prefix, rest) = splitAt i l in
                 case rest of
                 [] -> []
                 _ : postfix -> prefix ++ [e] ++ postfix )

make_with :: String -> Name -> Q [Dec]
make_with field_name lens = do
  lens_type <- reify lens
  let lens_cons_name = mkName "Lens.Lens"
  let VarI _ (ForallT for_alls _ (AppT (AppT (ConT lens_cons_name) in_type) field_type)) _ _ = lens_type
  let fun_name = mkName $ "with_" ++ field_name  
  let ret_type = mkName "ret_type"
  let monad_type = mkName "m"
--  let monadio = mkName "MonadIO"
  let monadstate = mkName "MonadState"
  let statet = mkName "StateT"
  let ctx = [ClassP monadstate ((VarT monad_type) : in_type : [])]
  let type_declare = SigD fun_name $ ForallT (for_alls ++ [PlainTV monad_type, PlainTV ret_type]) ctx
                                     (ArrowT #@
                                      ((ConT statet) #@ field_type #@ (VarT monad_type) #@ (VarT ret_type)) #@
                                      ((ConT statet) #@ in_type #@ (VarT monad_type) #@ (VarT ret_type)))
  let def_declare = FunD fun_name [Clause [] (NormalB ((VarE (mkName "withLens")) #@ (VarE lens))) []]
  return [type_declare,def_declare]

make_lenses_tuple :: String -> (String, String) -> Q [Dec]
make_lenses_tuple postfix (fstname, sndname) = do
  lens_fst <- [e| Lens fst (\ (_, old_snd) new_fst -> (new_fst, old_snd)) |]
  lens_snd <- [e| Lens snd (\ (old_fst, _) new_snd -> (old_fst, new_snd)) |]
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
  let TyConI (DataD data_context _ data_binds constructors _) = info
  let data_binds' = map (\ x -> case x of (PlainTV n) -> n; (KindedTV n _) -> n) data_binds
  let appliedT = foldl AppT (ConT rec_type_name) (map VarT data_binds')
  forM_flat constructors $ \cons ->
    let (rec_binds, rec_context, rec_cons) = case cons of
          RecC _ _ -> ([], [], cons)
          ForallC x y z -> (x, y, z) in do
      let rec_binds' = map (\ x -> case x of (PlainTV n) -> n; (KindedTV n _) -> n) rec_binds
--    print_type (return cons)
      let RecC cons_name fields = rec_cons
      forM_flat fields $ \field -> do
        let (field_name, _, field_type) = field
--      runIO $ putStrLn (show field_type)
--      print_type (return field_type)
--        let (rec_types,rec_binders) = unzip $ filter ( \ (atype, abind) -> nameIsInType atype field_type) (zip rec_binds' rec_binds)
--        let context' = data_context ++ (filter ( \ pred -> typesInPred pred rec_binders ) rec_context )
        let context' = data_context ++ rec_context
        let rec_types = rec_binds'
        let rec_binders = data_binds ++ rec_binds
        let field_type' = field_type
      
        let lens_name = mkName ((nameBase field_name) ++ "_in_" ++ postfix)
        old_data <- newName "old_data"
        new_value <- newName "new_value"
        let lens_sig = SigD lens_name ((ForallT rec_binders)
                                       context'
                                       (AppT
                                        (AppT
                                         (ConT
                                          (mkName "Lens"))
                                         appliedT) field_type'))
        let getter_body = VarE field_name
        let putter_body = LamE [VarP old_data]
                          (LamE [VarP new_value]
                           (RecUpdE (VarE old_data) [(field_name, VarE new_value)]))
        let lens_body = ValD
                        (VarP lens_name)
                        (NormalB (AppE (AppE
                                        (ConE (mkName "Lens"))
                                        getter_body)
                                  putter_body)) []

--        print_type (return lens_sig)
--        print_type (return lens_body)

        return $ [lens_sig,
                  lens_body]

print_type t = do
  tv <- t

  runIO $ do putStrLn "\n--------------------"
             putStrLn $ (pprint tv)
             putStrLn "\n--------------------\n"
  return []

--nameIsInType aname intype = case intype of
--  ConT n -> aname == n
--  VarT n -> aname == n
--  AppT t1 t2 -> (nameIsInType aname t1) || (nameIsInType aname t2)
--  SigT t1 k -> nameIsInType aname t1
--  TupleT _ -> False
--  ArrowT -> False
--  ListT -> False
--
--nameIsInTypeList aname = any (nameIsInType aname)
--
--typesInPred pred types = case pred of
--  ClassP n ts -> all ( \ t -> typeIsIn t types ) ts
--  EqualP t1 t2 -> elem n types
--
--isTypeClosed names atype =
--  case atype of
--  ForallT morenames _ t -> isTypeClosed ( map ( \ newbind -> case newbind of
--                                                  PlainTV n -> n
--                                                  KindedTV n _ -> n ) morenames ) t
--  
--
--isPredClosed names apred =
--  case apred of
--  ClassP aname types -> all (all isTypeClosed names) types
--  Equal t1 t2 -> (isTypeClosed names t1) && (isTypeClosed names t2)

--import Data.List

