{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances #-}

module List where

import qualified Data.Vector.Mutable as MV

import Control.Monad.Primitive
import Control.Monad.State.Strict
import Control.Applicative

import Data.Array.IO
import Data.Ratio
import Data.Monoid

-- Lists : various utility functions for manipulating lists, with or
-- without monad contexts.

forM_flat :: Monad m => [a] -> (a -> m [b]) -> m [b]
forM_flat items f = do
  foldM (\accum item -> do
             more_items <- f item
             return $ accum ++ more_items) [] items
