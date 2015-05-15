module Util where

import qualified Data.Vector.Mutable as MV
import Data.STRef
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.State.Lazy

data SumWindow m a = SumWindow a Int Int (MV.MVector (PrimState m) a)

sumwindow_add :: Num a => PrimMonad m => SumWindow m a -> a -> m (SumWindow m a)
sumwindow_add w@(SumWindow sum size current_p vector) new_val =
    let next_p = mod (current_p+1) size in do
    old_val <- MV.read vector next_p
    MV.write vector next_p new_val
    return $ SumWindow (sum - old_val + new_val) size next_p vector
sumwindow_new :: Num a => PrimMonad m => Int -> m (SumWindow m a)
sumwindow_new size = do
  v <- MV.new size
  MV.set v 0
  return $ SumWindow 0 size 0 v
           
data AvgWindow m a = AvgWindow Rational (SumWindow m a)

avgwindow_add :: Real a => Num a => PrimMonad m => AvgWindow m a -> a -> m (AvgWindow m a)
avgwindow_add (AvgWindow avg old_window) new_val = do
  new_window@(SumWindow sum size _ _) <- sumwindow_add old_window new_val
  return $ AvgWindow ((toRational sum) / (toRational size)) new_window
avgwindow_new :: Real a => Num a => PrimMonad m => Int -> m (AvgWindow m a)
avgwindow_new size = do
  w <- sumwindow_new size
  return $ AvgWindow 0 w
