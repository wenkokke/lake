{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Lake.AST.DeBruijn
  ( Z,
    absurd,
    S (..),
    IsNatural (..),
  )
where

import Control.Enumerable (Enumerable (..), Shared, Sized (..), c0, c1, datatype, share)
import Data.Data (Typeable)
import Numeric.Natural (Natural)

data Z
  deriving (Typeable, Show)

absurd :: Z -> a
absurd n = n `seq` error "instance of empty type Z"

data S n
  = FZ
  | FS n
  deriving (Typeable, Show)

-- Instances of 'Enumerable'

instance Enumerable Z where
  enumerate :: (Typeable f, Sized f) => Shared f Z
  enumerate = datatype []

instance (Enumerable n) => Enumerable (S n) where
  enumerate :: (Enumerable n, Typeable f, Sized f) => Shared f (S n)
  enumerate = share $ aconcat [c0 FZ, c1 FS]

-- Instances of 'IsNatural'

class IsNatural n where
  toNatural :: n -> Natural

instance IsNatural Z where
  toNatural :: Z -> Natural
  toNatural = absurd

instance (IsNatural n) => IsNatural (S n) where
  toNatural :: (IsNatural n) => S n -> Natural
  toNatural FZ = 0
  toNatural (FS n) = succ (toNatural n)
