{-# LANGUAGE InstanceSigs #-}

module Language.Lake.AST where

import Control.Enumerable (Enumerable (..), Shared, Sized (..), c1, c2, share)
import Data.Data (Typeable)
import Language.Lake.AST.DeBruijn (S (..))

data AST n
  = Var n
  | Lam (AST (S n))
  | App (AST n) (AST n)
  deriving (Typeable, Show)

-- Instances of 'Enumerable'

instance (Enumerable n) => Enumerable (AST n) where
  enumerate :: (Enumerable n, Typeable f, Sized f) => Shared f (AST n)
  enumerate =
    share $
      aconcat
        [ c1 Var,
          pay (c1 Lam),
          pay (c2 App)
        ]
