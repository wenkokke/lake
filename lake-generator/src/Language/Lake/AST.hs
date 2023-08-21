{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Lake.AST
  ( AST (..),
  )
where

import Control.Enumerable (Enumerable (..), Shared, Sized (..), c1, c2, share)
import Data.Aeson (Value)
import Data.Aeson.Types (Encoding, Options (..), SumEncoding (..), ToJSON (..), defaultOptions, genericToEncoding, genericToJSON)
import Data.Data (Typeable)
import GHC.Generics (Generic)
import Language.Lake.AST.DeBruijn (IsNatural, S (..))

data AST n
  = Var n
  | Lam (AST (S n))
  | App (AST n) (AST n)
  deriving (Generic, Typeable, Show)

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

-- Instances of 'ToJSON'

options :: Options
options =
  defaultOptions
    { sumEncoding = ObjectWithSingleField,
      tagSingleConstructors = True
    }

instance (IsNatural n, ToJSON n) => ToJSON (AST n) where
  toJSON :: (IsNatural n, ToJSON n) => AST n -> Value
  toJSON = genericToJSON options

  toEncoding :: (IsNatural n, ToJSON n) => AST n -> Encoding
  toEncoding = genericToEncoding options
