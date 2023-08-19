module Language.Lake.Generator (allValues) where

import Control.Enumerable (global)
import Language.Lake.AST (AST)
import Language.Lake.AST.DeBruijn (Z)
import Test.Feat.Access (valuesWith)

allValues :: [(Integer, [AST Z])]
allValues = valuesWith global