module Language.Lake.Generator
  ( Generator (size, count),
    emptyGenerator,
    newGenerator,
    getValue,
    hasNext,
    getNext,
  )
where

import Control.Applicative (Alternative (..))
import Control.Enumerable (global)
import Data.Maybe (isJust, listToMaybe)
import Language.Lake.AST (AST)
import Language.Lake.AST.DeBruijn (Z)
import Test.Feat.Access (valuesWith)

data Generator = Generator
  { size :: Int,
    count :: Integer,
    values :: [AST Z],
    nextData :: [(Integer, [AST Z])]
  }

emptyGenerator :: Generator
emptyGenerator =
  Generator
    { size = 0,
      count = 0,
      values = [],
      nextData = []
    }

newGenerator :: Generator
newGenerator = emptyGenerator {nextData = valuesWith global}

getValue :: Generator -> Maybe (AST Z)
getValue generator = listToMaybe (values generator)

hasNext :: Generator -> Bool
hasNext = isJust . getNext

getNext :: Generator -> Maybe Generator
getNext generator = getNextValue generator <|> getNextSize generator

getNextValue :: Generator -> Maybe Generator
getNextValue generator = case values generator of
  [] -> Nothing
  (_ : rest) ->
    Just $
      generator
        { count = count generator - 1,
          values = rest
        }

getNextSize :: Generator -> Maybe Generator
getNextSize generator = case nextData generator of
  [] -> Nothing
  ((nextCount, nextValues) : nextNextData) ->
    Just $
      Generator
        { size = size generator + 1,
          count = nextCount,
          values = nextValues,
          nextData = nextNextData
        }
