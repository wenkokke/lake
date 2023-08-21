module Language.Lake.Generator
  ( Generator,
    empty,
    new,
    value,
    hasNext,
    next,
  )
where

import Control.Enumerable (global)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Language.Lake.AST (AST)
import Language.Lake.AST.DeBruijn (Z)
import Test.Feat.Access (valuesWith)

data Generator = Generator
  { size :: Int,
    count :: Integer,
    values :: [AST Z],
    nextData :: [(Integer, [AST Z])]
  }

empty :: Generator
empty =
  Generator
    { size = 0,
      count = 0,
      values = [],
      nextData = []
    }

new :: Generator
new = fromMaybe empty (next generator)
  where
    generator = empty {nextData = valuesWith global}

value :: Generator -> Maybe (AST Z)
value generator = listToMaybe (values generator)

hasNext :: Generator -> Bool
hasNext = isJust . next

next :: Generator -> Maybe Generator
next = nextValue

nextValue :: Generator -> Maybe Generator
nextValue generator = case values generator of
  [] -> nextSize generator
  [_] -> nextSize generator
  (_ : rest) ->
    Just $
      generator
        { count = count generator - 1,
          values = rest
        }

nextSize :: Generator -> Maybe Generator
nextSize generator = case nextData generator of
  [] -> Nothing
  ((nextCount, nextValues) : nextNextData) ->
    Just $
      Generator
        { size = size generator + 1,
          count = nextCount,
          values = nextValues,
          nextData = nextNextData
        }
