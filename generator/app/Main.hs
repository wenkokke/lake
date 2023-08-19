import Data.Foldable (for_)
import Language.Lake.Generator (allValues)

main :: IO ()
main = do
  for_ allValues $ \(_size, values) ->
    for_ values $ \value ->
      print value
