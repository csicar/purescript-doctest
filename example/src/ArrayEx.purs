module ArrayEx where

--| fromFoldable
--| ```purescript run
--| > import Data.Array
--| > import Data.Maybe (Maybe(..))
--| > noInt :: Maybe Int
--|   noInt = Nothing
--| > fromFoldable (Just 1)
--| [1]
--| > fromFoldable (Nothing :: Maybe Int) -- Needed bacause PS needs to figure out what Maybe a is
--| []
--| ```
--| singleton
--| ```purescript run
--| > singleton 2
--| [2]
--| ```
--| ```purescript
--| > 2 .. 5
--| [2, 3, 4, 5]
--| ```
-- |
main = 2