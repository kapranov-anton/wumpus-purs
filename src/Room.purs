module Room where

import Prelude

import Data.Array (range)
import Data.Set (Set, fromFoldable)

newtype Room = Room Int
derive instance eqRoom :: Eq Room
derive instance ordRoom :: Ord Room
instance showRoom :: Show Room where
    show (Room x) = show x

roomList :: Set Room
roomList = fromFoldable $ Room <$> range 1 20

