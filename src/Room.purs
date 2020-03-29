module Room where

import Prelude

import Data.Array ((..))
import Data.Set (Set, fromFoldable)

newtype Room = Room Int
derive instance eqRoom :: Eq Room
derive instance ordRoom :: Ord Room
instance showRoom :: Show Room where
    show (Room x) = show x

roomSet :: Set Room
roomSet = fromFoldable $ Room <$> 1 .. 20

