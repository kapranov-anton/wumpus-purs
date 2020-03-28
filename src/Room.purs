module Room where

import Prelude
import Data.Array (range)

newtype Room = Room Int
derive instance eqRoom :: Eq Room
derive instance ordRoom :: Ord Room

roomList :: Array Room
roomList = Room <$> range 1 20

