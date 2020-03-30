module Gen where

import Prelude

import Control.Monad.State (State, modify)
import Data.Array (fromFoldable, sortWith)
import Data.Foldable (class Foldable)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Random.LCG (Seed, lcgNext, unSeed)

type Gen = State Seed

nextInt :: Gen Int
nextInt =
    unSeed <$> modify lcgNext

shuffle :: forall f a. Foldable f => f a -> Gen (Array a)
shuffle = fromFoldable >>> traverse f >>> map (sortWith snd >>> map fst)
    where f e = Tuple e <$> nextInt

