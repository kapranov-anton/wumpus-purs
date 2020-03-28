module Gen where

import Prelude

import Control.Monad.RWS (modify)
import Control.Monad.State (StateT)
import Data.Array (fromFoldable, sortWith)
import Data.Foldable (class Foldable)
import Data.Identity (Identity)
import Data.Int (toNumber)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Random.LCG (Seed, lcgM, lcgNext, unSeed)

type GenF m = StateT Seed m
type Gen = GenF Identity

nextInt :: Gen Int
nextInt =
    unSeed <$> modify lcgNext

nextNumber :: Gen Number
nextNumber = (\x -> toNumber x / toNumber lcgM) <$> nextInt

shuffle :: forall f a. Foldable f => f a -> Gen (Array a)
shuffle = fromFoldable >>> traverse f >>> map (sortWith snd >>> map fst)
    where f e = Tuple e <$> nextInt

