module Edge where

import Prelude

import Data.Maybe (Maybe(..))

data Edge a = Edge a a

derive instance eqEdge :: Eq a => Eq (Edge a)
derive instance ordEdge :: Ord a => Ord (Edge a)

instance functorEdge :: Functor Edge where
    map f (Edge x y) = Edge (f x) (f y)

linked :: forall a. Eq a => a -> Edge a -> Maybe a
linked r (Edge r1 r2)
    | r == r1 = Just r2
    | r == r2 = Just r1
    | otherwise = Nothing
