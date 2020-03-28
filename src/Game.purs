module Game where

import Prelude

import Control.Monad.RWS (modify)
import Control.Monad.State (State)
import Data.Array (sortWith, take)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Set (Set, singleton, fromFoldable)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Edge (Edge(..))
import Random.LCG (Seed, lcgM, lcgNext, unSeed)
import Room (Room(..), roomList)


type Lang = Int
type Game =
    { lang :: Lang
    , map :: Set (Edge Room)
    , wumpusRoom :: Room
    , wumpusAlive :: Boolean
    , pitRooms :: Set Room
    , batRooms :: Set Room
    , playerRoom :: Room
    , arrowTotal :: Int
    }

nonEmptyRooms :: Game -> Set Room
nonEmptyRooms {playerRoom, wumpusRoom, pitRooms, batRooms} =
    batRooms <> batRooms <> singleton playerRoom <> singleton wumpusRoom

type Gen a = State Seed a

nextInt :: Gen Int
nextInt =
    unSeed <$> modify lcgNext

nextNumber :: Gen Number
nextNumber = (\x -> toNumber x / toNumber lcgM) <$> nextInt

arrayShuffle :: forall a. Array a -> Gen (Array a)
arrayShuffle = traverse f >>> map (sortWith snd >>> map fst)
    where f e = Tuple e <$> nextInt

init :: Lang -> Gen (Maybe Game)
init l = do
    rooms <- arrayShuffle roomList
    let game = case take 6 rooms of
         [wumpusRoom, pit1, pit2, bat1, bat2, player] ->
             Just
                 { lang: l
                 , map: gameMap
                 , wumpusRoom: wumpusRoom
                 , wumpusAlive: true
                 , pitRooms: fromFoldable [pit1, pit2]
                 , batRooms: fromFoldable [bat1, bat2]
                 , playerRoom: player
                 , arrowTotal: 5
                 }
         _ -> Nothing
    pure game

gameMap :: Set (Edge Room)
gameMap =
    fromFoldable $ (map Room) <$>
        [ Edge 1 2
        , Edge 2 3
        , Edge 3 4
        , Edge 4 5
        , Edge 5 1
        , Edge 6 7
        , Edge 7 8
        , Edge 8 9
        , Edge 9 10
        , Edge 10 11
        , Edge 11 12
        , Edge 12 13
        , Edge 13 14
        , Edge 14 15
        , Edge 15 6
        , Edge 16 17
        , Edge 17 18
        , Edge 18 19
        , Edge 19 20
        , Edge 20 16
        , Edge 1 6
        , Edge 2 8
        , Edge 3 10
        , Edge 4 12
        , Edge 5 14
        , Edge 7 16
        , Edge 9 17
        , Edge 11 18
        , Edge 13 19
        , Edge 15 20
        ]

