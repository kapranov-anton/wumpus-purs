module Game where

import Prelude

import Data.Array (head, take)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set, difference, fromFoldable, member, singleton)
import Edge (Edge(..))
import Gen (Gen, shuffle)
import Room (Room(..), roomList)


type Game =
    { wumpusRoom :: Room
    , wumpusAlive :: Boolean
    , pitRooms :: Set Room
    , batRooms :: Set Room
    , playerRoom :: Room
    , arrowTotal :: Int
    }

nonEmptyRooms :: Game -> Set Room
nonEmptyRooms {playerRoom, wumpusRoom, pitRooms, batRooms} =
    batRooms <> batRooms <> singleton playerRoom <> singleton wumpusRoom

init :: Gen (Maybe Game)
init = do
    rooms <- shuffle roomList
    let game = case take 6 rooms of
         [wumpusRoom, pit1, pit2, bat1, bat2, player] ->
             Just
                 { wumpusRoom: wumpusRoom
                 , wumpusAlive: true
                 , pitRooms: fromFoldable [pit1, pit2]
                 , batRooms: fromFoldable [bat1, bat2]
                 , playerRoom: player
                 , arrowTotal: 5
                 }
         _ -> Nothing
    pure game

vacantRoom :: Game -> Gen (Maybe Room)
vacantRoom = nonEmptyRooms >>> difference roomList >>> shuffle >>> map head

onMove :: Room -> Game -> Gen Game
onMove r g =
    if member r g.batRooms then
        vacantRoom g # map (fromMaybe r >>> g {playerRoom = _})
    else
        pure $ g { playerRoom = r }

onShoot :: Room -> Game -> Gen Game
onShoot r g =
    if r == g.wumpusRoom then
        pure $ g { wumpusAlive = false }
    else
        vacantRoom g # map (fromMaybe g.wumpusRoom >>>
                            g { wumpusRoom = _
                              , arrowTotal = g.arrowTotal - 1 })

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

