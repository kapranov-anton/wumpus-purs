module Game where

import Prelude

import Data.Array (head, take)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set, difference, fromFoldable, mapMaybe, member, singleton)
import Data.Tuple.Nested ((/\), type (/\))
import Edge (Edge(..), linked)
import Gen (Gen, shuffle)
import Room (Room(..), roomSet)


type Game =
    { wumpusRoom :: Room
    , wumpusAlive :: Boolean
    , pitRooms :: Set Room
    , batRooms :: Set Room
    , playerRoom :: Room
    , arrowTotal :: Int
    }

data GameOverCause
    = Win
    | KilledByWumpus
    | FellIntoAPit
    | OutOfArrows

data TurnResult
    = GameOver GameOverCause
    | Missed
    | MovedByBats { from :: Room, to :: Room }
    | MovedTo Room

isGameOver :: TurnResult -> Boolean
isGameOver = case _ of
    GameOver _ -> true
    _ -> false

nonEmptyRooms :: Game -> Set Room
nonEmptyRooms {playerRoom, wumpusRoom, pitRooms, batRooms} =
    batRooms <> pitRooms <> singleton playerRoom <> singleton wumpusRoom

init :: Gen (Maybe Game)
init = do
    rooms <- shuffle roomSet
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

adjacentRooms :: Game -> Set Room
adjacentRooms game =
    mapMaybe (linked game.playerRoom) gameMap

vacantRoom :: Game -> Gen (Maybe Room)
vacantRoom = nonEmptyRooms >>> difference roomSet >>> shuffle >>> map head

onMove :: Room -> Game -> Gen (Game /\ TurnResult)
onMove r g =
    if member r g.batRooms then
        vacantRoom g # map \maybeRoom ->
            let
                newRoom = fromMaybe r maybeRoom
             in
             g { playerRoom = newRoom } /\ MovedByBats { from: r, to: newRoom }
    else
        let
            sideEffect =
                if r == g.wumpusRoom then
                    GameOver KilledByWumpus
                else if member r g.pitRooms then
                    GameOver FellIntoAPit
                else
                    MovedTo r
         in
        pure $ g { playerRoom = r } /\ sideEffect

onShoot :: Room -> Game -> Gen (Game /\ TurnResult)
onShoot r g =
    if r == g.wumpusRoom then
        pure $ g { wumpusAlive = false } /\ GameOver Win
    else
        let
            sideEffect = if g.arrowTotal > 1 then Missed else GameOver OutOfArrows
         in
        vacantRoom g # map \maybeRoom ->
            g { wumpusRoom = fromMaybe g.wumpusRoom maybeRoom
              , arrowTotal = g.arrowTotal - 1
              } /\ sideEffect

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

