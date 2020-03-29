module GUI where

import Prelude

import Control.Monad.State (runState)
import Data.Array (intercalate, fromFoldable)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Game (Game, adjacentRooms, onMove, onShoot)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lang (Lang, ru)
import PlayerCommand (PlayerCommand(..))
import Random.LCG (Seed)
import Room (Room)

type GUIState =
    { command :: Maybe PlayerCommand
    , lang :: Lang
    , game :: Game
    , seed :: Seed
    }

type InitParams = Tuple Seed Game

initialState :: InitParams -> GUIState
initialState (Tuple seed game) =
    { command: Nothing
    , lang: ru
    , game: game
    , seed: seed
    }

data Action
    = SelectCommand PlayerCommand
    | UnselectCommand
    | DoCommand PlayerCommand Room

getCommandLabel :: Lang -> PlayerCommand -> String
getCommandLabel lang = case _ of
    Shoot -> lang.shoot
    Move -> lang.move

renderCommandButton :: forall m. Lang -> PlayerCommand -> H.ComponentHTML Action () m
renderCommandButton lang command =
    let
        label = getCommandLabel lang command
     in
    HH.button
        [ HP.title label
        , HE.onClick (\_ -> Just $ SelectCommand command)
        ]
        [ HH.text label ]

renderDirectionButton :: forall m. PlayerCommand -> Room -> H.ComponentHTML Action () m
renderDirectionButton c room =
    HH.button
        [ HP.title $ show room
        , HE.onClick (\_ -> Just $ DoCommand c room)
        ]
        [ HH.text $ show room]


renderSelectedCommand :: forall m. Lang -> Game -> PlayerCommand -> H.ComponentHTML Action () m
renderSelectedCommand l game c =
    HH.p []
        [ HH.text $ getCommandLabel l c
        , HH.button
            [ HP.title l.cancelCommand
            , HE.onClick (\_ -> Just $ UnselectCommand)
            ]
            [ HH.text "×"]
        , HH.span [] (adjacentRooms game # fromFoldable # map (renderDirectionButton c))
        ]

renderStatus :: forall m. Lang -> Game -> H.ComponentHTML Action () m
renderStatus lang game =
    let
        adjacent = adjacentRooms game # fromFoldable # map show # intercalate ", "
     in
    HH.div []
        [ HH.p [] [ HH.text $ lang.youAreHere <> " " <> show game.playerRoom ]
        , HH.p [] [ HH.text $ lang.youSeeRooms <> " " <> adjacent ]
        , HH.p [] [ HH.text $ lang.arrows <> ": " <> show game.arrowTotal ]
        ]

render :: forall m. GUIState -> H.ComponentHTML Action () m
render state =
    let
        commandButtons =
            HH.div []
                [ renderCommandButton state.lang Shoot
                , renderCommandButton state.lang Move
                ]
        renderedCommand =
            maybe commandButtons (renderSelectedCommand state.lang state.game) state.command
     in
    HH.div []
        [ renderStatus state.lang state.game
        , renderedCommand
        ]


handleAction ∷ forall o m. Action -> H.HalogenM GUIState Action () o m Unit
handleAction = case _ of
  SelectCommand c ->
    H.modify_ _ { command = Just c }
  UnselectCommand ->
    H.modify_ _ { command = Nothing }
  DoCommand Move r ->
    H.modify_ \s ->
        let
            Tuple newGame newSeed = runState (onMove r s.game) s.seed
         in
            s { command = Nothing, game = newGame, seed = newSeed }
  DoCommand Shoot r ->
    H.modify_ \s ->
        let
            Tuple newGame newSeed = runState (onShoot r s.game) s.seed
         in
            s { command = Nothing, game = newGame, seed = newSeed }

component :: forall q o m. H.Component HH.HTML q InitParams o m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }
