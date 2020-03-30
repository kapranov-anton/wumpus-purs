module GUI where

import Prelude

import Control.Monad.State as MS
import Data.Array (any, filter, fromFoldable, intercalate, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set, intersection, isEmpty, member)
import Data.String (null)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Game (Game, GameOverCause(..), TurnResult(..), adjacentRooms, onMove, onShoot, isGameOver)
import Gen (Gen)
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
    , turnLog :: Array TurnResult
    }


type InitParams = Tuple Seed Game

initialState :: InitParams -> GUIState
initialState (Tuple seed game) =
    { command: Nothing
    , lang: ru
    , game: game
    , seed: seed
    , turnLog: []
    }

data Action
    = SelectCommand PlayerCommand
    | CancelCommand
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

renderSelectedCommand :: forall m. Lang -> Set Room -> PlayerCommand -> H.ComponentHTML Action () m
renderSelectedCommand l adjacent c =
    HH.div []
    [ HH.p []
        [ HH.text $ getCommandLabel l c
        , HH.button
            [ HP.title l.cancelCommand
            , HE.onClick (\_ -> Just $ CancelCommand)
            ]
            [ HH.text "×"]
        ]
    , HH.p [] (map (renderDirectionButton c) $ fromFoldable adjacent)
    ]

renderStatus :: forall m. Lang -> Set Room -> Game -> H.ComponentHTML Action () m
renderStatus lang adjacent game =
    let
        adjacentStr = adjacent # fromFoldable # map show # intercalate ", "
        hasBats = intersection adjacent game.batRooms # isEmpty # not
        hasPits = intersection adjacent game.pitRooms # isEmpty # not
        hasWumpus = member game.wumpusRoom adjacent
        messages =
            [ if (hasBats) then lang.batsNearby else ""
            , if (hasPits) then lang.pitNearby else ""
            , if (hasWumpus) then lang.wumpusNearby else ""
            ] # filter (not <<< null) # intercalate ", "
     in
    HH.div []
        [ HH.p [] [ HH.text $ lang.youAreHere <> " " <> show game.playerRoom ]
        , HH.p [] [ HH.text $ lang.youSeeRooms <> " " <> adjacentStr ]
        , HH.p [] [ HH.text $ lang.arrows <> ": " <> show game.arrowTotal ]
        , HH.p [] [ HH.text messages ]
        ]


renderCommandSelector :: forall m. GUIState -> H.ComponentHTML Action () m
renderCommandSelector state =
    let
        adjacent = adjacentRooms state.game
        commandButtons =
            HH.div []
                [ renderCommandButton state.lang Shoot
                , renderCommandButton state.lang Move
                ]
        renderedCommand =
            maybe commandButtons (renderSelectedCommand state.lang adjacent) state.command
     in
    HH.div []
        [ renderStatus state.lang adjacent state.game
        , renderedCommand
        ]

showLogEntry :: Lang -> TurnResult -> String
showLogEntry lang = case _ of
    GameOver Win -> lang.win
    GameOver KilledByWumpus -> lang.wumpusKill
    GameOver FellIntoAPit -> lang.fell
    GameOver OutOfArrows -> lang.outOfArrows
    Missed -> lang.missed
    MovedByBats { from, to } -> lang.batsMoveYou <> " " <> show from <> " → " <> show to
    MovedTo r -> lang.moveTo <> " " <> show r

render :: forall m. GUIState -> H.ComponentHTML Action () m
render state =
    let
        renderEntry e = HH.p [] [HH.text $ showLogEntry state.lang e ]
     in
    HH.div []
        [ if (any isGameOver state.turnLog)
              then HH.text ""
              else renderCommandSelector state
        , HH.hr_
        , HH.div [] (renderEntry <$> state.turnLog)
        ]


runGen :: forall m. MS.MonadState GUIState m => (Game -> Gen (Game /\ TurnResult)) -> m Unit
runGen f =
    MS.modify_ \s ->
    let Tuple (newGame /\ tr) newSeed = MS.runState (f s.game) s.seed
     in s { game = newGame, seed = newSeed, turnLog = tr : s.turnLog }

cancelCommand :: forall o m. H.HalogenM GUIState Action () o m Unit
cancelCommand = H.modify_ \s -> s { command = Nothing }

handleAction ∷ forall o m. Action -> H.HalogenM GUIState Action () o m Unit
handleAction = case _ of
    SelectCommand c ->
        H.modify_ _ { command = Just c }
    CancelCommand ->
        cancelCommand
    DoCommand Move r ->
        runGen (onMove r) *> cancelCommand
    DoCommand Shoot r ->
        runGen (onShoot r) *> cancelCommand


component :: forall q o m. H.Component HH.HTML q InitParams o m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }
