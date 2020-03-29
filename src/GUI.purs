module GUI where

import Prelude

import Control.Monad.State as MS
import Data.Array (filter, fromFoldable, intercalate)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Set (Set, intersection, isEmpty, member)
import Data.String (null)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Game (Game, adjacentRooms, onMove, onShoot)
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
    HH.p []
        [ HH.text $ getCommandLabel l c
        , HH.button
            [ HP.title l.cancelCommand
            , HE.onClick (\_ -> Just $ CancelCommand)
            ]
            [ HH.text "×"]
        , HH.span [] (map (renderDirectionButton c) $ fromFoldable adjacent)
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

deathCause :: Lang -> Game -> Maybe String
deathCause lang game
    | game.wumpusRoom == game.playerRoom = Just lang.wumpusKill
    | member game.playerRoom game.pitRooms = Just lang.fell
    | game.arrowTotal == 0 = Just lang.outOfArrows
    | otherwise = Nothing

render :: forall m. GUIState -> H.ComponentHTML Action () m
render state =
    let
        _ = spy ">>>" state
        death = deathCause state.lang state.game
     in
    if isJust death then
        HH.p [] [ HH.text $ fromMaybe "" death ]
    else if not state.game.wumpusAlive then
        HH.p [] [ HH.text state.lang.win ]
    else
        renderCommandSelector state


runGen :: forall m. MS.MonadState GUIState m => (Game -> Gen Game) -> m Unit
runGen f =
    MS.modify_ \s ->
    let Tuple newGame newSeed = MS.runState (f s.game) s.seed
     in s { game = newGame, seed = newSeed }

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
