module GUI where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Game (Game)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lang (Lang, ru)
import PlayerCommand (PlayerCommand(..))
import Random.LCG (Seed)

type GUIState =
    { command :: Maybe PlayerCommand
    , lang :: Lang
    , game :: Game
    }

type InitParams = Tuple Seed Game

initialState :: InitParams -> GUIState
initialState (Tuple seed game) =
    { command: Nothing
    , lang: ru
    , game: game
    }

data Action
    = SelectCommand PlayerCommand
    | UnselectCommand
    {--| SelectDirection Room--}

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

renderSelectedCommand :: forall m. Lang -> PlayerCommand -> H.ComponentHTML Action () m
renderSelectedCommand l c =
    HH.p []
        [ HH.text $ getCommandLabel l c
        , HH.button
            [ HP.title l.cancelCommand
            , HE.onClick (\_ -> Just $ UnselectCommand)
            ]
            [ HH.text "×"]
        ]

renderLocation :: forall m. Lang -> Game -> H.ComponentHTML Action () m
renderLocation lang game =
    HH.p [] [ HH.text $ lang.youAreHere <> show game.playerRoom ]

render :: forall m. GUIState -> H.ComponentHTML Action () m
render state =
    let
        commandButtons =
            HH.div []
                [ renderCommandButton state.lang Shoot
                , renderCommandButton state.lang Move
                ]
        renderedCommand =
            maybe commandButtons (renderSelectedCommand state.lang) state.command
     in
    HH.div []
        [ renderLocation state.lang state.game
        , renderedCommand
        ]


handleAction ∷ forall o m. Action -> H.HalogenM GUIState Action () o m Unit
handleAction = case _ of
  SelectCommand c ->
    H.modify_ \st -> st { command = Just c }
  UnselectCommand ->
    H.modify_ \st -> st { command = Nothing }

component :: forall q o m. H.Component HH.HTML q InitParams o m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }
