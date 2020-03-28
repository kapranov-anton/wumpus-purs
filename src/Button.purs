module Button where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Effect.Class (class MonadEffect)
import Effect.Random (random)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


type State = { magicNumber :: Maybe Number }

data Action = Toggle

initialState :: forall i. i -> State
initialState = const { magicNumber: Nothing }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
    let
        label = maybe "???" show state.magicNumber
     in
    HH.button
    [ HP.title label
    , HE.onClick (\_ -> Just Toggle)
    ]
    [ HH.text label ]


handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
    Toggle -> do
       newNumber <- H.liftEffect random
       H.put $ { magicNumber: Just newNumber }

component :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }

{-
    [Стрелять] [Идти]
    Стрелять -> [1] [2] [3]

    LOG...
-}
