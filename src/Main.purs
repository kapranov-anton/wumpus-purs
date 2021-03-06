module Main where

import Prelude

import Control.Monad.State (runState)
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (error)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import GUI as GUI
import Game (Game, init)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Random.LCG (Seed, randomSeed)


initGame :: Effect (Tuple Seed Game)
initGame = do
    initSeed <- randomSeed
    let
        Tuple maybeGame seed = runState init initSeed
        cannotInitGame = throwException (error "Cannot initialize a game")
    Tuple seed <$> maybe cannotInitGame pure maybeGame

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  params <- liftEffect initGame
  runUI GUI.component params body
