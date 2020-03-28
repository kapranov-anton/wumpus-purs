module PlayerCommand where

import Data.String (toLower, trim)

data PlayerCommand
  = Move
  | Shoot
  | Quit
  | Unknown

fromString :: String -> PlayerCommand
fromString s = case toLower (trim s) of
    "m" -> Move
    "s" -> Shoot
    "q" -> Quit
    _   -> Unknown

