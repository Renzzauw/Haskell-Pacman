-- | This module contains the data types
--   which represent the state of the game
module Model where
  
data Direction = DirUp | DirDown | DirLeft | DirRight | DirNone
  
data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , direction   :: Direction
                 }

initialState :: GameState
initialState = GameState ShowNothing 0 DirNone