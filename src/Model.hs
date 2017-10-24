-- | This module contains the data types
--   which represent the state of the game
module Model where
  
import Level 

data Direction = DirUp | DirDown | DirLeft | DirRight | DirNone

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowString  String

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1

data GameState = GameState {
  infoToShow :: InfoToShow,
  level :: Level,
  initLevel :: Level,
  playerPos :: (Float, Float),
  playerDir :: Direction,
  pointList :: Points
}

data GameMode = MainMenu | LevelChooser | DiedScreen | WonScreen | Paused | PlayingLevel

initialState :: GameState
initialState = GameState ShowNothing initialLevel initialLevel initialPlayerPos DirNone initialPointList

initialLevel :: Level
initialLevel = undefined

initialPlayerPos :: (Float, Float)
initialPlayerPos = undefined

initialPointList :: Points
initialPointList = undefined