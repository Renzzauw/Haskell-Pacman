-- | This module contains the data types
--   which represent the state of the game
module Model where
  
import Level 

data InfoToShow = ShowNothing
                | ShowString  String

data GameState = GameState {
  infoToShow :: InfoToShow,
  level :: Level,
  initLevel :: Level,
  player :: Player,
  pointList :: Points
}

data GameMode = MainMenu | LevelChooser | DiedScreen | WonScreen | Paused | PlayingLevel

initialState :: GameState
initialState = GameState ShowNothing initialLevel initialLevel Player {pos = initialPlayerPos, dir = DirNone} initialPointList

initialLevel :: Level
initialLevel = undefined

initialPlayerPos :: Position
initialPlayerPos = undefined

initialPointList :: Points
initialPointList = undefined