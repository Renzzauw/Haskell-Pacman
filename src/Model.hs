-- | This module contains the data types
--   which represent the state of the game
module Model where
  
import Level 

data InfoToShow = ShowNothing
                | ShowString  String
                  deriving (Eq)

data GameState = 
      PlayingLevel {
          infoToShow :: InfoToShow,
          initLevel :: Level,
          player :: Player,
          pointList :: Points,
          enemies :: [Enemy] } 
      | MainMenu 
      | LevelChooser 
      | DiedScreen 
      | WonScreen 
      | Paused
        deriving (Eq)

initialState :: GameState
initialState = PlayingLevel ShowNothing initialLevel Player { playerPos = initialPlayerPos, playerDir = DirNone} initialPointList []

initialLevel :: Level
initialLevel = undefined

initialPlayerPos :: Position
initialPlayerPos = undefined

initialPointList :: Points
initialPointList = undefined