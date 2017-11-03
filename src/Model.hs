-- | This module contains the data types
--   which represent the state of the game
module Model where
  
import Level 

data InfoToShow = ShowNothing
                | ShowLevel
                  deriving (Eq)

data GameState = 
      PlayingLevel {
          infoToShow :: InfoToShow,
          level :: Level,
          player :: Player,
          pointList :: Points,
          enemies :: [Enemy] } 
      | MainMenu 
      | LevelChooser 
      | DiedScreen 
      | WonScreen 
      | Paused {
        infoToShow :: InfoToShow,
        level :: Level,
        player :: Player,
        pointList :: Points,
        enemies :: [Enemy] }
        deriving (Eq)

initialState :: GameState
initialState = PlayingLevel ShowNothing initialLevel Player { playerPos = initialPlayerPos, playerDir = DirNone} initialPointList []

initialLevel :: Level
initialLevel = undefined

initialPlayerPos :: Position
initialPlayerPos = undefined

initialPointList :: Points
initialPointList = undefined

playerVelocity :: Float
playerVelocity = 0.04

enemyVelocity :: Float
enemyVelocity = 0.01