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

playerVelocity :: Float
playerVelocity = 0.075

enemyVelocity :: Float
enemyVelocity = 0.03

-- Renzo toegevoegd 
getFieldType :: GameState -> Position -> FieldType
getFieldType gs pos = fst ((currlevel !! y) !! x)
                 where x = toInt (fst pos)
                       y = toInt (snd pos)
                       currlevel = level gs

toInt :: Float -> Int
toInt = round