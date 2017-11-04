-- | This module contains the data types
--   which represent the state of the game
module Model where
  
import Level 

-- Data type for each state the game is currently in
data GameState = 
      PlayingLevel {
          score :: Int,
          level :: Level,
          player :: Player,
          pointList :: Points,
          enemies :: [Enemy],
          powerUp :: PowerUp,
          availablePowerUps :: [PowerUp],
          passedTime :: Float } 
      | MainMenu 
      | LevelChooser {
          levels :: [FilePath] }
      | DiedScreen {
          score :: Int }
      | WonScreen {
          score :: Int }
      | Paused {
          score :: Int,
          level :: Level,
          player :: Player,
          pointList :: Points,
          enemies :: [Enemy],
          powerUp :: PowerUp,
          availablePowerUps :: [PowerUp],
          passedTime :: Float }
      | HelpScreen
      | ControlsScreen
        deriving (Eq)

data PowerUp = 
    SpeedUp { 
        duration :: Float,
        position :: Position }
    | Invincible {
        duration :: Float,
        position :: Position }
    | InvertedEnemies {
        duration :: Float,
        position :: Position }
    | NoPowerUp
        deriving (Eq)
        
initialState :: FilePath -> IO GameState
initialState filePath = do
    (level, points, player, enemies) <- loadLevel ("Levels/" ++ filePath)
    let state = PlayingLevel 0 level player points enemies NoPowerUp [InvertedEnemies 5 (18, 24)] 0
    return $ state

levelChooserState :: IO GameState
levelChooserState = do
    _levels <- getLevels
    let state = LevelChooser _levels
    return $ state

playerVelocity :: Float
playerVelocity = 0.04

enemyVelocity :: Float
enemyVelocity = 0.042