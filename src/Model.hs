-- | This module contains the data types
--   which represent the state of the game
module Model where
  
import Level 

import System.Random

-- Data type for each state the game is currently in
data GameState = 
      PlayingLevel {
          score :: Int,
          level :: Level,
          player :: Player,
          player2 :: Maybe Player,
          pointList :: Points,
          enemies :: [Enemy],
          powerUp :: PowerUp,
          availablePowerUps :: [PowerUp],
          passedTime :: Float,
          rng :: StdGen,
          frame :: Int } 
      | MainMenu 
      | LevelChooser {
          levels :: [FilePath] }
      | DiedScreen {
          score :: Int }
      | WonScreen {
          score :: Int }
      | Player1WonScreen {
          score :: Int }
      | Player2WonScreen {
          score :: Int }
      | Paused {
          score :: Int,
          level :: Level,
          player :: Player,
          player2 :: Maybe Player,
          pointList :: Points,
          enemies :: [Enemy],
          powerUp :: PowerUp,
          availablePowerUps :: [PowerUp],
          passedTime :: Float,
          rng :: StdGen,
          frame :: Int }
      | HelpScreen
      | ControlsScreen
        deriving (Eq)

data PowerUp = PowerUp { 
    puType :: PowerUpType, 
    duration :: Float, 
    position :: Position }
        deriving (Eq)
    
data PowerUpType = SpeedUp | Invincible | InvertedEnemies | NoPowerUp
    deriving (Eq, Enum)
   
instance Eq StdGen where
        a == b = False
        a /= b = True
    
initialState :: FilePath -> IO GameState
initialState filePath = do
    (level, points, player, player2, enemies) <- loadLevel ("Levels/" ++ filePath)
    rng <- newStdGen
    let state = PlayingLevel 0 level player player2 points enemies powerUp [] 0 rng 1
    return $ state
    where   powerUp = PowerUp NoPowerUp 0 (0, 0)

levelChooserState :: IO GameState
levelChooserState = do
    _levels <- getLevels
    let state = LevelChooser _levels
    return $ state

playerVelocity :: Float
playerVelocity = 0.04

enemyVelocity :: Float
enemyVelocity = 0.042