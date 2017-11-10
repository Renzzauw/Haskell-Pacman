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
          frame :: Int,
          activeAnimations :: [Animation] } 
      | MainMenu 
      | LevelChooser {
          levels :: [FilePath] }
      | DiedScreen {
          score :: Int,
          updatedHighScore :: Bool }
      | WonScreen {
          score :: Int,
          updatedHighScore :: Bool }
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
          frame :: Int,
          activeAnimations :: [Animation] }
      | HelpScreen
      | ControlsScreen

-- Data type for a powerup
data PowerUp = PowerUp { 
    puType :: PowerUpType, 
    duration :: Float, 
    position :: Position }
        deriving (Eq)
    
-- The different types of powerups in the game
data PowerUpType = SpeedUp | EatEnemies | InvertedEnemies | NoPowerUp
    deriving (Eq, Enum)

-- Data type for an animation
data Animation = Animation {
    animationType :: AnimationType,
    animPos :: Position,
    startTime :: Float,
    stopTime :: Float }

-- The different types of animations in the game
data AnimationType = RedEnemyDied | BlueEnemyDied | Trigger
    deriving (Eq)

-- Function that creates the initial of a level based on its filepath
initialState :: FilePath -> IO GameState
initialState filePath = do
    (_level, _points, _player, _player2, _enemies) <- loadLevel ("Levels/" ++ filePath)
    _rng <- newStdGen
    let state = PlayingLevel 0 _level _player _player2 _points _enemies _powerUp [] 0 _rng 1 []
    return state
    where   _powerUp = PowerUp NoPowerUp 0 (0, 0)

-- GameState for choosing the levels, IO because it has to get the directory-contents of folder Levels
levelChooserState :: IO GameState
levelChooserState = do
    _levels <- getLevels
    let state = LevelChooser _levels
    return state

-- Constances for the player- and enemyvelocities
playerVelocity :: Float
playerVelocity = 0.04

enemyVelocity :: Float
enemyVelocity = 0.042