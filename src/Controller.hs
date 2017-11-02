-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Level
import Enemy

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
    | gstate == MainMenu = return $ gstate
    | gstate == WonScreen = return $ gstate
    | gstate == DiedScreen = return $ gstate
    | gstate == LevelChooser = return $ gstate
    | isPaused gstate = return $ gstate
    | otherwise = return $ moveEnemies (updateEnemyDirection (movePlayer gstate))

updateEnemyDirection :: GameState -> GameState
updateEnemyDirection gstate = gstate { enemies = newEnemies }
    where   _enemies = enemies gstate
            enemy e = (enemyPos e, enemyDir e)
            _playerPos = playerPos (player gstate)
            newDir enemyPos = lookForPlayer gstate _playerPos enemyPos
            newEnemy e = Enemy ((fst (enemy e))) (newDir (fst (enemy e)))
            newEnemies = map newEnemy _enemies

movePlayer :: GameState -> GameState
movePlayer gstate = gstate { player = newPlayer }
    where   _player = player gstate
            _playerDir = playerDir _player
            (oldXPos, oldYPos) = playerPos _player
            newPos = case _playerDir of
                DirUp       ->  (oldXPos, oldYPos - playerVelocity)
                DirDown     ->  (oldXPos, oldYPos + playerVelocity)
                DirRight    ->  (oldXPos + playerVelocity, oldYPos)
                DirLeft     ->  (oldXPos - playerVelocity, oldYPos)
                _           ->  (oldXPos, oldYPos)
            newPlayer = Player newPos _playerDir

moveEnemies :: GameState -> GameState
moveEnemies gstate = gstate { enemies = newEnemies }
    where   _enemies = enemies gstate
            enemy e = (enemyPos e, enemyDir e)
            newPos (oldXPos, oldYPos) dir = case dir of
                DirUp       ->  (oldXPos, oldYPos - enemyVelocity)
                DirDown     ->  (oldXPos, oldYPos + enemyVelocity)
                DirRight    ->  (oldXPos + enemyVelocity, oldYPos)
                DirLeft     ->  (oldXPos - enemyVelocity, oldYPos)
                _           ->  (oldXPos, oldYPos)
            newEnemy e = Enemy (newPos (fst (enemy e)) (snd (enemy e))) (snd (enemy e))
            newEnemies = map newEnemy _enemies

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'p') Down _ _) gstate
    | isPlaying gstate = pauseGame gstate
    | otherwise = unPauseGame gstate
inputKey (EventKey (Char 'w') _ _ _) gstate 
    | isPlaying gstate && playerDir (player gstate) /= DirDown = gstate { player = (player gstate) { playerDir = DirUp } }
    | otherwise = gstate
inputKey (EventKey (Char 'a') _ _ _) gstate 
    | isPlaying gstate && playerDir (player gstate) /= DirRight = gstate { player = (player gstate) { playerDir = DirLeft } }
    | otherwise = gstate
inputKey (EventKey (Char 's') _ _ _) gstate 
    | isPlaying gstate && playerDir (player gstate) /= DirUp = gstate { player = (player gstate) { playerDir = DirDown } }
    | otherwise = gstate
inputKey (EventKey (Char 'd') _ _ _) gstate 
    | isPlaying gstate && playerDir (player gstate) /= DirLeft = gstate { player = (player gstate) { playerDir = DirRight } }
    | otherwise = gstate
inputKey (EventKey (Char 'l') _ _ _) gstate 
    | isPlaying gstate = gstate { infoToShow = ShowLevel }
    | otherwise = gstate
inputKey _ gstate = gstate -- Otherwise keep the same

isPlaying :: GameState -> Bool
isPlaying (PlayingLevel _ _ _ _ _) = True
isPlaying _ = False

isPaused :: GameState -> Bool
isPaused (Paused _ _ _ _ _) = True
isPaused _ = False

pauseGame :: GameState -> GameState
pauseGame gstate = Paused _infoToShow _level _player _pointList _enemies
    where   _infoToShow = infoToShow gstate
            _level = level gstate
            _player = player gstate
            _pointList = pointList gstate
            _enemies = enemies gstate

unPauseGame :: GameState -> GameState
unPauseGame gstate = PlayingLevel _infoToShow _level _player _pointList _enemies
    where   _infoToShow = infoToShow gstate
            _level = level gstate
            _player = player gstate
            _pointList = pointList gstate
            _enemies = enemies gstate