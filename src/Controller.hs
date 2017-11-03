-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Level
import Enemy

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step _ gstate@MainMenu = return $ gstate
step _ gstate@(WonScreen _) = return $ gstate
step _ gstate@(DiedScreen _) = return $ gstate
step _ gstate@LevelChooser = return $ gstate
step _ gstate   | isPaused gstate = return $ gstate
                | otherwise = if levelComplete (pointList gstate)
                                then return $ WonScreen (score gstate)
                                else return $ moveEnemies (updateEnemyDirection (movePlayer (checkCurrentPosition gstate)))
                    
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
            validNewPos = checkNewPlayerPosition gstate newPos
            newPlayer = if validNewPos
                            then Player newPos _playerDir
                            else Player (oldXPos, oldYPos) _playerDir

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
            newEnemy e = if (checkNewEnemyPosition gstate e (newPos (fst (enemy e)) (snd (enemy e))))
                            then Enemy (newPos (fst (enemy e)) (snd (enemy e))) (snd (enemy e))
                            else Enemy (fst (enemy e)) (snd (enemy e))
            newEnemies = map newEnemy _enemies

checkNewPlayerPosition :: GameState -> Position -> Bool
checkNewPlayerPosition gstate (x, y) = case field of
                                    WallField   -> False
                                    _           -> True
    where   _level = level gstate
            _playerDir = playerDir (player gstate)
            (field, _) = if _playerDir == DirUp
                            then (_level !! floor y) !! round x
                            else if _playerDir == DirDown
                                then (_level !! ceiling y) !! round x
                                else if _playerDir == DirRight
                                    then (_level !! round y) !! ceiling x
                                    else (_level !! round y) !! floor x

checkNewEnemyPosition :: GameState -> Enemy -> Position -> Bool
checkNewEnemyPosition gstate e (x, y) = case field of
                                    WallField   -> False
                                    _           -> True
    where   _level = level gstate
            _dir = enemyDir e
            (field, _) = if _dir == DirUp
                            then (_level !! floor y) !! round x
                            else if _dir == DirDown
                                then (_level !! ceiling y) !! round x
                                else if _dir == DirRight
                                    then (_level !! round y) !! ceiling x
                                    else (_level !! round y) !! floor x
                        
checkCurrentPosition :: GameState -> GameState
checkCurrentPosition gs | index /= Nothing = gs { score = newscore, pointList = (delete (fromIntegral (round x), fromIntegral (round y)) _pointlist) }
                        | otherwise = gs
                        where (x, y)  = playerPos (player gs)
                              index = elemIndex (fromIntegral (round x), fromIntegral (round y)) _pointlist
                              _pointlist = pointList gs 
                              newscore   = score gs + 10

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'p') Down _ _) gstate
    | isPlaying gstate = pauseGame gstate
    | otherwise = unPauseGame gstate
inputKey (EventKey (Char 'w') _ _ _) gstate 
    | isPlaying gstate && playerDir (player gstate) /= DirDown = setPlayerDirectionToUp gstate
    | otherwise = gstate
inputKey (EventKey (Char 'a') _ _ _) gstate 
    | isPlaying gstate && playerDir (player gstate) /= DirRight = setPlayerDirectionToLeft gstate
    | otherwise = gstate
inputKey (EventKey (Char 's') _ _ _) gstate 
    | isPlaying gstate && playerDir (player gstate) /= DirUp = setPlayerDirectionToDown gstate
    | otherwise = gstate
inputKey (EventKey (Char 'd') _ _ _) gstate 
    | isPlaying gstate && playerDir (player gstate) /= DirLeft = setPlayerDirectionToRight gstate
    | otherwise = gstate
inputKey _ gstate = gstate -- Otherwise keep the same

setPlayerDirectionToUp :: GameState -> GameState
setPlayerDirectionToUp gstate = if (x - fromIntegral (floor x)) < 0.2 && checkUpperFieldFree (fromIntegral (floor x)) y
                                    then gstate { player = Player (newPlayerPos (fromIntegral (floor x)) y) DirUp }
                                    else if (x - fromIntegral (floor x)) > 0.8 && checkUpperFieldFree (fromIntegral (ceiling x)) y
                                        then gstate { player = Player (newPlayerPos (fromIntegral (ceiling x)) y) DirUp }
                                        else gstate
    where   _level = level gstate
            _player = player gstate
            _playerDir = playerDir _player
            (x, y) = playerPos _player
            newPlayerPos _x _y = (_x, _y)
            checkUpperFieldFree x y = checkNewPlayerPosition gstate (newPlayerPos x (y - 1))

setPlayerDirectionToDown :: GameState -> GameState
setPlayerDirectionToDown gstate = if (x - fromIntegral (floor x)) < 0.2 && checkLowerFieldFree (fromIntegral (floor x)) y
                                    then gstate { player = Player (newPlayerPos (fromIntegral (floor x)) y) DirDown }
                                    else if (x - fromIntegral (floor x)) > 0.8 && checkLowerFieldFree (fromIntegral (ceiling x)) y
                                        then gstate { player = Player (newPlayerPos (fromIntegral (ceiling x)) y) DirDown }
                                        else gstate
    where   _level = level gstate
            _player = player gstate
            _playerDir = playerDir _player
            (x, y) = playerPos _player
            newPlayerPos _x _y = (_x, _y)
            checkLowerFieldFree x y = checkNewPlayerPosition gstate (newPlayerPos x (y + 1))
            
setPlayerDirectionToRight :: GameState -> GameState
setPlayerDirectionToRight gstate = if (y - fromIntegral (floor y)) < 0.2 && checkRightFieldFree x (fromIntegral (floor y))
                                    then gstate { player = Player (newPlayerPos x (fromIntegral (floor y))) DirRight }
                                    else if (y - fromIntegral (floor y)) > 0.8 && checkRightFieldFree x (fromIntegral (ceiling y))
                                        then gstate { player = Player (newPlayerPos x (fromIntegral (ceiling y))) DirRight }
                                        else gstate
    where   _level = level gstate
            _player = player gstate
            _playerDir = playerDir _player
            (x, y) = playerPos _player
            newPlayerPos _x _y = (_x, _y)
            checkRightFieldFree x y = checkNewPlayerPosition gstate (newPlayerPos (x + 1) y)
            
setPlayerDirectionToLeft :: GameState -> GameState
setPlayerDirectionToLeft gstate = if (y - fromIntegral (floor y)) < 0.2 && checkLeftFieldFree x (fromIntegral (floor y))
                                    then gstate { player = Player (newPlayerPos x (fromIntegral (floor y))) DirLeft }
                                    else if (y - fromIntegral (floor y)) > 0.8 && checkLeftFieldFree x (fromIntegral (ceiling y))
                                        then gstate { player = Player (newPlayerPos x (fromIntegral (ceiling y))) DirLeft }
                                        else gstate
    where   _level = level gstate
            _player = player gstate
            _playerDir = playerDir _player
            (x, y) = playerPos _player
            newPlayerPos _x _y = (_x, _y)
            checkLeftFieldFree x y = checkNewPlayerPosition gstate (newPlayerPos (x - 1) y)
            
isPlaying :: GameState -> Bool
isPlaying (PlayingLevel _ _ _ _ _) = True
isPlaying _ = False

isPaused :: GameState -> Bool
isPaused (Paused _ _ _ _ _) = True
isPaused _ = False

pauseGame :: GameState -> GameState
pauseGame gstate = Paused _score _level _player _pointList _enemies
    where   _score = score gstate
            _level = level gstate
            _player = player gstate
            _pointList = pointList gstate
            _enemies = enemies gstate

unPauseGame :: GameState -> GameState
unPauseGame gstate = PlayingLevel _score _level _player _pointList _enemies
    where   _score = score gstate
            _level = level gstate
            _player = player gstate
            _pointList = pointList gstate
            _enemies = enemies gstate