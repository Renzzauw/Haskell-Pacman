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
import Data.Maybe
import Data.Char

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(PlayingLevel _ _ _ player2 _ _ _ _ _ _ _) | isNothing player2 = if levelComplete (pointList updatedGameState)
                                                                                    then return $ WonScreen (score updatedGameState)
                                                                                    else if isPlayerDead updatedGameState && isInvincible (puType (powerUp updatedGameState)) == False
                                                                                        then return $ DiedScreen (score updatedGameState)
                                                                                        else updateRNG (moveEnemies (updateEnemyDirection (movePlayers (checkCurrentPosition updatedGameState))))
                                                            | otherwise = if levelComplete (pointList updatedGameState)
                                                                            then return $ Player1WonScreen (score updatedGameState)
                                                                            else if (isPlayerDead updatedGameState || checkPlayer2Won updatedGameState) && isInvincible (puType (powerUp updatedGameState)) == False
                                                                                then return $ Player2WonScreen (score updatedGameState)
                                                                                else updateRNG (moveEnemies (updateEnemyDirection (movePlayers (checkCurrentPosition updatedGameState))))
                                        where   updatedGameState = checkForNewPowerUps (createNewPowerUps (deleteOldPowerUps (updatePowerUp (updateFrameGState (updateTimeGState secs gstate)))))
step secs gstate = return $ updateTimeGState secs gstate

updateTimeGState :: Float -> GameState -> GameState
updateTimeGState time gstate@(PlayingLevel _ _ _ _ _ _ _ _ _ _ _) = gstate { passedTime = (passedTime gstate + time) }
updateTimeGState _ gstate = gstate

updateFrameGState :: GameState -> GameState
updateFrameGState gstate = gstate { frame = newFrame }
    where   _frame = frame gstate
            frameIncrement = _frame + 1
            newFrame = if frameIncrement > 60
                            then frameIncrement - 60
                            else frameIncrement

updateRNG :: GameState -> IO GameState
updateRNG gstate = do
    newRng <- newStdGen
    let newState = gstate { rng = newRng }
    return $ newState

checkPlayer2Won :: GameState -> Bool
checkPlayer2Won gstate = checkPos
    where   (p1x, p1y) = playerPos (player gstate)
            (p2x, p2y) = playerPos (fromJust (player2 gstate))
            checkPos    | abs (p1x - p2x) < 0.3 && abs (p1y - p2y) < 0.3 = True
                        | otherwise = False

deleteOldPowerUps :: GameState -> GameState
deleteOldPowerUps gstate = gstate { availablePowerUps = foldr (++) [] (map checkDuration powerUps) }
    where   secs = passedTime gstate
            powerUps = availablePowerUps gstate
            checkDuration pu = if duration pu > secs
                                    then [pu]
                                    else []

createNewPowerUps :: GameState -> GameState
createNewPowerUps gstate    | chance < 0.005 && field /= WallField && elem newPowerUp (availablePowerUps gstate) == False = gstate { availablePowerUps = availablePowerUps gstate ++ [newPowerUp] }
                            | otherwise = gstate
    where   random1 = rng gstate
            (chance, random2) = randomR (0 :: Float, 1 :: Float) random1
            (powerUp, random3) = randomR (1 :: Int, 3 :: Int) random2
            (xPos, random4) = randomR (0 :: Int, (levelWidth - 1)) random3
            (yPos, random5) = randomR (0 :: Int, (levelHeight - 1)) random4
            (randomDuration, random6) = randomR (10 :: Float, 30 :: Float) random5
            _level = level gstate
            (field, pos) = _level !! yPos !! xPos
            levelWidth = length (head _level)
            levelHeight = length _level
            newPowerUp = PowerUp (types !! (powerUp - 1)) (randomDuration + secs) pos
            types = init [SpeedUp ..]
            secs = passedTime gstate

updatePowerUp :: GameState -> GameState
updatePowerUp gstate    | puType _powerUp /= NoPowerUp = if duration _powerUp <= secs
                                                    then gstate { powerUp = PowerUp NoPowerUp 0 (0, 0) }
                                                    else gstate
                        | otherwise = gstate
    where   _powerUp = powerUp gstate
            secs = passedTime gstate

checkForNewPowerUps :: GameState -> GameState
checkForNewPowerUps gstate  | index == Nothing = gstate
                            | otherwise = gstate { powerUp = (newPowerUp (fromJust index)) { duration = randomDuration + secs }, availablePowerUps = delete (newPowerUp (fromJust index)) _availablePowerUps }
    where   (x, y) = playerPos (player gstate)
            _availablePowerUps = availablePowerUps gstate
            _positions = map position _availablePowerUps
            (randomDuration, _) = randomR (5 :: Float, 20 :: Float) (rng gstate)
            index = elemIndex (fromIntegral (round x), fromIntegral (round y)) _positions
            newPowerUp i = _availablePowerUps !! i
            secs = passedTime gstate

updateEnemyDirection :: GameState -> GameState
updateEnemyDirection gstate = gstate { enemies = newEnemies }
    where   _enemies = enemies gstate
            enemy e = (enemyPos e, enemyDir e)
            _playerPos = playerPos (player gstate)
            newDirAndPos (enemyPos, enemyDir) enemyType = if isInvertedEnemies (puType (powerUp gstate))
                                                                then invertedDirection gstate enemyDir _playerPos enemyPos
                                                                else normalDirection gstate enemyDir _playerPos enemyPos enemyType
            newEnemy e = Enemy (fst (newDirAndPos (enemy e) (enemyType e))) (snd (newDirAndPos (enemy e) (enemyType e))) (enemyType e)
            newEnemies = map newEnemy _enemies

movePlayers :: GameState -> GameState
movePlayers gstate  | player2Exists = movePlayer _player2 p1
                    | otherwise = p1
    where   p1 = movePlayer (player gstate) gstate
            player2Exists   | isNothing (player2 gstate) = False
                            | otherwise = True
            _player2 = fromJust (player2 gstate)

movePlayer :: Player -> GameState -> GameState
movePlayer _player gstate   | _player == player gstate = gstate { player = newPlayer }
                            | otherwise = gstate { player2 = Just newPlayer }
    where   _playerDir = playerDir _player
            (oldXPos, oldYPos) = playerPos _player
            usedVelocityP1  | isSpeedUp (puType (powerUp gstate)) = playerVelocity * 1.2
                            | otherwise = playerVelocity
            usedVelocityP2 = enemyVelocity
            usedVelocity    | _player == player gstate = usedVelocityP1
                            | otherwise = usedVelocityP2
            newPos = case _playerDir of
                DirUp       ->  (oldXPos, oldYPos - usedVelocity)
                DirDown     ->  (oldXPos, oldYPos + usedVelocity)
                DirRight    ->  (oldXPos + usedVelocity, oldYPos)
                DirLeft     ->  (oldXPos - usedVelocity, oldYPos)
                _           ->  (oldXPos, oldYPos)
            validNewPos = checkNewPlayerPosition gstate _player newPos
            newPlayer = if validNewPos
                            then Player newPos _playerDir
                            else Player (oldXPos, oldYPos) _playerDir

isSpeedUp :: PowerUpType -> Bool
isSpeedUp SpeedUp = True
isSpeedUp _ = False

isInvincible :: PowerUpType -> Bool
isInvincible Invincible = True
isInvincible _ = False

isInvertedEnemies :: PowerUpType -> Bool
isInvertedEnemies InvertedEnemies = True
isInvertedEnemies _ = False

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
                            then Enemy (newPos (fst (enemy e)) (snd (enemy e))) (snd (enemy e)) (enemyType e)
                            else Enemy (fst (enemy e)) (snd (enemy e)) (enemyType e)
            newEnemies = map newEnemy _enemies

checkNewPlayerPosition :: GameState -> Player -> Position -> Bool
checkNewPlayerPosition gstate player (x, y) = case field of
                                    WallField   -> False
                                    _           -> True
    where   _level = level gstate
            _playerDir = playerDir player
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
checkCurrentPosition gs | index /= Nothing = gs { score = newscore index, pointList = (delete ((fromIntegral (round x), fromIntegral (round y)), _bool index) _pointlist) }
                        | otherwise = gs
                        where (x, y)  = playerPos (player gs)
                              index = elemIndex (fromIntegral (round x), fromIntegral (round y)) (map fst _pointlist)
                              _pointlist = pointList gs 
                              newscore index    | _bool index = score gs + 25
                                                | otherwise = score gs + 10
                              _bool index = snd (_pointlist !! (fromJust index))

-- | Handle user input
input :: Event -> GameState -> IO GameState
input (EventKey (SpecialKey KeyEnter) Down _ _) MainMenu = levelChooserState
input e@(EventKey (Char c) Down _ _) gstate@(LevelChooser levelList) = if isDigit c && length levelList >= number
                                                                            then initialState (levelList !! (number - 1))
                                                                            else return (inputKey e gstate)
                                                                 where   number = digitToInt c
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate = MainMenu
inputKey (EventKey (Char 'c') Down _ _) (LevelChooser _) = ControlsScreen
inputKey (EventKey (Char 'h') Down _ _) (LevelChooser _) = HelpScreen
inputKey (EventKey (Char 'p') Down _ _) gstate
    | isPlaying gstate = pauseGame gstate
    | otherwise = unPauseGame gstate
inputKey (EventKey (Char 'w') _ _ _) gstate 
    | isPlaying gstate = setPlayerDirectionToUp gstate (player gstate)
    | otherwise = gstate
inputKey (EventKey (Char 'a') _ _ _) gstate 
    | isPlaying gstate = setPlayerDirectionToLeft gstate (player gstate)
    | otherwise = gstate
inputKey (EventKey (Char 's') _ _ _) gstate 
    | isPlaying gstate = setPlayerDirectionToDown gstate (player gstate)
    | otherwise = gstate
inputKey (EventKey (Char 'd') _ _ _) gstate 
    | isPlaying gstate = setPlayerDirectionToRight gstate (player gstate)
    | otherwise = gstate
inputKey (EventKey (SpecialKey KeyUp) _ _ _) gstate 
    | isPlaying gstate && isJust (player2 gstate) && isInvertedEnemies (puType (powerUp gstate)) == False = setPlayerDirectionToUp gstate (fromJust (player2 gstate))
    | isPlaying gstate && isJust (player2 gstate) && isInvertedEnemies (puType (powerUp gstate)) == True = setPlayerDirectionToDown gstate (fromJust (player2 gstate))
    | otherwise = gstate
inputKey (EventKey (SpecialKey KeyLeft) _ _ _) gstate 
    | isPlaying gstate && isJust (player2 gstate) && isInvertedEnemies (puType (powerUp gstate)) == False = setPlayerDirectionToLeft gstate (fromJust (player2 gstate))
    | isPlaying gstate && isJust (player2 gstate) && isInvertedEnemies (puType (powerUp gstate)) == True = setPlayerDirectionToRight gstate (fromJust (player2 gstate))
    | otherwise = gstate
inputKey (EventKey (SpecialKey KeyDown) _ _ _) gstate 
    | isPlaying gstate && isJust (player2 gstate) && isInvertedEnemies (puType (powerUp gstate)) == False = setPlayerDirectionToDown gstate (fromJust (player2 gstate))
    | isPlaying gstate && isJust (player2 gstate) && isInvertedEnemies (puType (powerUp gstate)) == True = setPlayerDirectionToUp gstate (fromJust (player2 gstate))
    | otherwise = gstate
inputKey (EventKey (SpecialKey KeyRight) _ _ _) gstate 
    | isPlaying gstate && isJust (player2 gstate) && isInvertedEnemies (puType (powerUp gstate)) == False = setPlayerDirectionToRight gstate (fromJust (player2 gstate))
    | isPlaying gstate && isJust (player2 gstate) && isInvertedEnemies (puType (powerUp gstate)) == True = setPlayerDirectionToLeft gstate (fromJust (player2 gstate))
    | otherwise = gstate
inputKey (EventKey (SpecialKey KeyEnter) Down _ _) (WonScreen _) = MainMenu
inputKey (EventKey (SpecialKey KeyEnter) Down _ _) (DiedScreen _) = MainMenu
inputKey (EventKey (SpecialKey KeyEnter) Down _ _) (Player1WonScreen _) = MainMenu
inputKey (EventKey (SpecialKey KeyEnter) Down _ _) (Player2WonScreen _) = MainMenu
inputKey _ gstate = gstate -- Otherwise keep the same

setPlayerDirectionToUp :: GameState -> Player -> GameState
setPlayerDirectionToUp gstate _player   | _player == player gstate = if (x - fromIntegral (floor x)) < 0.2 && checkUpperFieldFree (fromIntegral (floor x)) y
                                                                        then gstate { player = Player (newPlayerPos (fromIntegral (floor x)) y) DirUp }
                                                                        else if (x - fromIntegral (floor x)) > 0.8 && checkUpperFieldFree (fromIntegral (ceiling x)) y
                                                                            then gstate { player = Player (newPlayerPos (fromIntegral (ceiling x)) y) DirUp }
                                                                            else gstate
                                        | otherwise = if (x - fromIntegral (floor x)) < 0.2 && checkUpperFieldFree (fromIntegral (floor x)) y
                                                            then gstate { player2 = Just (Player (newPlayerPos (fromIntegral (floor x)) y) DirUp) }
                                                            else if (x - fromIntegral (floor x)) > 0.8 && checkUpperFieldFree (fromIntegral (ceiling x)) y
                                                                then gstate { player2 = Just (Player (newPlayerPos (fromIntegral (ceiling x)) y) DirUp) }
                                                                else gstate
    where   _level = level gstate
            _playerDir = playerDir _player
            (x, y) = playerPos _player
            newPlayerPos _x _y = (_x, _y)
            checkUpperFieldFree x y = checkNewPlayerPosition gstate _player (newPlayerPos x (y - 1))

setPlayerDirectionToDown :: GameState -> Player -> GameState
setPlayerDirectionToDown gstate _player     | _player == player gstate = if (x - fromIntegral (floor x)) < 0.2 && checkLowerFieldFree (fromIntegral (floor x)) y
                                                                            then gstate { player = Player (newPlayerPos (fromIntegral (floor x)) y) DirDown }
                                                                            else if (x - fromIntegral (floor x)) > 0.8 && checkLowerFieldFree (fromIntegral (ceiling x)) y
                                                                                then gstate { player = Player (newPlayerPos (fromIntegral (ceiling x)) y) DirDown }
                                                                                else gstate
                                            | otherwise = if (x - fromIntegral (floor x)) < 0.2 && checkLowerFieldFree (fromIntegral (floor x)) y
                                                                then gstate { player2 = Just (Player (newPlayerPos (fromIntegral (floor x)) y) DirDown) }
                                                                else if (x - fromIntegral (floor x)) > 0.8 && checkLowerFieldFree (fromIntegral (ceiling x)) y
                                                                    then gstate { player2 = Just (Player (newPlayerPos (fromIntegral (ceiling x)) y) DirDown) }
                                                                    else gstate
    where   _level = level gstate
            _playerDir = playerDir _player
            (x, y) = playerPos _player
            newPlayerPos _x _y = (_x, _y)
            checkLowerFieldFree x y = checkNewPlayerPosition gstate _player (newPlayerPos x (y + 1))
            
setPlayerDirectionToRight :: GameState -> Player -> GameState
setPlayerDirectionToRight gstate _player    | _player == player gstate = if (y - fromIntegral (floor y)) < 0.2 && checkRightFieldFree x (fromIntegral (floor y))
                                                                            then gstate { player = Player (newPlayerPos x (fromIntegral (floor y))) DirRight }
                                                                            else if (y - fromIntegral (floor y)) > 0.8 && checkRightFieldFree x (fromIntegral (ceiling y))
                                                                                then gstate { player = Player (newPlayerPos x (fromIntegral (ceiling y))) DirRight }
                                                                                else gstate
                                            | otherwise = if (y - fromIntegral (floor y)) < 0.2 && checkRightFieldFree x (fromIntegral (floor y))
                                                                then gstate { player2 = Just (Player (newPlayerPos x (fromIntegral (floor y))) DirRight) }
                                                                else if (y - fromIntegral (floor y)) > 0.8 && checkRightFieldFree x (fromIntegral (ceiling y))
                                                                    then gstate { player2 = Just (Player (newPlayerPos x (fromIntegral (ceiling y))) DirRight) }
                                                                    else gstate
    where   _level = level gstate
            _playerDir = playerDir _player
            (x, y) = playerPos _player
            newPlayerPos _x _y = (_x, _y)
            checkRightFieldFree x y = checkNewPlayerPosition gstate _player (newPlayerPos (x + 1) y)
            
setPlayerDirectionToLeft :: GameState -> Player -> GameState
setPlayerDirectionToLeft gstate _player     | _player == player gstate = if (y - fromIntegral (floor y)) < 0.2 && checkLeftFieldFree x (fromIntegral (floor y))
                                                                            then gstate { player = Player (newPlayerPos x (fromIntegral (floor y))) DirLeft }
                                                                            else if (y - fromIntegral (floor y)) > 0.8 && checkLeftFieldFree x (fromIntegral (ceiling y))
                                                                                then gstate { player = Player (newPlayerPos x (fromIntegral (ceiling y))) DirLeft }
                                                                                else gstate
                                            | otherwise = if (y - fromIntegral (floor y)) < 0.2 && checkLeftFieldFree x (fromIntegral (floor y))
                                                                then gstate { player2 = Just (Player (newPlayerPos x (fromIntegral (floor y))) DirLeft) }
                                                                else if (y - fromIntegral (floor y)) > 0.8 && checkLeftFieldFree x (fromIntegral (ceiling y))
                                                                    then gstate { player2 = Just (Player (newPlayerPos x (fromIntegral (ceiling y))) DirLeft) }
                                                                    else gstate
    where   _level = level gstate
            _playerDir = playerDir _player
            (x, y) = playerPos _player
            newPlayerPos _x _y = (_x, _y)
            checkLeftFieldFree x y = checkNewPlayerPosition gstate _player (newPlayerPos (x - 1) y)
            
isPlaying :: GameState -> Bool
isPlaying (PlayingLevel _ _ _ _ _ _ _ _ _ _ _) = True
isPlaying _ = False

isPaused :: GameState -> Bool
isPaused (Paused _ _ _ _ _ _ _ _ _ _ _) = True
isPaused _ = False

pauseGame :: GameState -> GameState
pauseGame gstate = Paused _score _level _player _player2 _pointList _enemies _powerUp _availablePowerUps _passedTime _rng _frame
    where   _score = score gstate
            _level = level gstate
            _player = player gstate
            _player2 = player2 gstate
            _pointList = pointList gstate
            _enemies = enemies gstate
            _powerUp = powerUp gstate
            _availablePowerUps = availablePowerUps gstate
            _passedTime = passedTime gstate
            _rng = rng gstate
            _frame = frame gstate

unPauseGame :: GameState -> GameState
unPauseGame gstate = PlayingLevel _score _level _player _player2 _pointList _enemies _powerUp _availablePowerUps _passedTime _rng _frame
    where   _score = score gstate
            _level = level gstate
            _player = player gstate
            _player2 = player2 gstate
            _pointList = pointList gstate
            _enemies = enemies gstate
            _powerUp = powerUp gstate
            _availablePowerUps = availablePowerUps gstate
            _passedTime = passedTime gstate
            _rng = rng gstate
            _frame = frame gstate