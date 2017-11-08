-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Level
import Enemy

import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List
import Data.Maybe
import Data.Char

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(PlayingLevel _ _ _ _player2 _ _ _ _ _ _ _)    | isNothing _player2 = if levelComplete (pointList updatedGameState)
                                                                                        then return $ WonScreen (score updatedGameState)
                                                                                        else if not (null (isPlayerDead updatedGameState)) && not (isEatEnemies (puType (powerUp updatedGameState)))
                                                                                            then return $ DiedScreen (score updatedGameState)
                                                                                            else if not (null (isPlayerDead updatedGameState))
                                                                                                    then updateRNG (moveEnemies (updateEnemyDirection (movePlayers (checkCurrentPosition (deleteEnemies updatedGameState (isPlayerDead updatedGameState))))))
                                                                                                    else updateRNG (moveEnemies (updateEnemyDirection (movePlayers (checkCurrentPosition updatedGameState))))
                                                                | otherwise = if levelComplete (pointList updatedGameState)
                                                                                then return $ Player1WonScreen (score updatedGameState)
                                                                                else if (not (null (isPlayerDead updatedGameState)) || checkPlayer2Won updatedGameState) && not (isEatEnemies (puType (powerUp updatedGameState)))
                                                                                    then return $ Player2WonScreen (score updatedGameState)
                                                                                    else if (not (null (isPlayerDead updatedGameState)) || checkPlayer2Won updatedGameState)
                                                                                        then updateRNG (moveEnemies (updateEnemyDirection (movePlayers (checkCurrentPosition (deleteEnemies updatedGameState (isPlayerDead updatedGameState))))))
                                                                                        else updateRNG (moveEnemies (updateEnemyDirection (movePlayers (checkCurrentPosition updatedGameState))))
                                        where   updatedGameState = checkForNewPowerUps (createNewPowerUps (deleteOldPowerUps (updatePowerUp (updateFrameGState (updateTimeGState secs gstate)))))
step secs gstate = return $ updateTimeGState secs gstate

updateTimeGState :: Float -> GameState -> GameState
updateTimeGState time gstate@PlayingLevel {} = gstate { passedTime = passedTime gstate + time }
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
    return newState

deleteEnemies :: GameState -> [Int] -> GameState
deleteEnemies gstate indices = gstate { enemies = newEnemies }
    where   _enemies = enemies gstate
            newEnemies = deleteAtIndices indices _enemies

deleteAtIndices :: [Int] -> [Enemy] -> [Enemy]
deleteAtIndices _ [] = []
deleteAtIndices [] list = list
deleteAtIndices list list2 = deleteAtIndices (tail list) list3
    where   toDelete = list2 !! (head list)
            list3 = delete toDelete list2

checkPlayer2Won :: GameState -> Bool
checkPlayer2Won gstate = checkPos
    where   (p1x, p1y) = playerPos (player gstate)
            (p2x, p2y) = playerPos (fromJust (player2 gstate))
            checkPos    | abs (p1x - p2x) < 0.3 && abs (p1y - p2y) < 0.3 = True
                        | otherwise = False

deleteOldPowerUps :: GameState -> GameState
deleteOldPowerUps gstate = gstate { availablePowerUps = foldr ((++) . checkDuration) [] powerUps }
    where   secs = passedTime gstate
            powerUps = availablePowerUps gstate
            checkDuration pu = [pu | duration pu > secs]

createNewPowerUps :: GameState -> GameState
createNewPowerUps gstate    | chance < 0.005 && field /= WallField && notElem newPowerUp (availablePowerUps gstate) = gstate { availablePowerUps = availablePowerUps gstate ++ [newPowerUp] }
                            | otherwise = gstate
    where   random1 = rng gstate
            (chance, random2) = randomR (0 :: Float, 1 :: Float) random1
            (_powerUp, random3) = randomR (1 :: Int, 3 :: Int) random2
            (xPos, random4) = randomR (0 :: Int, levelWidth - 1) random3
            (yPos, random5) = randomR (0 :: Int, levelHeight - 1) random4
            (randomDuration, _) = randomR (10 :: Float, 30 :: Float) random5
            _level = level gstate
            (field, pos) = _level !! yPos !! xPos
            levelWidth = length (head _level)
            levelHeight = length _level
            newPowerUp = PowerUp (types !! (_powerUp - 1)) (randomDuration + secs) pos
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
checkForNewPowerUps gstate  | isNothing index = gstate
                            | otherwise = gstate { powerUp = (newPowerUp (fromJust index)) { duration = randomDuration + secs }, availablePowerUps = delete (newPowerUp (fromJust index)) _availablePowerUps }
    where   (x, y) = playerPos (player gstate)
            _availablePowerUps = availablePowerUps gstate
            _positions = map position _availablePowerUps
            (randomDuration, _) = randomR (5 :: Float, 15 :: Float) (rng gstate)
            index = elemIndex (fromInteger (round x), fromInteger (round y)) _positions
            newPowerUp i = _availablePowerUps !! i
            secs = passedTime gstate

updateEnemyDirection :: GameState -> GameState
updateEnemyDirection gstate = gstate { enemies = newEnemies }
    where   _enemies = enemies gstate
            enemy e = (enemyPos e, enemyDir e)
            _playerPos = playerPos (player gstate)
            newDirAndPos (_enemyPos, _enemyDir) _enemyType = if isInvertedEnemies (puType (powerUp gstate))
                                                                then invertedDirection gstate _enemyDir _playerPos _enemyPos
                                                                else normalDirection (gstate { rng = snd (randomR (0 :: Float, 1 :: Float) (rng gstate)) }) _enemyDir _playerPos _enemyPos _enemyType
            newEnemy e = uncurry Enemy (newDirAndPos (enemy e) (enemyType e)) (enemyType e)
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
            newPlayer   | validNewPos && _playerDir /= DirNone = Player newPos _playerDir _playerDir
                        | validNewPos = Player newPos _playerDir (lastDir (player gstate))
                        | otherwise = Player (oldXPos, oldYPos) DirNone _playerDir

isSpeedUp :: PowerUpType -> Bool
isSpeedUp SpeedUp = True
isSpeedUp _ = False

isEatEnemies :: PowerUpType -> Bool
isEatEnemies EatEnemies = True
isEatEnemies _ = False

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
            newEnemy e = if checkNewEnemyPosition gstate e (uncurry newPos (enemy e))
                            then Enemy (uncurry newPos (enemy e)) (snd (enemy e)) (enemyType e)
                            else uncurry Enemy (enemy e) (enemyType e)
            newEnemies = map newEnemy _enemies

checkNewPlayerPosition :: GameState -> Player -> Position -> Bool
checkNewPlayerPosition gstate _player (x, y) = case field of
                                    WallField   -> False
                                    _           -> True
    where   _level = level gstate
            _playerDir = playerDir _player
            (field, _)  | _playerDir == DirUp = (_level !! floor y) !! round x
                        | _playerDir == DirDown = (_level !! ceiling y) !! round x
                        | _playerDir == DirRight = (_level !! round y) !! ceiling x
                        | otherwise = (_level !! round y) !! floor x

checkNewEnemyPosition :: GameState -> Enemy -> Position -> Bool
checkNewEnemyPosition gstate e (x, y) = case field of
                                    WallField   -> False
                                    _           -> True
    where   _level = level gstate
            _dir = enemyDir e
            (field, _)  | _dir == DirUp = (_level !! floor y) !! round x
                        | _dir == DirDown = (_level !! ceiling y) !! round x
                        | _dir == DirRight = (_level !! round y) !! ceiling x
                        | otherwise = (_level !! round y) !! floor x
                        
checkCurrentPosition :: GameState -> GameState
checkCurrentPosition gs | isJust index = gs { score = newscore index, pointList = delete ((fromInteger (round x), fromInteger (round y)), _bool index) _pointlist }
                        | otherwise = gs
                        where (x, y)  = playerPos (player gs)
                              index = elemIndex (fromInteger (round x), fromInteger (round y)) (map fst _pointlist)
                              _pointlist = pointList gs 
                              newscore _index   | _bool _index = score gs + 25
                                                | otherwise = score gs + 10
                              _bool i = snd (_pointlist !! fromJust i)

-- | Handle user input
input :: Event -> GameState -> IO GameState
input (EventKey (SpecialKey KeyEnter) Down _ _) MainMenu = levelChooserState
input e@(EventKey (Char c) Down _ _) gstate@(LevelChooser levelList) = if isDigit c && length levelList >= number
                                                                            then initialState (levelList !! (number - 1))
                                                                            else return (inputKey e gstate)
                                                                 where   number = digitToInt c
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) _ = MainMenu
inputKey (EventKey (Char 'c') Down _ _) (LevelChooser _) = ControlsScreen
inputKey (EventKey (Char 'h') Down _ _) (LevelChooser _) = HelpScreen
inputKey (EventKey (Char 'p') Down _ _) gstate
    | isPlaying gstate = pauseGame gstate
    | otherwise = unPauseGame gstate
inputKey (EventKey (Char 'w') _ _ _) gstate 
    | isPlaying gstate = setPlayerDirectionToUp gstate (player gstate) (playerDir (player gstate))
    | otherwise = gstate
inputKey (EventKey (Char 'a') _ _ _) gstate 
    | isPlaying gstate = setPlayerDirectionToLeft gstate (player gstate) (playerDir (player gstate))
    | otherwise = gstate
inputKey (EventKey (Char 's') _ _ _) gstate 
    | isPlaying gstate = setPlayerDirectionToDown gstate (player gstate) (playerDir (player gstate))
    | otherwise = gstate
inputKey (EventKey (Char 'd') _ _ _) gstate 
    | isPlaying gstate = setPlayerDirectionToRight gstate (player gstate) (playerDir (player gstate))
    | otherwise = gstate
inputKey (EventKey (SpecialKey KeyUp) _ _ _) gstate 
    | isPlaying gstate && isJust (player2 gstate) && not (isInvertedEnemies (puType (powerUp gstate))) = setPlayerDirectionToUp gstate (fromJust (player2 gstate)) (playerDir (fromJust (player2 gstate)))
    | isPlaying gstate && isJust (player2 gstate) && isInvertedEnemies (puType (powerUp gstate)) = setPlayerDirectionToDown gstate (fromJust (player2 gstate)) (playerDir (fromJust (player2 gstate)))
    | otherwise = gstate
inputKey (EventKey (SpecialKey KeyLeft) _ _ _) gstate 
    | isPlaying gstate && isJust (player2 gstate) && not (isInvertedEnemies (puType (powerUp gstate))) = setPlayerDirectionToLeft gstate (fromJust (player2 gstate)) (playerDir (fromJust (player2 gstate)))
    | isPlaying gstate && isJust (player2 gstate) && isInvertedEnemies (puType (powerUp gstate)) = setPlayerDirectionToRight gstate (fromJust (player2 gstate)) (playerDir (fromJust (player2 gstate)))
    | otherwise = gstate
inputKey (EventKey (SpecialKey KeyDown) _ _ _) gstate 
    | isPlaying gstate && isJust (player2 gstate) && not (isInvertedEnemies (puType (powerUp gstate))) = setPlayerDirectionToDown gstate (fromJust (player2 gstate)) (playerDir (fromJust (player2 gstate)))
    | isPlaying gstate && isJust (player2 gstate) && isInvertedEnemies (puType (powerUp gstate)) = setPlayerDirectionToUp gstate (fromJust (player2 gstate)) (playerDir (fromJust (player2 gstate)))
    | otherwise = gstate
inputKey (EventKey (SpecialKey KeyRight) _ _ _) gstate 
    | isPlaying gstate && isJust (player2 gstate) && not (isInvertedEnemies (puType (powerUp gstate))) = setPlayerDirectionToRight gstate (fromJust (player2 gstate)) (playerDir (fromJust (player2 gstate)))
    | isPlaying gstate && isJust (player2 gstate) && isInvertedEnemies (puType (powerUp gstate)) = setPlayerDirectionToLeft gstate (fromJust (player2 gstate)) (playerDir (fromJust (player2 gstate)))
    | otherwise = gstate
inputKey (EventKey (SpecialKey KeyEnter) Down _ _) (WonScreen _) = MainMenu
inputKey (EventKey (SpecialKey KeyEnter) Down _ _) (DiedScreen _) = MainMenu
inputKey (EventKey (SpecialKey KeyEnter) Down _ _) (Player1WonScreen _) = MainMenu
inputKey (EventKey (SpecialKey KeyEnter) Down _ _) (Player2WonScreen _) = MainMenu
inputKey _ gstate = gstate -- Otherwise keep the same

setPlayerDirectionToUp :: GameState -> Player -> Direction -> GameState
setPlayerDirectionToUp gstate _player _lastDir  | _player == player gstate = if (x - fromInteger (floor x)) < 0.2 && checkUpperFieldFree (fromInteger (floor x)) y
                                                                                then gstate { player = Player (newPlayerPos (fromInteger (floor x)) y) DirUp _lastDir }
                                                                                else if (x - fromInteger (floor x)) > 0.8 && checkUpperFieldFree (fromInteger (ceiling x)) y
                                                                                    then gstate { player = Player (newPlayerPos (fromInteger (ceiling x)) y) DirUp _lastDir }
                                                                                    else gstate
                                                | otherwise = if (x - fromInteger (floor x)) < 0.2 && checkUpperFieldFree (fromInteger (floor x)) y
                                                                    then gstate { player2 = Just (Player (newPlayerPos (fromInteger (floor x)) y) DirUp _lastDir) }
                                                                    else if (x - fromInteger (floor x)) > 0.8 && checkUpperFieldFree (fromInteger (ceiling x)) y
                                                                        then gstate { player2 = Just (Player (newPlayerPos (fromInteger (ceiling x)) y) DirUp _lastDir) }
                                                                        else gstate
    where   _level = level gstate
            (x, y) = playerPos _player
            newPlayerPos _x _y = (_x, _y)
            checkUpperFieldFree xCoor yCoor = checkNewPlayerPosition gstate _player (newPlayerPos xCoor (yCoor - 1))

setPlayerDirectionToDown :: GameState -> Player -> Direction -> GameState
setPlayerDirectionToDown gstate _player _lastDir    | _player == player gstate = if (x - fromInteger (floor x)) < 0.2 && checkLowerFieldFree (fromInteger (floor x)) y
                                                                                    then gstate { player = Player (newPlayerPos (fromInteger (floor x)) y) DirDown _lastDir }
                                                                                    else if (x - fromInteger (floor x)) > 0.8 && checkLowerFieldFree (fromInteger (ceiling x)) y
                                                                                        then gstate { player = Player (newPlayerPos (fromInteger (ceiling x)) y) DirDown _lastDir }
                                                                                        else gstate
                                                    | otherwise = if (x - fromInteger (floor x)) < 0.2 && checkLowerFieldFree (fromInteger (floor x)) y
                                                                        then gstate { player2 = Just (Player (newPlayerPos (fromInteger (floor x)) y) DirDown _lastDir) }
                                                                        else if (x - fromInteger (floor x)) > 0.8 && checkLowerFieldFree (fromInteger (ceiling x)) y
                                                                            then gstate { player2 = Just (Player (newPlayerPos (fromInteger (ceiling x)) y) DirDown _lastDir) }
                                                                            else gstate
    where   _level = level gstate
            (x, y) = playerPos _player
            newPlayerPos _x _y = (_x, _y)
            checkLowerFieldFree xCoor yCoor = checkNewPlayerPosition gstate _player (newPlayerPos xCoor (yCoor + 1))
            
setPlayerDirectionToRight :: GameState -> Player -> Direction -> GameState
setPlayerDirectionToRight gstate _player _lastDir   | _player == player gstate = if (y - fromInteger (floor y)) < 0.2 && checkRightFieldFree x (fromInteger (floor y))
                                                                                    then gstate { player = Player (newPlayerPos x (fromInteger (floor y))) DirRight _lastDir }
                                                                                    else if (y - fromInteger (floor y)) > 0.8 && checkRightFieldFree x (fromInteger (ceiling y))
                                                                                        then gstate { player = Player (newPlayerPos x (fromInteger (ceiling y))) DirRight _lastDir }
                                                                                        else gstate
                                                    | otherwise = if (y - fromInteger (floor y)) < 0.2 && checkRightFieldFree x (fromInteger (floor y))
                                                                        then gstate { player2 = Just (Player (newPlayerPos x (fromInteger (floor y))) DirRight _lastDir) }
                                                                        else if (y - fromInteger (floor y)) > 0.8 && checkRightFieldFree x (fromInteger (ceiling y))
                                                                            then gstate { player2 = Just (Player (newPlayerPos x (fromInteger (ceiling y))) DirRight _lastDir) }
                                                                            else gstate
    where   _level = level gstate
            (x, y) = playerPos _player
            newPlayerPos _x _y = (_x, _y)
            checkRightFieldFree xCoor yCoor = checkNewPlayerPosition gstate _player (newPlayerPos (xCoor + 1) yCoor)
            
setPlayerDirectionToLeft :: GameState -> Player -> Direction -> GameState
setPlayerDirectionToLeft gstate _player _lastDir    | _player == player gstate = if (y - fromInteger (floor y)) < 0.2 && checkLeftFieldFree x (fromInteger (floor y))
                                                                                    then gstate { player = Player (newPlayerPos x (fromInteger (floor y))) DirLeft _lastDir }
                                                                                    else if (y - fromInteger (floor y)) > 0.8 && checkLeftFieldFree x (fromInteger (ceiling y))
                                                                                        then gstate { player = Player (newPlayerPos x (fromInteger (ceiling y))) DirLeft _lastDir }
                                                                                        else gstate
                                                    | otherwise = if (y - fromInteger (floor y)) < 0.2 && checkLeftFieldFree x (fromInteger (floor y))
                                                                        then gstate { player2 = Just (Player (newPlayerPos x (fromInteger (floor y))) DirLeft _lastDir) }
                                                                        else if (y - fromInteger (floor y)) > 0.8 && checkLeftFieldFree x (fromInteger (ceiling y))
                                                                            then gstate { player2 = Just (Player (newPlayerPos x (fromInteger (ceiling y))) DirLeft _lastDir) }
                                                                            else gstate
    where   _level = level gstate
            (x, y) = playerPos _player
            newPlayerPos _x _y = (_x, _y)
            checkLeftFieldFree xCoor yCoor = checkNewPlayerPosition gstate _player (newPlayerPos (xCoor - 1) yCoor)
            
isPlaying :: GameState -> Bool
isPlaying PlayingLevel {} = True
isPlaying _ = False

isPaused :: GameState -> Bool
isPaused Paused {} = True
isPaused _ = False

pauseGame :: GameState -> GameState
pauseGame (PlayingLevel _score _level _player _player2 _pointList _enemies _powerUp _availablePowerUps _passedTime _rng _frame) = Paused _score _level _player _player2 _pointList _enemies _powerUp _availablePowerUps _passedTime _rng _frame
pauseGame gstate = gstate

unPauseGame :: GameState -> GameState
unPauseGame (Paused _score _level _player _player2 _pointList _enemies _powerUp _availablePowerUps _passedTime _rng _frame) = PlayingLevel _score _level _player _player2 _pointList _enemies _powerUp _availablePowerUps _passedTime _rng _frame
unPauseGame gstate = gstate