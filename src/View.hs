-- | This module defines how to turn
--   the game state into a picture
module View where

import Model
import Level
import Images

import Data.Maybe
import Graphics.Gloss

-- Draw loop fro drawing images on the game window
view :: GameState -> IO Picture
view = return . viewPure

-- Show the right screen with the given GameState
viewPure :: GameState -> Picture
viewPure MainMenu = mainmenuBackground
viewPure gstate@(WonScreen _) = drawWonScreen gstate
viewPure gstate@(DiedScreen _) = drawDiedScreen gstate
viewPure (Player1WonScreen _) = head playerWins
viewPure (Player2WonScreen _) = playerWins !! 1
viewPure gstate@(LevelChooser _) = drawLevelChooser gstate
viewPure Paused {} = pauseScreen
viewPure HelpScreen = levelTutScreen
viewPure ControlsScreen = controlsScreen
viewPure gstate = pictures (drawLevel gstate : [drawScore gstate])

-- Draw the score of the player on the screen
drawScore :: GameState -> Picture
drawScore gs = translate (-600) 200 (scale 0.2 0.2 (color yellow (text ("Score: " ++ show _score))))
        where _score = score gs

-- Draw the proper text when the player completes the level          
drawWonScreen :: GameState -> Picture
drawWonScreen gs = pictures [head playerWins, translate (-300) (-120) (scale 0.6 0.6 (color yellow (text ("You scored: " ++ show _score))))]
        where _score = score gs

-- Draw the proper text when the player dies
drawDiedScreen :: GameState -> Picture
drawDiedScreen gs = pictures [emptyBackground, translate (-300) 50 (scale 0.7 0.7 (color red (text "You Died!"))), translate (-300) (-50) (scale 0.6 0.6 (color yellow (text ("You scored: " ++ show _score))))]
        where _score = score gs

-- Draw the proper text when the player has to choose a level
drawLevelChooser :: GameState -> Picture
drawLevelChooser gs = pictures (levelSelectScreen : [translate (-600) (-fromIntegral amountOfLevels * 35 - 25) (pictures (translateLoadLevelText amountOfLevels (map createPicture numbers)))])
    where   list = levels gs
            amountOfLevels = length list
            numbers = [1..amountOfLevels]
            createText number = "Press \'" ++ show number ++ "\' to load " ++ list !! (number - 1)
            createPicture n = scale 0.4 0.4 (color green (text (createText n)))

-- Translate the texts for each level the player can choose
translateLoadLevelText :: Int -> [Picture] -> [Picture]
translateLoadLevelText 0 p = p
translateLoadLevelText _ [] = []
translateLoadLevelText n (p:ps) = translate 0 (70 * fromIntegral n) p : translateLoadLevelText (n - 1) ps

-- Function that draws each Field of a given level
drawLevel :: GameState -> Picture
drawLevel gstate = translatedLevel
    where   _level = level gstate
            levelWidth = length (head _level)
            levelHeight = length _level
            totalLevel = pictures (map (pictures . map drawTile) _level)
            startList   | isNothing (player2 gstate) = []
                        | otherwise = [drawPlayer2 gstate]
            includingPacmanAndPoints = pictures (totalLevel : drawPoints gstate : drawAnimations gstate : drawEnemies gstate : drawPowerUps gstate : drawPacman gstate : startList)
            translatedLevel = pictures [gameBackground, translate (0.5 * fromIntegral (-spriteSize * levelWidth)) (-0.5 * fromIntegral (-spriteSize * levelHeight)) includingPacmanAndPoints]

-- Function that draws tiles
drawTile :: Field -> Picture
drawTile (WallField, (xPos, yPos)) = translate (xPos * fromIntegral spriteSize) (-yPos * fromIntegral spriteSize) wallTile
drawTile (_, (xPos, yPos)) = translate (xPos * fromIntegral spriteSize) (-yPos * fromIntegral spriteSize) emptyTile

-- Function that draws the player
drawPacman :: GameState -> Picture
drawPacman gstate = translatedPacman
    where   rotatedPacman = sprite
            translatedPacman = translate (xPos * fromIntegral spriteSize) (-yPos * fromIntegral spriteSize) rotatedPacman 
            (xPos, yPos) = playerPos (player gstate)
            currframe   = frame gstate
            _playerDir = playerDir (player gstate)
            sprite      | _playerDir == DirNone = rotate (calculateRotation (lastDir (player gstate))) pacmanIdle
                        | otherwise = rotate (calculateRotation _playerDir) (calculateAnimationFrame currframe 2 pacmanSeq)

-- Function that draws player 2 as a green ghost
drawPlayer2 :: GameState -> Picture
drawPlayer2 gstate = translatedPlayer
        where   sprite  | _powerUp == InvertedEnemies && _dir == DirUp = calculateAnimationFrame _frame 3 invertedGhostMovingUp
                        | _powerUp == InvertedEnemies && _dir == DirDown = calculateAnimationFrame _frame 3 invertedGhostMovingDown
                        | _powerUp == InvertedEnemies && _dir == DirLeft = calculateAnimationFrame _frame 3 invertedGhostMovingLeft
                        | _powerUp == InvertedEnemies && _dir == DirRight = calculateAnimationFrame _frame 3 invertedGhostMovingRight
                        | _powerUp == InvertedEnemies = invertedGhostIdle
                        | _dir == DirUp = calculateAnimationFrame _frame 3 greenGhostMovingUp
                        | _dir == DirDown = calculateAnimationFrame _frame 3 greenGhostMovingDown
                        | _dir == DirLeft = calculateAnimationFrame _frame 3 greenGhostMovingLeft
                        | _dir == DirRight = calculateAnimationFrame _frame 3 greenGhostMovingRight
                        | otherwise = greenGhostIdle
                translatedPlayer = translate (xPos * fromIntegral spriteSize) (-yPos * fromIntegral spriteSize) sprite 
                (xPos, yPos) = playerPos _player2
                _player2 = fromJust (player2 gstate)
                _dir = playerDir _player2
                _powerUp = puType (powerUp gstate)
                _frame = frame gstate

-- Function that draws collectable points
drawPoints :: GameState -> Picture
drawPoints gstate = pictures (map (drawPoint . fst) usedPoints)
    where   usedPoints = pointList gstate
            usedTile (x, y)     | fst ((level gstate !! round y) !! round x) == BigPointField = bigPointTile
                                | otherwise = pointTile
            drawPoint pos@(x, y) = translate (x * fromIntegral spriteSize) (-y * fromIntegral spriteSize) (usedTile pos)

-- Function that draws the ghosts
drawEnemies :: GameState -> Picture
drawEnemies gstate = pictures (map (drawSprite . enemy) _enemies)
    where   _powerUp = puType (powerUp gstate)
            _enemies = enemies gstate
            enemy e = (enemyPos e, enemyDir e, enemyType e)
            usedSprite dir eType    | dir == DirUp = if _powerUp == InvertedEnemies
                                                        then calculateAnimationFrame _frame 3 invertedGhostMovingUp
                                                        else if eType == GoToPlayer
                                                                then calculateAnimationFrame _frame 3 redGhostMovingUp
                                                                else calculateAnimationFrame _frame 3 blueGhostMovingUp
                                    | dir == DirDown = if _powerUp == InvertedEnemies
                                                        then calculateAnimationFrame _frame 3 invertedGhostMovingDown
                                                        else if eType == GoToPlayer
                                                                then calculateAnimationFrame _frame 3 redGhostMovingDown
                                                                else calculateAnimationFrame _frame 3 blueGhostMovingDown
                                    | dir == DirLeft = if _powerUp == InvertedEnemies
                                                        then calculateAnimationFrame _frame 3 invertedGhostMovingLeft
                                                        else if eType == GoToPlayer
                                                                then calculateAnimationFrame _frame 3 redGhostMovingLeft
                                                                else calculateAnimationFrame _frame 3 blueGhostMovingLeft
                                    | dir == DirRight = if _powerUp == InvertedEnemies
                                                                then calculateAnimationFrame _frame 3 invertedGhostMovingRight
                                                                else if eType == GoToPlayer
                                                                        then calculateAnimationFrame _frame 3 redGhostMovingRight
                                                                        else calculateAnimationFrame _frame 3 blueGhostMovingRight
                                    | otherwise = if _powerUp == InvertedEnemies
                                                        then invertedGhostIdle
                                                        else if eType == GoToPlayer
                                                                then redGhostIdle
                                                                else blueGhostIdle
            drawSprite ((x, y), dir, eType) = translate (x * fromIntegral spriteSize) (-y * fromIntegral spriteSize) (usedSprite dir eType)
            _frame = frame gstate

-- Function that draws the available powerups
drawPowerUps :: GameState -> Picture
drawPowerUps gstate = pictures (map drawPowerUp powerUps)
    where   powerUps = availablePowerUps gstate
            sprite x = case x of
                SpeedUp         -> speedUpPowerUp
                InvertedEnemies -> invertedEnemiesPowerUp
                EatEnemies      -> eatEnemiesPowerUp
                _               -> emptyTile
            drawPowerUp _powerUp = translate (fst (position _powerUp) * fromIntegral spriteSize) (-snd (position _powerUp) * fromIntegral spriteSize) (sprite (puType _powerUp))

-- Function that draws all the active animations
drawAnimations :: GameState -> Picture
drawAnimations gstate = pictures (map (drawAnimation secs) animations)
    where   animations = activeAnimations gstate
            secs = passedTime gstate

-- Function that draws one animation
drawAnimation :: Float -> Animation -> Picture
drawAnimation secs anim = translate (x * fromIntegral spriteSize) (-y * fromIntegral spriteSize) (usedPictures !! index)
    where   _startTime = startTime anim
            _stopTime = stopTime anim
            _animType = animationType anim
            (x, y) = animPos anim
            usedPictures        | _animType == RedEnemyDied = redEnemyDiedAnimation
                                | otherwise = blueEnemyDiedAnimation
            numberOfSprites = length usedPictures
            index = floor ((secs - _startTime) / ((_stopTime - _startTime) / fromIntegral numberOfSprites))

-- Function that calculates the rotation of the player sprite with a given direction he should move into
calculateRotation :: Direction -> Float
calculateRotation DirUp = 270
calculateRotation DirLeft = 180
calculateRotation DirDown = 90
calculateRotation _ = 0

-- Function that gives the current frame of an animation from a list based on the amount of cycles per second and the current frame
calculateAnimationFrame :: Int -> Int -> [Picture] -> Picture
calculateAnimationFrame _frame cycles list = list !! usedIndex
    where   index = (_frame * _length * cycles) `div` 60
            usedIndex = index `mod` _length
            _length = length list