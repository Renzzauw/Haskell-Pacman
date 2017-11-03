-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Game
import Model
import Level

-- Draw loop fro drawing images on the game window
view :: GameState -> IO Picture
view = return . viewPure

-- Show the right screen with the given GameState
viewPure :: GameState -> Picture
viewPure MainMenu = translate (-266.67) 0 (color green (text "MainMenu"))
viewPure WonScreen = translate (-300) 0 (color green (text "WonScreen"))
viewPure DiedScreen = translate (-333.33) 0 (color green (text "DiedScreen"))
viewPure LevelChooser = translate (-400) 0 (color green (text "LevelChooser"))
viewPure (Paused _ _ _ _ _) = translate (-200) 0 (color green (text "Paused"))
viewPure gstate = pictures (drawLevel gstate : [drawScore gstate])


drawScore :: GameState -> Picture
drawScore gs = translate (-600) 200 (scale 0.2 0.2 (color yellow (text ("Score: "++show _score))))
           where _score = score gs

-- A fixed size for each Field, each image is scaled to this value
spriteSize :: Int
spriteSize = 40

-- ######################################### This part contains loading in images / animations for all sprites in the game #########################################
pacmanSprites :: [Picture]
pacmanSprites = [scalePicture (png "Images/Pacman1.png"), scalePicture (png "Images/Pacman2.png"), scalePicture (png "Images/Pacman3.png"), scalePicture (png "Images/Pacman2.png")]

redGhostMovingUp :: [Picture]
redGhostMovingUp = [scalePicture (png "Images/RedGhostUp1.png"), scalePicture (png "Images/RedGhostUp2.png")]

redGhostMovingDown :: [Picture]
redGhostMovingDown = [scalePicture (png "Images/RedGhostDown1.png"), scalePicture (png "Images/RedGhostDown2.png")]

redGhostMovingLeft :: [Picture]
redGhostMovingLeft = [scalePicture (png "Images/RedGhostLeft1.png"), scalePicture (png "Images/RedGhostLeft2.png")]

redGhostMovingRight :: [Picture]
redGhostMovingRight = [scalePicture (png "Images/RedGhostRight1.png"), scalePicture (png "Images/RedGhostRight2.png")]

redGhostUp :: Picture
redGhostUp = scalePicture (png "Images/RedGhostUp1.png")

redGhostDown :: Picture
redGhostDown = scalePicture (png "Images/RedGhostDown1.png")

redGhostLeft :: Picture
redGhostLeft = scalePicture (png "Images/RedGhostLeft1.png")

redGhostRight :: Picture
redGhostRight = scalePicture (png "Images/RedGhostRight1.png")

pacman :: Picture
pacman = scalePicture (png "Images/Pacman2.png")

wallTile :: Picture
wallTile = scalePicture (png "Images/WallTile.png")

pointTile :: Picture
pointTile = scalePicture (png "Images/PointTile.png")

bigPointTile :: Picture
bigPointTile = scalePicture (png "Images/BigPointTile.png")

emptyTile :: Picture
emptyTile = scalePicture (png "Images/Empty.png")

-- #############################################################################################################################################################################

-- Function that scales sprites 
scalePicture :: Picture -> Picture
scalePicture p = scale xScale yScale p
    where   (_, (xSize, ySize)) = boundingBox p
            xScale = fromIntegral spriteSize / xSize
            yScale = fromIntegral spriteSize / ySize

-- Function that draws each Field of a given level
drawLevel :: GameState -> Picture
drawLevel gstate = translatedLevel
    where   _level = level gstate
            levelWidth = length (head _level)
            levelHeight = length _level
            totalLevel = pictures (map pictures ((map . map) drawTile _level))
            includingPacmanAndPoints = pictures (totalLevel : drawPoints gstate : drawEnemies gstate : [drawPacman gstate])
            translatedLevel = translate (0.5 * (fromIntegral (-spriteSize * levelWidth))) (-0.5 * (fromIntegral (-spriteSize * levelHeight))) includingPacmanAndPoints

-- Function that draws tiles
drawTile :: Field -> Picture
drawTile (WallField, (xPos, yPos)) = translate (xPos * fromIntegral spriteSize) (-yPos * fromIntegral spriteSize) wallTile
drawTile (_, (xPos, yPos)) = translate (xPos * fromIntegral spriteSize) (-yPos * fromIntegral spriteSize) emptyTile

-- Function that draws the player
drawPacman :: GameState -> Picture
drawPacman gstate = translatedPacman
    where   rotatedPacman = rotate (calculateRotation (playerDir (player gstate))) pacman
            translatedPacman = translate (xPos * (fromIntegral spriteSize)) (-yPos * fromIntegral spriteSize) rotatedPacman 
            (xPos, yPos) = playerPos (player gstate)
            levelHeight = length (level gstate)

-- Function that draws collectable points
drawPoints :: GameState -> Picture
drawPoints gstate = pictures (map drawPoint usedPoints)
    where   usedPoints = pointList gstate
            usedTile (x, y)     | fst (((level gstate) !! round y) !! round x) == BigPointField = bigPointTile
                                | otherwise = pointTile
            drawPoint pos@(x, y) = translate (x * (fromIntegral spriteSize)) (-y * (fromIntegral spriteSize)) (usedTile pos)

-- Function that draws the ghosts
drawEnemies :: GameState -> Picture
drawEnemies gstate = pictures (map drawSprite (map enemy _enemies))
    where   _enemies = enemies gstate
            enemy e = (enemyPos e, enemyDir e)
            usedSprite dir  | dir == DirUp = redGhostUp
                            | dir == DirDown = redGhostDown
                            | dir == DirLeft = redGhostLeft
                            | otherwise = redGhostRight
            drawSprite ((x, y), dir) = translate (x * (fromIntegral spriteSize)) (-y * (fromIntegral spriteSize)) (usedSprite dir)

-- Function that calculates the rotation of the player sprite with a given direction he should move into
calculateRotation :: Direction -> Float
calculateRotation DirUp = 270
calculateRotation DirLeft = 180
calculateRotation DirDown = 90
calculateRotation _ = 0