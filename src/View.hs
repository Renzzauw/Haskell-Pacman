-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Game
import Model
import Level

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure MainMenu = color green (text "MainMenu")
viewPure WonScreen = color green (text "WonScreen")
viewPure DiedScreen = color green (text "DiedScreen")
viewPure LevelChooser = color green (text "LevelChooser")
viewPure Paused = color green (text "Paused")
viewPure gstate = case infoToShow gstate of
              ShowNothing       -> blank
              ShowString  s     -> color green (text s)
              ShowLevel         -> drawLevel gstate
              _                 -> blank

spriteSize :: Int
spriteSize = 40

pacmanSprites :: [Picture]
pacmanSprites = [png "Images/Pacman1.png", png "Images/Pacman2.png", png "Images/Pacman3.png", png "Images/Pacman2.png"]

redGhostMovingUp :: [Picture]
redGhostMovingUp = [png "Images/RedGhostUp1.png", png "Images/RedGhostUp2.png"]

redGhostMovingDown :: [Picture]
redGhostMovingDown = [png "Images/RedGhostDown1.png", png "Images/RedGhostDown2.png"]

redGhostMovingLeft :: [Picture]
redGhostMovingLeft = [png "Images/RedGhostLeft1.png", png "Images/RedGhostLeft2.png"]

redGhostMovingRight :: [Picture]
redGhostMovingRight = [png "Images/RedGhostRight1.png", png "Images/RedGhostRight2.png"]

redGhostUp :: Picture
redGhostUp = png "Images/RedGhostUp1.png"

redGhostDown :: Picture
redGhostDown = png "Images/RedGhostDown1.png"

redGhostLeft :: Picture
redGhostLeft = png "Images/RedGhostLeft1.png"

redGhostRight :: Picture
redGhostRight = png "Images/RedGhostRight1.png"

pacman :: Picture
pacman = png "Images/Pacman2.png"

wallTile :: Picture
wallTile = png "Images/WallTile.png"

pointTile :: Picture
pointTile = png "Images/PointTile.png"

bigPointTile :: Picture
bigPointTile = png "Images/BigPointTile.png"

emptyTile :: Picture
emptyTile = png "Images/Empty.png"

drawLevel :: GameState -> Picture
drawLevel gstate = translatedLevel
    where   _level = level gstate
            levelWidth = length (head _level)
            levelHeight = length _level
            totalLevel = pictures (map pictures ((map . map) drawTile _level))
            includingPacmanAndPoints = pictures (totalLevel : drawPoints gstate : drawEnemies gstate : [drawPacman gstate])
            translatedLevel = translate (0.5 * (fromIntegral (-spriteSize * levelWidth))) (-0.5 * (fromIntegral (-spriteSize * levelHeight))) includingPacmanAndPoints

drawTile :: Field -> Picture
drawTile (WallField, (xPos, yPos)) = translate (xPos * fromIntegral spriteSize) (-yPos * fromIntegral spriteSize) wallTile
drawTile (_, (xPos, yPos)) = translate (xPos * fromIntegral spriteSize) (-yPos * fromIntegral spriteSize) emptyTile

drawPacman :: GameState -> Picture
drawPacman gstate = translatedPacman
    where   rotatedPacman = rotate (calculateRotation (playerDir (player gstate))) pacman
            translatedPacman = translate (xPos * (fromIntegral spriteSize)) (-yPos * fromIntegral spriteSize) rotatedPacman 
            (xPos, yPos) = playerPos (player gstate)
            levelHeight = length (level gstate)

drawPoints :: GameState -> Picture
drawPoints gstate = pictures (map drawPoint positions)
    where   usedPoints = filter ((==True).fst) (pointList gstate)
            positions = map snd usedPoints
            usedTile (x, y)     | fst (((level gstate) !! round y) !! round x) == BigPointField = bigPointTile
                                | otherwise = pointTile
            drawPoint pos@(x, y) = translate (x * (fromIntegral spriteSize)) (-y * (fromIntegral spriteSize)) (usedTile pos)

drawEnemies :: GameState -> Picture
drawEnemies gstate = pictures (map drawSprite (map enemy _enemies))
    where   _enemies = enemies gstate
            enemy e = (enemyPos e, enemyDir e)
            usedSprite dir  | dir == DirUp = redGhostUp
                            | dir == DirDown = redGhostDown
                            | dir == DirLeft = redGhostLeft
                            | otherwise = redGhostRight
            drawSprite ((x, y), dir) = translate (x * (fromIntegral spriteSize)) (-y * (fromIntegral spriteSize)) (usedSprite dir)

calculateRotation :: Direction -> Float
calculateRotation DirUp = 270
calculateRotation DirLeft = 180
calculateRotation DirDown = 90
calculateRotation _ = 0