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
pacmanSprites = [png "Images/Pacman1.png", png "Images/Pacman2.png", png "Images/Pacman3.png"]

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
            rows = map (drawRow levelWidth) _level
            translatedRows = translateRowsInY levelHeight rows
            totalLevel = pictures translatedRows
            includingPacman = pictures (totalLevel : [drawPacman gstate])
            translatedLevel = translate (0.5 * (fromIntegral (-spriteSize * levelWidth))) (0.5 * (fromIntegral (-spriteSize * levelHeight))) includingPacman

drawRow :: Int -> Row -> Picture
drawRow w row = pictures translatedRow
    where   tiles = map drawTile row
            translatedRow = translateRowsInX w tiles

translateRowsInY :: Int -> [Picture] -> [Picture]
translateRowsInY 1 list = list
translateRowsInY _ [] = []
translateRowsInY n (x:xs) = translate 0 (fromIntegral (spriteSize * (n-1))) x : translateRowsInY (n-1) xs

translateRowsInX :: Int -> [Picture] -> [Picture]
translateRowsInX 1 list = list
translateRowsInX _ [] = []
translateRowsInX n list = translate (fromIntegral (spriteSize * (n-1))) 0 (last list) : translateRowsInX (n-1) (init list)

drawTile :: Field -> Picture
drawTile WallField = wallTile
drawTile PointField = pointTile
drawTile BigPointField = bigPointTile
drawTile EnemyField = emptyTile -- enemies moeten naar aparte functie verplaatst worden net zoals pacman
drawTile EmptyField = emptyTile
drawTile _ = emptyTile

drawPacman :: GameState -> Picture
drawPacman gstate = translatedPacman
    where   rotatedPacman = rotate (calculateRotation (playerDir (player gstate))) pacman
            translatedPacman = translate (xPos * (fromIntegral spriteSize)) ((fromIntegral levelHeight - yPos - 1) * fromIntegral spriteSize) rotatedPacman 
            (xPos, yPos) = playerPos (player gstate)
            levelHeight = length (level gstate)

calculateRotation :: Direction -> Float
calculateRotation DirUp = 270
calculateRotation DirLeft = 180
calculateRotation DirDown = 90
calculateRotation _ = 0