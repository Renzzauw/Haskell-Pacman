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
            includingPacman = pictures (totalLevel : [drawPacman gstate])
            translatedLevel = translate (0.5 * (fromIntegral (-spriteSize * levelWidth))) (-0.5 * (fromIntegral (-spriteSize * levelHeight))) includingPacman

drawTile :: Field -> Picture
drawTile (WallField, (xPos, yPos)) = translate (xPos * fromIntegral spriteSize) (-yPos * fromIntegral spriteSize) wallTile
drawTile (PointField, (xPos, yPos)) = translate (xPos * fromIntegral spriteSize) (-yPos * fromIntegral spriteSize) pointTile
drawTile (BigPointField, (xPos, yPos)) = translate (xPos * fromIntegral spriteSize) (-yPos * fromIntegral spriteSize) bigPointTile
drawTile (EnemyField, (xPos, yPos)) = translate (xPos * fromIntegral spriteSize) (-yPos * fromIntegral spriteSize) emptyTile -- enemies moeten naar aparte functie verplaatst worden net zoals pacman
drawTile (EmptyField, (xPos, yPos)) = translate (xPos * fromIntegral spriteSize) (-yPos * fromIntegral spriteSize) emptyTile
drawTile (_, (xPos, yPos)) = translate (xPos * fromIntegral spriteSize) (-yPos * fromIntegral spriteSize) emptyTile

drawPacman :: GameState -> Picture
drawPacman gstate = translatedPacman
    where   rotatedPacman = rotate (calculateRotation (playerDir (player gstate))) pacman
            translatedPacman = translate (xPos * (fromIntegral spriteSize)) (-yPos * fromIntegral spriteSize) rotatedPacman 
            (xPos, yPos) = playerPos (player gstate)
            levelHeight = length (level gstate)

calculateRotation :: Direction -> Float
calculateRotation DirUp = 270
calculateRotation DirLeft = 180
calculateRotation DirDown = 90
calculateRotation _ = 0