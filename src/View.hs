-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Graphics.Gloss.Game
import Model
import Level

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate
    | gstate == MainMenu = color green (text "MainMenu")
    | gstate == WonScreen = color green (text "WonScreen")
    | gstate == DiedScreen = color green (text "DiedScreen")
    | gstate == LevelChooser = color green (text "LevelChooser")
    | gstate == Paused = color green (text "Paused")
    | otherwise = case infoToShow gstate of
              ShowNothing       -> blank
              ShowString  s     -> color green (text s)
              ShowPicture p r   -> rotate r (png p)
              ShowLevel         -> drawLevel gstate

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

drawLevel :: GameState -> Picture
drawLevel gstate = translate (0.5 * (fromIntegral (-spriteSize * levelWidth))) (0.5 * (fromIntegral (-spriteSize * levelHeight))) (pictures (translateRows levelHeight rowPictures))
    where   _level = level gstate
            levelWidth = length (head _level)
            levelHeight = length _level
            rowPictures = map pictures (map (drawRow levelWidth) _level)

drawRow :: Int -> Row -> [Picture]
drawRow 0 _ = []
drawRow _ [] = []
drawRow n list = translate (fromIntegral (spriteSize * (n-1))) 0 (drawTile (last list)) : drawRow (n-1) (init list)

translateRows :: Int -> [Picture] -> [Picture]
translateRows 0 list = list
translateRows _ [] = []
translateRows n (x:xs) = translate 0 (fromIntegral (spriteSize * (n-1))) x : translateRows (n-1) xs

drawTile :: Field -> Picture
drawTile PlayerField = pacman
drawTile WallField = wallTile
drawTile PointField = pointTile
drawTile BigPointField = bigPointTile
drawTile EnemyField = blank
drawTile EmptyField = blank