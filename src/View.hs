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
    | otherwise = pictures ((case infoToShow gstate of
              ShowNothing       -> blank
              ShowString  s     -> color green (text s)
              ShowPicture p r   -> rotate r (png p)
              ShowLevel         -> drawLevel gstate) : [drawLevel gstate])

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
            rowPictures = map pictures (map (drawRow gstate levelWidth) _level)

drawRow :: GameState -> Int -> Row -> [Picture]
drawRow _ 0 _ = []
drawRow _ _ [] = []
drawRow gstate n list = translate (fromIntegral (spriteSize * (n-1))) 0 (drawTile gstate (last list)) : drawRow gstate (n-1) (init list)

translateRows :: Int -> [Picture] -> [Picture]
translateRows 0 list = list
translateRows _ [] = []
translateRows n (x:xs) = translate 0 (fromIntegral (spriteSize * (n-1))) x : translateRows (n-1) xs

drawTile :: GameState -> Field -> Picture
drawTile gstate PlayerField = rotate (calculateRotation (playerDir (player gstate))) pacman
drawTile _ WallField = wallTile
drawTile _ PointField = pointTile
drawTile _ BigPointField = bigPointTile
drawTile gstate EnemyField = blank -- hier moet nog iets bedacht worden voor de enemies aangezien deze in een list staan
drawTile _ EmptyField = blank
drawTile _ _ = blank

calculateRotation :: Direction -> Float
calculateRotation DirUp = 270
calculateRotation DirLeft = 180
calculateRotation DirDown = 90
calculateRotation _ = 0