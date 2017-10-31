-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import Level

view :: GameState -> IO Picture
view gstate = case infoToShow gstate of
    ShowLevel   -> createLevel gstate
    _           -> return (viewPure gstate)

viewPure :: GameState -> Picture
viewPure MainMenu = color green (text "MainMenu")
viewPure WonScreen = color green (text "WonScreen")
viewPure DiedScreen = color green (text "DiedScreen")
viewPure LevelChooser = color green (text "LevelChooser")
viewPure Paused = color green (text "Paused")
viewPure gstate = case infoToShow gstate of
              ShowNothing       -> blank
              ShowString  s     -> color green (text s)
              _                 -> blank

spriteSize :: Int
spriteSize = 40

pacmanSprites :: [IO Picture]
pacmanSprites = [loadBMP "Images/Pacman1.bmp", loadBMP "Images/Pacman2.bmp", loadBMP "Images/Pacman3.bmp"]

pacman :: IO Picture
pacman = loadBMP "Images/Pacman2.bmp"

wallTile :: IO Picture
wallTile = loadBMP "Images/WallTile.bmp"

pointTile :: IO Picture
pointTile = loadBMP "Images/PointTile.bmp"

bigPointTile :: IO Picture
bigPointTile = loadBMP "Images/BigPointTile.bmp"

emptyTile :: IO Picture
emptyTile = loadBMP "Images/Empty.bmp"

getTiles :: GameState -> Row -> [IO Picture]
getTiles gstate row = map (drawTile gstate) row

convertTiles :: GameState -> Row -> IO [Picture]
convertTiles gstate row = sequence (getTiles gstate row)

getLevelTiles :: GameState -> IO [[Picture]]
getLevelTiles gstate = sequence (map (convertTiles gstate) (level gstate))

createLevel :: GameState -> IO Picture
createLevel gstate = do
    _pictures <- getLevelTiles gstate
    let translatedRows = map (translateRowsInX levelWidth) _pictures
    let picturedRows = map pictures translatedRows
    let translatedColumns = translateRows levelHeight picturedRows
    let total = pictures translatedColumns
    return $ total
    where   levelWidth = length (head (level gstate))
            levelHeight = length (level gstate)

{-drawLevel :: GameState -> Picture
drawLevel gstate = translate (0.5 * (fromIntegral (-spriteSize * levelWidth))) (0.5 * (fromIntegral (-spriteSize * levelHeight))) (pictures (translateRows levelHeight rowPictures))
    where   _level = level gstate
            levelWidth = length (head _level)
            levelHeight = length _level
            rowPictures = map pictures (map (drawRow gstate levelWidth) _level)-}

{-drawRow :: GameState -> Int -> Row -> [Picture]
drawRow _ 0 _ = []
drawRow _ _ [] = []
drawRow gstate n list = translate (fromIntegral (spriteSize * (n-1))) 0 (drawTile gstate (last list)) : drawRow gstate (n-1) (init list)-}

translateRows :: Int -> [Picture] -> [Picture]
translateRows 0 list = list
translateRows _ [] = []
translateRows n (x:xs) = translate 0 (fromIntegral (spriteSize * (n-1))) x : translateRows (n-1) xs

translateRowsInX :: Int -> [Picture] -> [Picture]
translateRowsInX 0 list = list
translateRowsInX _ [] = []
translateRowsInX n (x:xs) = translate (fromIntegral (spriteSize * (n-1))) 0 x : translateRows (n-1) xs

drawTile :: GameState -> Field -> IO Picture
drawTile gstate PlayerField = do
    _pacman <- pacman
    let rotatedPacman = rotate (calculateRotation (playerDir (player gstate))) _pacman
    return $ rotatedPacman
drawTile _ WallField = wallTile
drawTile _ PointField = pointTile
drawTile _ BigPointField = bigPointTile
drawTile gstate EnemyField = emptyTile -- hier moet nog iets bedacht worden voor de enemies aangezien deze in een list staan
drawTile _ EmptyField = emptyTile
drawTile _ _ = emptyTile

calculateRotation :: Direction -> Float
calculateRotation DirUp = 270
calculateRotation DirLeft = 180
calculateRotation DirDown = 90
calculateRotation _ = 0