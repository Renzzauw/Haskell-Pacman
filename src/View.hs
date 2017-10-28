-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Graphics.Gloss.Game
import Model

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

pacmanSprites :: [Picture]
pacmanSprites = [png "Images/Pacman1.png", png "Images/Pacman2.png", png "Images/Pacman3.png"]

wallTile :: Picture
wallTile = png "Images/WallTile.png"