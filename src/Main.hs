module Main where

import Controller
import Model
import View
import Level
import Enemy

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do 
       (level, points, player, enemies) <- loadLevel "Levels/Level02.txt"
       let state = PlayingLevel 0 level player points enemies
       playIO (InWindow "Pacman" (1280, 720) (320, 180)) -- Or FullScreen
              black            -- Background color
              60               -- Frames per second
              state            -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function