module Main where

import Controller
import Model
import View
import Level
import Enemy

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Pacman" (1280, 720) (320, 180)) -- Or FullScreen
              black            -- Background color
              60               -- Frames per second
              MainMenu         -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function