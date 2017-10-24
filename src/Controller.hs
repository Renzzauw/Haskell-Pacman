-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Level

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
    | gstate == MainMenu = return $ gstate
    | gstate == WonScreen = return $ gstate
    | gstate == DiedScreen = return $ gstate
    | gstate == LevelChooser = return $ gstate
    | gstate == Paused = return $ gstate
    | otherwise = return $ gstate

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char '1') _ _ _) gstate = MainMenu
inputKey (EventKey (Char '2') _ _ _) gstate = LevelChooser
inputKey (EventKey (Char '3') _ _ _) gstate = WonScreen
inputKey (EventKey (Char '4') _ _ _) gstate = DiedScreen
inputKey (EventKey (Char '5') _ _ _) gstate = Paused
inputKey (EventKey (Char '6') _ _ _) gstate = initialState
inputKey (EventKey (Char 'w') _ _ _) gstate
    | isPlaying gstate = gstate { infoToShow = ShowString "omhoog", player = Player { playerPos = playerPos (player gstate), playerDir = DirUp } }
    | otherwise = gstate
inputKey (EventKey (Char 'a') _ _ _) gstate
    | isPlaying gstate = gstate { infoToShow = ShowString "links", player = Player { playerPos = playerPos (player gstate), playerDir = DirLeft } }
    | otherwise = gstate
inputKey (EventKey (Char 's') _ _ _) gstate
    | isPlaying gstate = gstate { infoToShow = ShowString "omlaag", player = Player { playerPos = playerPos (player gstate), playerDir = DirDown } }
    | otherwise = gstate
inputKey (EventKey (Char 'd') _ _ _) gstate 
    | isPlaying gstate = gstate { infoToShow = ShowString "rechts", player = Player { playerPos = playerPos (player gstate), playerDir = DirRight } }
    | otherwise = gstate
inputKey _ gstate = gstate -- Otherwise keep the same

isPlaying :: GameState -> Bool
isPlaying MainMenu = False
isPlaying LevelChooser = False
isPlaying WonScreen = False
isPlaying DiedScreen = False
isPlaying Paused = False
isPlaying _ = True