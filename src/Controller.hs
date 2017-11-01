-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Level
--import Enemy (zipPointAndFieldType)

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
    -- | True = zipPointAndFieldType $ level gstate
    | gstate == MainMenu = return $ gstate
    | gstate == WonScreen = return $ gstate
    | gstate == DiedScreen = return $ gstate
    | gstate == LevelChooser = return $ gstate
    | gstate == Paused = return $ gstate
    | otherwise = return $ movePlayer gstate

movePlayer :: GameState -> GameState
movePlayer gstate = gstate { player = newPlayer }
    where   _player = player gstate
            _playerDir = playerDir _player
            (oldXPos, oldYPos) = playerPos _player
            newPos = case _playerDir of
                DirUp       ->  (oldXPos, oldYPos - playerVelocity)
                DirDown     ->  (oldXPos, oldYPos + playerVelocity)
                DirRight    ->  (oldXPos + playerVelocity, oldYPos)
                DirLeft     ->  (oldXPos - playerVelocity, oldYPos)
                _           ->  (oldXPos, oldYPos)
            newPlayer = Player newPos _playerDir

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
    | isPlaying gstate && playerDir (player gstate) /= DirDown = gstate { player = Player (playerPos (player gstate)) DirUp }
    | otherwise = gstate
inputKey (EventKey (Char 'a') _ _ _) gstate 
    | isPlaying gstate && playerDir (player gstate) /= DirRight = gstate { player = Player (playerPos (player gstate)) DirLeft }
    | otherwise = gstate
inputKey (EventKey (Char 's') _ _ _) gstate 
    | isPlaying gstate && playerDir (player gstate) /= DirUp = gstate { player = Player (playerPos (player gstate)) DirDown }
    | otherwise = gstate
inputKey (EventKey (Char 'd') _ _ _) gstate 
    | isPlaying gstate && playerDir (player gstate) /= DirLeft = gstate { player = Player (playerPos (player gstate)) DirRight }
    | otherwise = gstate
inputKey (EventKey (Char 'l') _ _ _) gstate 
    | isPlaying gstate = gstate { infoToShow = ShowLevel }
    | otherwise = gstate
inputKey _ gstate = gstate -- Otherwise keep the same

isPlaying :: GameState -> Bool
isPlaying MainMenu = False
isPlaying LevelChooser = False
isPlaying WonScreen = False
isPlaying DiedScreen = False
isPlaying Paused = False
isPlaying _ = True