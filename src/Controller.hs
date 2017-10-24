-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return $ gstate

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'w') _ _ _) gstate = gstate { infoToShow = ShowString "omhoog", playerDir = DirUp }
inputKey (EventKey (Char 'a') _ _ _) gstate = gstate { infoToShow = ShowString "links", playerDir = DirLeft }
inputKey (EventKey (Char 's') _ _ _) gstate = gstate { infoToShow = ShowString "omlaag", playerDir = DirDown }
inputKey (EventKey (Char 'd') _ _ _) gstate = gstate { infoToShow = ShowString "rechts", playerDir = DirRight }
inputKey (EventKey (Char c) _ _ _) gstate = gstate { infoToShow = ShowAChar c }
inputKey _ gstate = gstate -- Otherwise keep the same