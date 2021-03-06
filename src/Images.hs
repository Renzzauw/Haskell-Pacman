module Images where

import Graphics.Gloss.Game

-- A fixed size for each Field, each image is scaled to this value
spriteSize :: Int
spriteSize = 20

-- Function that scales sprites 
scalePicture :: Picture -> Picture
scalePicture p = scale xScale yScale p
    where   (_, (xSize, ySize)) = boundingBox p
            xScale = fromIntegral spriteSize / xSize
            yScale = fromIntegral spriteSize / ySize

-- ######################################### This part contains loading in images / animations for all sprites in the game #########################################

-- Backgrounds / Screens --

gameBackground :: Picture
gameBackground = png "Images/New/AchtergrondIngame.png"

mainmenuBackground :: Picture 
mainmenuBackground = png "Images/New/AchtergrondMenu.png"

levelTutScreen :: Picture
levelTutScreen = png "Images/New/LevelTutScreen.png"

controlsScreen :: Picture
controlsScreen = png "Images/New/ControlsScreen.png"

levelSelectScreen :: Picture
levelSelectScreen = png "Images/New/LevelSelect.png"

pauseScreen :: Picture
pauseScreen = png "Images/New/PauseMenu.png"

emptyBackground :: Picture
emptyBackground = png "Images/New/emptybg.png"

playerWins :: [Picture]
playerWins = [png "Images/New/Player1Wins.png", png "Images/New/Player2Wins.png"]

-- Ghosts --

redGhostMovingUp :: [Picture]
redGhostMovingUp = [scalePicture (png "Images/Ghosts/Red/RedGhostUp1.png"), scalePicture (png "Images/Ghosts/Red/RedGhostUp2.png")]

redGhostMovingDown :: [Picture]
redGhostMovingDown = [scalePicture (png "Images/Ghosts/Red/RedGhostDown1.png"), scalePicture (png "Images/Ghosts/Red/RedGhostDown2.png")]

redGhostMovingLeft :: [Picture]
redGhostMovingLeft = [scalePicture (png "Images/Ghosts/Red/RedGhostLeft1.png"), scalePicture (png "Images/Ghosts/Red/RedGhostLeft2.png")]

redGhostMovingRight :: [Picture]
redGhostMovingRight = [scalePicture (png "Images/Ghosts/Red/RedGhostRight1.png"), scalePicture (png "Images/Ghosts/Red/RedGhostRight2.png")]

redGhostIdle :: Picture
redGhostIdle = scalePicture (png "Images/Ghosts/Red/RedGhostRight1.png")

blueGhostMovingUp :: [Picture]
blueGhostMovingUp = [scalePicture (png "Images/Ghosts/Blue/BlueGhostUp1.png"), scalePicture (png "Images/Ghosts/Blue/BlueGhostUp2.png")]

blueGhostMovingDown :: [Picture]
blueGhostMovingDown = [scalePicture (png "Images/Ghosts/Blue/BlueGhostDown1.png"), scalePicture (png "Images/Ghosts/Blue/BlueGhostDown2.png")]

blueGhostMovingLeft :: [Picture]
blueGhostMovingLeft = [scalePicture (png "Images/Ghosts/Blue/BlueGhostLeft1.png"), scalePicture (png "Images/Ghosts/Blue/BlueGhostLeft2.png")]

blueGhostMovingRight :: [Picture]
blueGhostMovingRight = [scalePicture (png "Images/Ghosts/Blue/BlueGhostRight1.png"), scalePicture (png "Images/Ghosts/Blue/BlueGhostRight2.png")]

blueGhostIdle :: Picture
blueGhostIdle = scalePicture (png "Images/Ghosts/Blue/BlueGhostRight1.png")

invertedGhostMovingUp :: [Picture]
invertedGhostMovingUp = [scalePicture (png "Images/Ghosts/Inverted/InvertedGhostUp1.png"), scalePicture (png "Images/Ghosts/Inverted/InvertedGhostUp2.png")]

invertedGhostMovingDown :: [Picture]
invertedGhostMovingDown = [scalePicture (png "Images/Ghosts/Inverted/InvertedGhostDown1.png"), scalePicture (png "Images/Ghosts/Inverted/InvertedGhostDown2.png")]

invertedGhostMovingLeft :: [Picture]
invertedGhostMovingLeft = [scalePicture (png "Images/Ghosts/Inverted/InvertedGhostLeft1.png"), scalePicture (png "Images/Ghosts/Inverted/InvertedGhostLeft2.png")]

invertedGhostMovingRight :: [Picture]
invertedGhostMovingRight = [scalePicture (png "Images/Ghosts/Inverted/InvertedGhostRight1.png"), scalePicture (png "Images/Ghosts/Inverted/InvertedGhostRight2.png")]

invertedGhostIdle :: Picture
invertedGhostIdle = scalePicture (png "Images/Ghosts/Inverted/InvertedGhostRight1.png")

greenGhostMovingUp :: [Picture]
greenGhostMovingUp = [scalePicture (png "Images/Ghosts/Green/GreenGhostUp1.png"), scalePicture (png "Images/Ghosts/Green/GreenGhostUp2.png")]

greenGhostMovingDown :: [Picture]
greenGhostMovingDown = [scalePicture (png "Images/Ghosts/Green/GreenGhostDown1.png"), scalePicture (png "Images/Ghosts/Green/GreenGhostDown2.png")]

greenGhostMovingLeft :: [Picture]
greenGhostMovingLeft = [scalePicture (png "Images/Ghosts/Green/GreenGhostLeft1.png"), scalePicture (png "Images/Ghosts/Green/GreenGhostLeft2.png")]

greenGhostMovingRight :: [Picture]
greenGhostMovingRight = [scalePicture (png "Images/Ghosts/Green/GreenGhostRight1.png"), scalePicture (png "Images/Ghosts/Green/GreenGhostRight2.png")]

greenGhostIdle :: Picture
greenGhostIdle = scalePicture (png "Images/Ghosts/Green/GreenGhostRight1.png")

purpleGhostMovingUp :: [Picture]
purpleGhostMovingUp = [scalePicture (png "Images/Ghosts/Purple/PurpleGhostUp1.png"), scalePicture (png "Images/Ghosts/Purple/PurpleGhostUp2.png")]

purpleGhostMovingDown :: [Picture]
purpleGhostMovingDown = [scalePicture (png "Images/Ghosts/Purple/PurpleGhostDown1.png"), scalePicture (png "Images/Ghosts/Purple/PurpleGhostDown2.png")]

purpleGhostMovingLeft :: [Picture]
purpleGhostMovingLeft = [scalePicture (png "Images/Ghosts/Purple/PurpleGhostLeft1.png"), scalePicture (png "Images/Ghosts/Purple/PurpleGhostLeft2.png")]

purpleGhostMovingRight :: [Picture]
purpleGhostMovingRight = [scalePicture (png "Images/Ghosts/Purple/PurpleGhostRight1.png"), scalePicture (png "Images/Ghosts/Purple/PurpleGhostRight2.png")]

purpleGhostIdle :: Picture
purpleGhostIdle = scalePicture (png "Images/Ghosts/Purple/PurpleGhostRight1.png")

-- Pacman --

pacmanSeq :: [Picture]
pacmanSeq = [scalePicture (png "Images/PacmanSeq/Pacman0.png"), scalePicture (png "Images/PacmanSeq/Pacman1.png"), scalePicture (png "Images/PacmanSeq/Pacman2.png"), scalePicture (png "Images/PacmanSeq/Pacman1.png")] 

pacmanIdle :: Picture
pacmanIdle = scalePicture (png "Images/PacmanSeq/Pacman1.png")

redPacmanSeq :: [Picture]
redPacmanSeq = [scalePicture (png "Images/PacmanSeq/RedPacman0.png"), scalePicture (png "Images/PacmanSeq/RedPacman1.png"), scalePicture (png "Images/PacmanSeq/RedPacman2.png"), scalePicture (png "Images/PacmanSeq/RedPacman1.png")] 

orangePacmanSeq :: [Picture]
orangePacmanSeq = [scalePicture (png "Images/PacmanSeq/OrangePacman0.png"), scalePicture (png "Images/PacmanSeq/OrangePacman1.png"), scalePicture (png "Images/PacmanSeq/OrangePacman2.png"), scalePicture (png "Images/PacmanSeq/OrangePacman1.png")] 

-- Tiles -- 

wallTile :: Picture
wallTile = scalePicture (png "Images/New/WallTile.png")

pointTile :: Picture
pointTile = scalePicture (png "Images/New/PointTile.png")

bigPointTile :: Picture
bigPointTile = scalePicture (png "Images/New/BigPointTile.png")

emptyTile :: Picture
emptyTile = scalePicture (png "Images/New/Empty.png")

speedUpPowerUp :: Picture
speedUpPowerUp = scalePicture (png "Images/New/PowerUpSpeed.png")

invertedEnemiesPowerUp :: Picture
invertedEnemiesPowerUp = scalePicture (png "Images/New/PowerUpReverse.png")

eatEnemiesPowerUp :: Picture
eatEnemiesPowerUp = scalePicture (png "Images/New/PowerUpEatEnemies.png")

-- Triggers --

orangeTrigger :: [Picture]
orangeTrigger = map (scale 0.6 0.6) [png "Images/Trigger/Orange/OrangeCirkel0.png", png "Images/Trigger/Orange/OrangeCirkel1.png", png "Images/Trigger/Orange/OrangeCirkel2.png", png "Images/Trigger/Orange/OrangeCirkel3.png", png "Images/Trigger/Orange/OrangeCirkel4.png", png "Images/Trigger/Orange/OrangeCirkel5.png", png "Images/Trigger/Orange/OrangeCirkel6.png", png "Images/Trigger/Orange/OrangeCirkel7.png"]

redTrigger :: [Picture]
redTrigger = map (scale 0.6 0.6) [png "Images/Trigger/Red/RedCirkel0.png", png "Images/Trigger/Red/RedCirkel1.png", png "Images/Trigger/Red/RedCirkel2.png", png "Images/Trigger/Red/RedCirkel3.png", png "Images/Trigger/Red/RedCirkel4.png", png "Images/Trigger/Red/RedCirkel5.png", png "Images/Trigger/Red/RedCirkel6.png", png "Images/Trigger/Red/RedCirkel7.png"]

purpleTrigger :: [Picture]
purpleTrigger = map (scale 0.6 0.6) [png "Images/Trigger/Purple/PurpleCirkel0.png", png "Images/Trigger/Purple/PurpleCirkel1.png", png "Images/Trigger/Purple/PurpleCirkel2.png", png "Images/Trigger/Purple/PurpleCirkel3.png", png "Images/Trigger/Purple/PurpleCirkel4.png", png "Images/Trigger/Purple/PurpleCirkel5.png", png "Images/Trigger/Purple/PurpleCirkel6.png", png "Images/Trigger/Purple/PurpleCirkel7.png"]

redEnemyDiedAnimation :: [Picture]
redEnemyDiedAnimation = [scalePicture (png "Images/DieSeq/Bluedie0.png"), scalePicture (png "Images/DieSeq/Bluedie1.png"), scalePicture (png "Images/DieSeq/Bluedie2.png"), scalePicture (png "Images/DieSeq/Bluedie3.png"), scalePicture (png "Images/DieSeq/Bluedie4.png"), scalePicture (png "Images/DieSeq/Bluedie5.png"), scalePicture (png "Images/DieSeq/Bluedie6.png"), scalePicture (png "Images/DieSeq/Bluedie7.png"), scalePicture (png "Images/DieSeq/Bluedie8.png"), scalePicture (png "Images/DieSeq/Bluedie9.png"), scalePicture (png "Images/DieSeq/Bluedie10.png"), scalePicture (png "Images/DieSeq/Bluedie11.png"), scalePicture (png "Images/DieSeq/Bluedie12.png"), scalePicture (png "Images/DieSeq/Bluedie13.png")]

blueEnemyDiedAnimation :: [Picture]
blueEnemyDiedAnimation = [scalePicture (png "Images/DieSeq/Bluedie0.png"), scalePicture (png "Images/DieSeq/Bluedie1.png"), scalePicture (png "Images/DieSeq/Bluedie2.png"), scalePicture (png "Images/DieSeq/Bluedie3.png"), scalePicture (png "Images/DieSeq/Bluedie4.png"), scalePicture (png "Images/DieSeq/Bluedie5.png"), scalePicture (png "Images/DieSeq/Bluedie6.png"), scalePicture (png "Images/DieSeq/Bluedie7.png"), scalePicture (png "Images/DieSeq/Bluedie8.png"), scalePicture (png "Images/DieSeq/Bluedie9.png"), scalePicture (png "Images/DieSeq/Bluedie10.png"), scalePicture (png "Images/DieSeq/Bluedie11.png"), scalePicture (png "Images/DieSeq/Bluedie12.png"), scalePicture (png "Images/DieSeq/Bluedie13.png")]

-- #############################################################################################################################################################################