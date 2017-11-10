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
gameBackground = png "Images/new/AchtergrondIngame.png"

mainmenuBackground :: Picture 
mainmenuBackground = png "Images/new/AchtergrondMenu.png"

levelTutScreen :: Picture
levelTutScreen = png "Images/new/LevelTutScreen.png"

controlsScreen :: Picture
controlsScreen = png "Images/new/ControlsScreen.png"

levelSelectScreen :: Picture
levelSelectScreen = png "Images/new/LevelSelect.png"

pauseScreen :: Picture
pauseScreen = png "Images/new/PauseMenu.png"

emptyBackground :: Picture
emptyBackground = png "Images/new/emptybg.png"

playerWins :: [Picture]
playerWins = [png "Images/new/Player1Wins.png", png "Images/new/Player2Wins.png"]

-- Ghosts --

redGhostMovingUp :: [Picture]
redGhostMovingUp = [scalePicture (png "Images/Ghosts/RedGhostUp1.png"), scalePicture (png "Images/Ghosts/RedGhostUp2.png")]

redGhostMovingDown :: [Picture]
redGhostMovingDown = [scalePicture (png "Images/Ghosts/RedGhostDown1.png"), scalePicture (png "Images/Ghosts/RedGhostDown2.png")]

redGhostMovingLeft :: [Picture]
redGhostMovingLeft = [scalePicture (png "Images/Ghosts/RedGhostLeft1.png"), scalePicture (png "Images/Ghosts/RedGhostLeft2.png")]

redGhostMovingRight :: [Picture]
redGhostMovingRight = [scalePicture (png "Images/Ghosts/RedGhostRight1.png"), scalePicture (png "Images/Ghosts/RedGhostRight2.png")]

redGhostIdle :: Picture
redGhostIdle = scalePicture (png "Images/Ghosts/RedGhostRight1.png")

blueGhostMovingUp :: [Picture]
blueGhostMovingUp = [scalePicture (png "Images/Ghosts/BlueGhostUp1.png"), scalePicture (png "Images/Ghosts/BlueGhostUp2.png")]

blueGhostMovingDown :: [Picture]
blueGhostMovingDown = [scalePicture (png "Images/Ghosts/BlueGhostDown1.png"), scalePicture (png "Images/Ghosts/BlueGhostDown2.png")]

blueGhostMovingLeft :: [Picture]
blueGhostMovingLeft = [scalePicture (png "Images/Ghosts/BlueGhostLeft1.png"), scalePicture (png "Images/Ghosts/BlueGhostLeft2.png")]

blueGhostMovingRight :: [Picture]
blueGhostMovingRight = [scalePicture (png "Images/Ghosts/BlueGhostRight1.png"), scalePicture (png "Images/Ghosts/BlueGhostRight2.png")]

blueGhostIdle :: Picture
blueGhostIdle = scalePicture (png "Images/Ghosts/BlueGhostRight1.png")

invertedGhostMovingUp :: [Picture]
invertedGhostMovingUp = [scalePicture (png "Images/Ghosts/InvertedGhostUp1.png"), scalePicture (png "Images/Ghosts/InvertedGhostUp2.png")]

invertedGhostMovingDown :: [Picture]
invertedGhostMovingDown = [scalePicture (png "Images/Ghosts/InvertedGhostDown1.png"), scalePicture (png "Images/Ghosts/InvertedGhostDown2.png")]

invertedGhostMovingLeft :: [Picture]
invertedGhostMovingLeft = [scalePicture (png "Images/Ghosts/InvertedGhostLeft1.png"), scalePicture (png "Images/Ghosts/InvertedGhostLeft2.png")]

invertedGhostMovingRight :: [Picture]
invertedGhostMovingRight = [scalePicture (png "Images/Ghosts/InvertedGhostRight1.png"), scalePicture (png "Images/Ghosts/InvertedGhostRight2.png")]

invertedGhostIdle :: Picture
invertedGhostIdle = scalePicture (png "Images/Ghosts/InvertedGhostRight1.png")

greenGhostMovingUp :: [Picture]
greenGhostMovingUp = [scalePicture (png "Images/Ghosts/GreenGhostUp1.png"), scalePicture (png "Images/Ghosts/GreenGhostUp2.png")]

greenGhostMovingDown :: [Picture]
greenGhostMovingDown = [scalePicture (png "Images/Ghosts/GreenGhostDown1.png"), scalePicture (png "Images/Ghosts/GreenGhostDown2.png")]

greenGhostMovingLeft :: [Picture]
greenGhostMovingLeft = [scalePicture (png "Images/Ghosts/GreenGhostLeft1.png"), scalePicture (png "Images/Ghosts/GreenGhostLeft2.png")]

greenGhostMovingRight :: [Picture]
greenGhostMovingRight = [scalePicture (png "Images/Ghosts/GreenGhostRight1.png"), scalePicture (png "Images/Ghosts/GreenGhostRight2.png")]

greenGhostIdle :: Picture
greenGhostIdle = scalePicture (png "Images/Ghosts/GreenGhostRight1.png")

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
wallTile = scalePicture (png "Images/new/WallTile.png")

pointTile :: Picture
pointTile = scalePicture (png "Images/new/PointTile.png")

bigPointTile :: Picture
bigPointTile = scalePicture (png "Images/new/BigPointTile.png")

emptyTile :: Picture
emptyTile = scalePicture (png "Images/Empty.png")

speedUpPowerUp :: Picture
speedUpPowerUp = scalePicture (png "Images/New/PowerUpSpeed.png")

invertedEnemiesPowerUp :: Picture
invertedEnemiesPowerUp = scalePicture (png "Images/New/PowerUpReverse.png")

eatEnemiesPowerUp :: Picture
eatEnemiesPowerUp = scalePicture (png "Images/New/PowerUpEatEnemies.png")

redEnemyDiedAnimation :: [Picture]
redEnemyDiedAnimation = [scalePicture (png "Images/PointTile.png"), scalePicture (png "Images/BigPointTile.png"), scalePicture (png "Images/PointTile.png"), scalePicture (png "Images/BigPointTile.png")]

blueEnemyDiedAnimation :: [Picture]
blueEnemyDiedAnimation = [wall, scale 0.8 0.8 wall, scale 0.6 0.6 wall, scale 0.4 0.4 wall, scale 0.2 0.2 wall]
    where   wall = scalePicture (png "Images/WallTile.png")

-- #############################################################################################################################################################################