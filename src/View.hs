-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Game
import Model
import Level

-- Draw loop fro drawing images on the game window
view :: GameState -> IO Picture
view = return . viewPure

-- Show the right screen with the given GameState
viewPure :: GameState -> Picture
viewPure MainMenu = mainmenuBackground
viewPure gstate@(WonScreen _) = drawWonScreen gstate
viewPure gstate@(DiedScreen _) = drawDiedScreen gstate
viewPure gstate@(LevelChooser _) = drawLevelChooser gstate--translate (-400) 0 (color green (text "LevelChooser"))
viewPure (Paused _ _ _ _ _) = pauseScreen
viewPure HelpScreen = drawHelpScreen
viewPure ControlsScreen = drawControlsScreen
viewPure gstate = pictures (drawLevel gstate : [drawScore gstate])

-- Draw the score of the player on the screen
drawScore :: GameState -> Picture
drawScore gs = translate (-600) 200 (scale 0.2 0.2 (color yellow (text ("Score: " ++ show _score))))
           where _score = score gs

 -- Draw the proper text when the player completes the level          
drawWonScreen :: GameState -> Picture
drawWonScreen gs = pictures [emptyBackground, translate (-300) 50 (scale 0.7 0.7 (color green (text "Congratulations!"))), translate (-300) (-100) (scale 0.6 0.6 (color yellow (text ("You scored: " ++ show _score))))]
                 where _score = score gs

drawDiedScreen :: GameState -> Picture
drawDiedScreen gs = pictures [emptyBackground, translate (-300) 50 (scale 0.7 0.7 (color red (text "You Died!"))), translate (-300) (-50) (scale 0.6 0.6 (color yellow (text ("You scored: " ++ show _score))))]
                where _score = score gs

drawLevelChooser :: GameState -> Picture
drawLevelChooser gs = pictures (helpText : [translate (-600) (-fromIntegral amountOfLevels * 35 - 25) (pictures (translateLoadLevelText amountOfLevels (map createPicture numbers)))])
    where   list = levels gs
            amountOfLevels = length list
            numbers = [1..amountOfLevels]
            createText number = "Press \'" ++ show number ++ "\' to load " ++ list !! (number - 1)
            createPicture n = scale 0.4 0.4 (color green (text (createText n)))
            helpText = translate (-400) (-350) (scale 0.2 0.2 (color green (text "Press \'H\' to go to the explanation for creating your own levels")))

translateLoadLevelText :: Int -> [Picture] -> [Picture]
translateLoadLevelText 0 p = p
translateLoadLevelText _ [] = []
translateLoadLevelText n (p:ps) = translate 0 (70 * fromIntegral n) p : translateLoadLevelText (n - 1) ps

drawHelpScreen :: Picture
drawHelpScreen = color green (text "HALP")

drawControlsScreen :: Picture
drawControlsScreen = color green (text "Controls")

-- A fixed size for each Field, each image is scaled to this value
spriteSize :: Int
spriteSize = 20

-- ######################################### This part contains loading in images / animations for all sprites in the game #########################################
gameBackground :: Picture
gameBackground = png "Images/new/AchtergrondIngame.png"

mainmenuBackground :: Picture 
mainmenuBackground = png "Images/new/AchtergrondMenu.png"

pauseScreen :: Picture
pauseScreen = png "Images/new/PauseMenu.png"

emptyBackground :: Picture
emptyBackground = png "Images/new/emptybg.png"

pacmanSprites :: [Picture]
pacmanSprites = [scalePicture (png "Images/Pacman1.png"), scalePicture (png "Images/Pacman2.png"), scalePicture (png "Images/Pacman3.png"), scalePicture (png "Images/Pacman2.png")]

redGhostMovingUp :: [Picture]
redGhostMovingUp = [scalePicture (png "Images/RedGhostUp1.png"), scalePicture (png "Images/RedGhostUp2.png")]

redGhostMovingDown :: [Picture]
redGhostMovingDown = [scalePicture (png "Images/RedGhostDown1.png"), scalePicture (png "Images/RedGhostDown2.png")]

redGhostMovingLeft :: [Picture]
redGhostMovingLeft = [scalePicture (png "Images/RedGhostLeft1.png"), scalePicture (png "Images/RedGhostLeft2.png")]

redGhostMovingRight :: [Picture]
redGhostMovingRight = [scalePicture (png "Images/RedGhostRight1.png"), scalePicture (png "Images/RedGhostRight2.png")]

redGhostUp :: Picture
redGhostUp = scalePicture (png "Images/RedGhostUp1.png")

redGhostDown :: Picture
redGhostDown = scalePicture (png "Images/RedGhostDown1.png")

redGhostLeft :: Picture
redGhostLeft = scalePicture (png "Images/RedGhostLeft1.png")

redGhostRight :: Picture
redGhostRight = scalePicture (png "Images/RedGhostRight1.png")

pacman :: Picture
pacman = scalePicture (png "Images/new/Pacman.png") 

wallTile :: Picture
wallTile = scalePicture (png "Images/new/WallTile.png")

pointTile :: Picture
pointTile = scalePicture (png "Images/new/PointTile.png")

bigPointTile :: Picture
bigPointTile = scalePicture (png "Images/new/BigPointTile.png")

emptyTile :: Picture
emptyTile = scalePicture (png "Images/Empty.png")

-- #############################################################################################################################################################################

-- Function that scales sprites 
scalePicture :: Picture -> Picture
scalePicture p = scale xScale yScale p
    where   (_, (xSize, ySize)) = boundingBox p
            xScale = fromIntegral spriteSize / xSize
            yScale = fromIntegral spriteSize / ySize

-- Function that draws each Field of a given level
drawLevel :: GameState -> Picture
drawLevel gstate = translatedLevel
    where   _level = level gstate
            levelWidth = length (head _level)
            levelHeight = length _level
            totalLevel = pictures ((map pictures ((map . map) drawTile _level)))
            includingPacmanAndPoints = pictures (totalLevel : drawPoints gstate : drawEnemies gstate : [drawPacman gstate])
            translatedLevel = pictures [gameBackground, (translate (0.5 * (fromIntegral (-spriteSize * levelWidth))) (-0.5 * (fromIntegral (-spriteSize * levelHeight))) includingPacmanAndPoints)]

-- Function that draws tiles
drawTile :: Field -> Picture
drawTile (WallField, (xPos, yPos)) = translate (xPos * fromIntegral spriteSize) (-yPos * fromIntegral spriteSize) wallTile
drawTile (_, (xPos, yPos)) = translate (xPos * fromIntegral spriteSize) (-yPos * fromIntegral spriteSize) emptyTile

-- Function that draws the player
drawPacman :: GameState -> Picture
drawPacman gstate = translatedPacman
    where   rotatedPacman = rotate (calculateRotation (playerDir (player gstate))) pacman
            translatedPacman = translate (xPos * (fromIntegral spriteSize)) (-yPos * fromIntegral spriteSize) rotatedPacman 
            (xPos, yPos) = playerPos (player gstate)
            levelHeight = length (level gstate)

-- Function that draws collectable points
drawPoints :: GameState -> Picture
drawPoints gstate = pictures (map drawPoint (map fst usedPoints))
    where   usedPoints = pointList gstate
            usedTile (x, y)     | fst (((level gstate) !! round y) !! round x) == BigPointField = bigPointTile
                                | otherwise = pointTile
            drawPoint pos@(x, y) = translate (x * (fromIntegral spriteSize)) (-y * (fromIntegral spriteSize)) (usedTile pos)

-- Function that draws the ghosts
drawEnemies :: GameState -> Picture
drawEnemies gstate = pictures (map drawSprite (map enemy _enemies))
    where   _enemies = enemies gstate
            enemy e = (enemyPos e, enemyDir e)
            usedSprite dir  | dir == DirUp = redGhostUp
                            | dir == DirDown = redGhostDown
                            | dir == DirLeft = redGhostLeft
                            | otherwise = redGhostRight
            drawSprite ((x, y), dir) = translate (x * (fromIntegral spriteSize)) (-y * (fromIntegral spriteSize)) (usedSprite dir)

-- Function that calculates the rotation of the player sprite with a given direction he should move into
calculateRotation :: Direction -> Float
calculateRotation DirUp = 270
calculateRotation DirLeft = 180
calculateRotation DirDown = 90
calculateRotation _ = 0