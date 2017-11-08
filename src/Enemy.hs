module Enemy where
    
import Level
import Model

import System.Random
import Data.List

upPos :: Position -> Position
upPos (ex, ey) = (ex, ey - enemyVelocity)

downPos :: Position -> Position
downPos (ex, ey) = (ex, ey + enemyVelocity)

leftPos :: Position -> Position
leftPos (ex, ey) = (ex - enemyVelocity, ey)

rightPos :: Position -> Position
rightPos (ex, ey) = (ex + enemyVelocity, ey)

newUp :: Position -> (Position, Bool)
newUp pos = newEnemyPosInX (upPos pos)

newDown :: Position -> (Position, Bool)
newDown pos = newEnemyPosInX (downPos pos)

newLeft :: Position -> (Position, Bool)
newLeft pos = newEnemyPosInY (leftPos pos)

newRight :: Position -> (Position, Bool)
newRight pos = newEnemyPosInY (rightPos pos)

-- Function that returns a Direction for the enemy to move into based on the player position
lookForPlayer :: GameState -> Direction -> Position -> Position -> (Float -> Float -> Bool) -> (Float -> Float -> Bool) -> (Position, Direction)
lookForPlayer gs currDir (px, py) enemypos@(ex, ey) firstOp secondOp = case currDir of
                              DirUp       ->    upOrDown
                              DirDown     ->    upOrDown
                              DirLeft     ->    leftOrRight
                              DirRight    ->    leftOrRight
                              _           ->    noDir
                        -- firstOp is in normal situations <, when the enemies are inverted, it is >
                        -- secondOp is in normal situations >, when the enemies are inverted, it is <
      where upOrDown    | deltaY `firstOp` (-0.05) && checkNewPosition gs DirUp (upPos enemypos) && currDir /= DirDown = (enemypos, DirUp)
                        | deltaY `secondOp` 0.05 && checkNewPosition gs DirDown (downPos enemypos) && currDir /= DirUp = (enemypos, DirDown)
                        | deltaX `firstOp` (-0.05) && checkNewPosition gs DirLeft (leftPos enemypos) && snd (newLeft enemypos) = (fst (newLeft enemypos), DirLeft)
                        | deltaX `secondOp` 0.05 && checkNewPosition gs DirRight (rightPos enemypos) && snd (newRight enemypos) = (fst (newRight enemypos), DirRight)
                        | deltaY `firstOp` (-1) && not (checkNewPosition gs DirUp (upPos enemypos)) && checkNewPosition gs DirLeft (leftPos enemypos) && snd (newLeft enemypos) = (fst (newLeft enemypos), DirLeft)
                        | deltaY `firstOp` (-1) && not (checkNewPosition gs DirUp (upPos enemypos)) && checkNewPosition gs DirRight (rightPos enemypos) && snd (newRight enemypos) = (fst (newRight enemypos), DirRight)
                        | deltaY `secondOp` 1 && not (checkNewPosition gs DirDown (downPos enemypos)) && checkNewPosition gs DirLeft (leftPos enemypos) && snd (newLeft enemypos) = (fst (newLeft enemypos), DirLeft)
                        | deltaY `secondOp` 1 && not (checkNewPosition gs DirDown (downPos enemypos)) && checkNewPosition gs DirRight (rightPos enemypos) && snd (newRight enemypos) = (fst (newRight enemypos), DirRight)
                        | currDir == DirUp && checkNewPosition gs DirUp (upPos enemypos) = (enemypos, DirUp)
                        | currDir == DirDown && checkNewPosition gs DirDown (downPos enemypos) = (enemypos, DirDown)
                        | otherwise = (enemypos, DirNone)
            leftOrRight | deltaX `firstOp` (-0.05) && checkNewPosition gs DirLeft (leftPos enemypos) && currDir /= DirRight = (enemypos, DirLeft)
                        | deltaX `secondOp` 0.05 && checkNewPosition gs DirRight (rightPos enemypos) && currDir /= DirLeft = (enemypos, DirRight)
                        | deltaY `firstOp` (-0.05) && checkNewPosition gs DirUp (upPos enemypos) && snd (newUp enemypos) = (fst (newUp enemypos), DirUp)
                        | deltaY `secondOp` 0.05 && checkNewPosition gs DirDown (downPos enemypos) && snd (newDown enemypos) = (fst (newDown enemypos), DirDown)
                        | deltaX `firstOp` (-1) && not (checkNewPosition gs DirLeft (leftPos enemypos)) && checkNewPosition gs DirUp (upPos enemypos) && snd (newUp enemypos) = (fst (newUp enemypos), DirUp)
                        | deltaX `firstOp` (-1) && not (checkNewPosition gs DirLeft (leftPos enemypos)) && checkNewPosition gs DirDown (downPos enemypos) && snd (newDown enemypos) = (fst (newDown enemypos), DirDown)
                        | deltaX `secondOp` 1 && not (checkNewPosition gs DirRight (rightPos enemypos)) && checkNewPosition gs DirUp (upPos enemypos) && snd (newUp enemypos) = (fst (newUp enemypos), DirUp)
                        | deltaX `secondOp` 1 && not (checkNewPosition gs DirRight (rightPos enemypos)) && checkNewPosition gs DirDown (downPos enemypos) && snd (newDown enemypos) = (fst (newDown enemypos), DirDown)
                        | currDir == DirRight && checkNewPosition gs DirRight (rightPos enemypos) = (enemypos, DirRight)
                        | currDir == DirLeft && checkNewPosition gs DirLeft (leftPos enemypos) = (enemypos, DirLeft)
                        | otherwise = (enemypos, DirNone)
            noDir       | deltaY `firstOp` (-0.05) && checkNewPosition gs DirUp (upPos enemypos) && snd (newUp enemypos) = (fst (newUp enemypos), DirUp)
                        | deltaY `secondOp` 0.05 && checkNewPosition gs DirDown (downPos enemypos) && snd (newDown enemypos) = (fst (newDown enemypos), DirDown)
                        | deltaX `firstOp` (-0.05) && checkNewPosition gs DirLeft (leftPos enemypos) && snd (newLeft enemypos) = (fst (newLeft enemypos), DirLeft)
                        | deltaX `secondOp` 0.05 && checkNewPosition gs DirRight (rightPos enemypos) && snd (newRight enemypos) = (fst (newRight enemypos), DirRight)
                        | otherwise = (centerPosition enemypos, DirNone)
            deltaY = py - ey
            deltaX = px - ex

centerPosition :: Position -> Position
centerPosition pos = fst (newEnemyPosInX (fst (newEnemyPosInY pos)))

randomEnemy :: GameState -> Direction -> Position -> (Position, Direction)
randomEnemy gstate currDir pos = if validNewTime 
                                    then case newDir of
                                          DirUp       -> if currDir /= DirDown && snd (newUp pos)
                                                            then (fst (newUp pos), DirUp)
                                                            else (pos, currDir)
                                          DirDown     -> if currDir /= DirUp && snd (newDown pos)
                                                            then (fst (newDown pos), DirDown)
                                                            else (pos, currDir)
                                          DirLeft     -> if currDir /= DirRight && snd (newRight pos)
                                                            then (fst (newRight pos), DirLeft)
                                                            else (pos, currDir)
                                          DirRight    -> if currDir /= DirLeft && snd (newLeft pos)
                                                            then (fst (newLeft pos), DirRight)
                                                            else (pos, currDir)
                                          _           -> (pos, currDir)
                                    else (pos, currDir)
      where _rng = rng gstate
            up = (checkNewPosition gstate DirUp (upPos pos), DirUp)
            down = (checkNewPosition gstate DirDown (downPos pos), DirDown)
            left = (checkNewPosition gstate DirLeft (leftPos pos), DirLeft)
            right = (checkNewPosition gstate DirRight (rightPos pos), DirRight)
            none = (True, DirNone)
            directions = [up, down, left, right, none]
            possibleDirections = filter ((==True).fst) directions
            newDir = snd (possibleDirections !! fst (randomR (0 :: Int, length possibleDirections - 1) _rng))
            validNewTime = frame gstate `mod` 30 == 0

newEnemyPosInX :: Position -> (Position, Bool)
newEnemyPosInX (ex, ey) | deltaX < 0.2 = ((fromInteger (floor ex), ey), True)
                        | deltaX > 0.8 = ((fromInteger (ceiling ex), ey), True)
                        | otherwise = ((ex, ey), False)
      where deltaX = ex - fromInteger (floor ex)

newEnemyPosInY :: Position -> (Position, Bool)
newEnemyPosInY (ex, ey) | deltaY < 0.2 = ((ex, fromInteger (floor ey)), True)
                        | deltaY > 0.8 = ((ex, fromInteger (ceiling ey)), True)
                        | otherwise = ((ex, ey) , False)
      where deltaY = ey - fromInteger (floor ey)

normalDirection :: GameState -> Direction -> Position -> Position -> EnemyType -> (Position, Direction)
normalDirection gs currDir _playerPos _enemyPos eType = case eType of
      GoToPlayer  -> lookForPlayer gs currDir _playerPos _enemyPos (<) (>)
      _           -> randomEnemy gs currDir _enemyPos

invertedDirection :: GameState -> Direction -> Position -> Position -> (Position, Direction)
invertedDirection gs currDir _playerPos _enemyPos = lookForPlayer gs currDir _playerPos _enemyPos (>) (<)

checkNewPosition :: GameState -> Direction -> Position -> Bool
checkNewPosition gstate dir (x, y) = case field of
                              WallField   -> False
                              _           -> True
      where _level = level gstate
            (field, _)  | dir == DirUp = (_level !! floor y) !! round x
                        | dir == DirDown = (_level !! ceiling y) !! round x
                        | dir == DirRight = (_level !! round y) !! ceiling x
                        | otherwise = (_level !! round y) !! floor x 

-- Function that checks if the Enemy is on the same position as the player                                              
isPlayerDead :: GameState -> [Int]
isPlayerDead gstate = elemIndices True checkPoss
      where (px, py) = playerPos (player gstate)
            enemyPoss = map enemyPos (enemies gstate)
            checkPos (ex, ey) | abs (px - ex) < 0.3 && abs (py - ey) < 0.3 = True
                              | otherwise = False
            checkPoss = map checkPos enemyPoss