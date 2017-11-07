module Enemy where
    
import Level
import Model

import System.Random

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
      where upOrDown    | deltaY `firstOp` (-0.05) && checkNewPosition gs DirUp upPos && currDir /= DirDown = (enemypos, DirUp)
                        | deltaY `secondOp` 0.05 && checkNewPosition gs DirDown downPos && currDir /= DirUp = (enemypos, DirDown)
                        | deltaX `firstOp` (-0.05) && checkNewPosition gs DirLeft leftPos && snd newLeft = (fst newLeft, DirLeft)
                        | deltaX `secondOp` 0.05 && checkNewPosition gs DirRight rightPos && snd newRight = (fst newRight, DirRight)
                        | deltaY `firstOp` (-1) && checkNewPosition gs DirUp upPos == False && checkNewPosition gs DirLeft leftPos && snd newLeft = (fst newLeft, DirLeft)
                        | deltaY `firstOp` (-1) && checkNewPosition gs DirUp upPos == False && checkNewPosition gs DirRight rightPos && snd newRight = (fst newRight, DirRight)
                        | deltaY `secondOp` 1 && checkNewPosition gs DirDown downPos == False && checkNewPosition gs DirLeft leftPos && snd newLeft = (fst newLeft, DirLeft)
                        | deltaY `secondOp` 1 && checkNewPosition gs DirDown downPos == False && checkNewPosition gs DirRight rightPos && snd newRight = (fst newRight, DirRight)
                        | currDir == DirUp && checkNewPosition gs DirUp upPos = (enemypos, DirUp)
                        | currDir == DirDown && checkNewPosition gs DirDown downPos = (enemypos, DirDown)
                        | otherwise = (enemypos, DirNone)
            leftOrRight | deltaX `firstOp` (-0.05) && checkNewPosition gs DirLeft leftPos && currDir /= DirRight = (enemypos, DirLeft)
                        | deltaX `secondOp` 0.05 && checkNewPosition gs DirRight rightPos && currDir /= DirLeft = (enemypos, DirRight)
                        | deltaY `firstOp` (-0.05) && checkNewPosition gs DirUp upPos && snd newUp = (fst newUp, DirUp)
                        | deltaY `secondOp` 0.05 && checkNewPosition gs DirDown downPos && snd newDown = (fst newDown, DirDown)
                        | deltaX `firstOp` (-1) && checkNewPosition gs DirLeft leftPos == False && checkNewPosition gs DirUp upPos && snd newUp = (fst newUp, DirUp)
                        | deltaX `firstOp` (-1) && checkNewPosition gs DirLeft leftPos == False && checkNewPosition gs DirDown downPos && snd newDown = (fst newDown, DirDown)
                        | deltaX `secondOp` 1 && checkNewPosition gs DirRight rightPos == False && checkNewPosition gs DirUp upPos && snd newUp = (fst newUp, DirUp)
                        | deltaX `secondOp` 1 && checkNewPosition gs DirRight rightPos == False && checkNewPosition gs DirDown downPos && snd newDown = (fst newDown, DirDown)
                        | currDir == DirRight && checkNewPosition gs DirRight rightPos = (enemypos, DirRight)
                        | currDir == DirLeft && checkNewPosition gs DirLeft leftPos = (enemypos, DirLeft)
                        | otherwise = (enemypos, DirNone)
            noDir       | deltaY `firstOp` (-0.05) && checkNewPosition gs DirUp upPos && snd newUp = (fst newUp, DirUp)
                        | deltaY `secondOp` 0.05 && checkNewPosition gs DirDown downPos && snd newDown = (fst newDown, DirDown)
                        | deltaX `firstOp` (-0.05) && checkNewPosition gs DirLeft leftPos && snd newLeft = (fst newLeft, DirLeft)
                        | deltaX `secondOp` 0.05 && checkNewPosition gs DirRight rightPos && snd newRight = (fst newRight, DirRight)
                        | otherwise = (centerPosition enemypos, DirNone)
            upPos = (ex, ey - enemyVelocity)
            downPos = (ex, ey + enemyVelocity)
            leftPos = (ex - enemyVelocity, ey)
            rightPos = (ex + enemyVelocity, ey)
            newUp = newEnemyPosInX upPos
            newDown = newEnemyPosInX downPos
            newLeft = newEnemyPosInY leftPos
            newRight = newEnemyPosInY rightPos
            deltaY = py - ey
            deltaX = px - ex

centerPosition :: Position -> Position
centerPosition pos = fst (newEnemyPosInX (fst (newEnemyPosInY pos)))

randomEnemy :: GameState -> Direction -> Position -> (Position, Direction)
randomEnemy gstate currDir pos@(ex, ey) = if validNewTime 
                                                then case newDir of
                                                      DirUp       -> if currDir /= DirDown && snd newUp
                                                                        then (fst newUp, DirUp)
                                                                        else (pos, currDir)
                                                      DirDown     -> if currDir /= DirUp && snd newDown
                                                                        then (fst newDown, DirDown)
                                                                        else (pos, currDir)
                                                      DirLeft     -> if currDir /= DirRight && snd newRight
                                                                        then (fst newRight, DirLeft)
                                                                        else (pos, currDir)
                                                      DirRight    -> if currDir /= DirLeft && snd newLeft
                                                                        then (fst newLeft, DirRight)
                                                                        else (pos, currDir)
                                                      _           -> (pos, currDir)
                                                else (pos, currDir)
      where _rng = rng gstate
            up = (checkNewPosition gstate DirUp upPos, DirUp)
            down = (checkNewPosition gstate DirDown downPos, DirDown)
            left = (checkNewPosition gstate DirLeft leftPos, DirLeft)
            right = (checkNewPosition gstate DirRight rightPos, DirRight)
            none = (True, DirNone)
            directions = [up, down, left, right, none]
            possibleDirections = filter ((==True).fst) directions
            newDir = snd (possibleDirections !! (fst (randomR (0 :: Int, length possibleDirections - 1) _rng)))
            validNewTime = (frame gstate) `mod` 30 == 0
            upPos = (ex, ey - enemyVelocity)
            downPos = (ex, ey + enemyVelocity)
            leftPos = (ex - enemyVelocity, ey)
            rightPos = (ex + enemyVelocity, ey)
            newUp = newEnemyPosInX upPos
            newDown = newEnemyPosInX downPos
            newLeft = newEnemyPosInY leftPos
            newRight = newEnemyPosInY rightPos

newEnemyPosInX :: Position -> (Position, Bool)
newEnemyPosInX (ex, ey) = if (ex - fromInteger (floor ex)) < 0.2
                              then (((fromInteger (floor ex)), ey), True)
                              else if ex - fromInteger (floor ex) > 0.8
                                    then (((fromInteger (ceiling ex)), ey), True)
                                    else ((ex, ey), False)

newEnemyPosInY :: Position -> (Position, Bool)
newEnemyPosInY (ex, ey) = if (ey - fromInteger (floor ey)) < 0.2
                              then ((ex, (fromInteger (floor ey))), True)
                              else if ey - fromInteger (floor ey) > 0.8
                                    then ((ex, (fromInteger (ceiling ey))), True)
                                    else ((ex, ey) , False)

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
            (field, _) = if dir == DirUp
                              then (_level !! floor y) !! round x
                              else if dir == DirDown
                                    then (_level !! ceiling y) !! round x
                                    else if dir == DirRight
                                          then (_level !! round y) !! ceiling x
                                          else (_level !! round y) !! floor x 

-- Function that checks if the Enemy is on the same position as the player                                              
isPlayerDead :: GameState -> Bool
isPlayerDead gstate = elem True checkPoss
      where (px, py) = playerPos (player gstate)
            enemyPoss = map enemyPos (enemies gstate)
            checkPos (ex, ey) | abs (px - ex) < 0.3 && abs (py - ey) < 0.3 = True
                              | otherwise = False
            checkPoss = map checkPos enemyPoss