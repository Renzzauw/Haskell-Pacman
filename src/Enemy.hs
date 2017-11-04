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
      where upOrDown    | firstOp (py - ey) (-0.05) && checkNewPosition gs DirUp upPos = (enemypos, DirUp)
                        | secondOp (py - ey) 0.05 && checkNewPosition gs DirDown downPos = (enemypos, DirDown)
                        | firstOp (px - ex) (-0.05) && checkNewPosition gs DirLeft leftPos && snd (newEnemyPosInY leftPos) = (fst (newEnemyPosInY leftPos), DirLeft)
                        | secondOp (px - ex) 0.05 && checkNewPosition gs DirRight rightPos && snd (newEnemyPosInY rightPos) = (fst (newEnemyPosInY rightPos), DirRight)
                        | otherwise = (enemypos, DirNone)
            leftOrRight | firstOp (px - ex) (-0.05) && checkNewPosition gs DirLeft leftPos = (enemypos, DirLeft)
                        | secondOp (px - ex) 0.05 && checkNewPosition gs DirRight rightPos = (enemypos, DirRight)
                        | firstOp (py - ey) (-0.05) && checkNewPosition gs DirUp upPos && snd (newEnemyPosInX upPos) = (fst (newEnemyPosInX upPos), DirUp)
                        | secondOp (py - ey) 0.05 && checkNewPosition gs DirDown downPos && snd (newEnemyPosInX downPos) = (fst (newEnemyPosInX downPos), DirDown)
                        | otherwise = (enemypos, DirNone)
            noDir       | firstOp (py - ey) (-0.05) && checkNewPosition gs DirUp upPos = (enemypos, DirUp)
                        | secondOp (py - ey) 0.05 && checkNewPosition gs DirDown downPos = (enemypos, DirDown)
                        | firstOp (px - ex) (-0.05) && checkNewPosition gs DirLeft leftPos = (enemypos, DirLeft)
                        | secondOp (px - ex) 0.05 && checkNewPosition gs DirRight rightPos = (enemypos, DirRight)
                        | otherwise = (enemypos, DirNone)
            upPos = (ex, ey - enemyVelocity)
            downPos = (ex, ey + enemyVelocity)
            leftPos = (ex - enemyVelocity, ey)
            rightPos = (ex + enemyVelocity, ey)

randomEnemy :: GameState -> Direction -> Position -> (Position, Direction)
randomEnemy gstate currDir pos@(ex, ey) = case newDir of
            DirUp       -> if currDir /= DirDown && validNewTime 
                              then (fst (newEnemyPosInX (ex, ey - enemyVelocity)), DirUp)
                              else (pos, currDir)
            DirDown     -> if currDir /= DirUp && validNewTime
                              then (fst (newEnemyPosInX (ex, ey + enemyVelocity)), DirDown)
                              else (pos, currDir)
            DirLeft     -> if currDir /= DirRight && validNewTime
                              then (fst (newEnemyPosInY (ex - enemyVelocity, ey)), DirLeft)
                              else (pos, currDir)
            DirRight    -> if currDir /= DirLeft && validNewTime
                              then (fst (newEnemyPosInY (ex + enemyVelocity, ey)), DirRight)
                              else (pos, currDir)
            _           -> (pos, currDir)
      where _rng = rng gstate
            up = (checkNewPosition gstate DirUp (ex, ey - enemyVelocity), DirUp)
            down = (checkNewPosition gstate DirDown (ex, ey + enemyVelocity), DirDown)
            left = (checkNewPosition gstate DirLeft (ex - enemyVelocity, ey), DirLeft)
            right = (checkNewPosition gstate DirRight (ex + enemyVelocity, ey), DirRight)
            none = (True, DirNone)
            directions = [up, down, left, right, none]
            possibleDirections = filter ((==True).fst) directions
            newDir = snd (possibleDirections !! (fst (randomR (0 :: Int, length possibleDirections - 1) _rng)))
            validNewTime = (frame gstate) `mod` 30 == 0

newEnemyPosInX :: Position -> (Position, Bool)
newEnemyPosInX (ex, ey) = if (ex - fromIntegral (floor ex)) < 0.2
                              then (((fromIntegral (floor ex)), ey), True)
                              else if ex - fromIntegral (floor ex) > 0.8
                                    then (((fromIntegral (ceiling ex)), ey), True)
                                    else ((ex, ey), False)

newEnemyPosInY :: Position -> (Position, Bool)
newEnemyPosInY (ex, ey) = if (ey - fromIntegral (floor ey)) < 0.2
                              then ((ex, (fromIntegral (floor ey))), True)
                              else if ey - fromIntegral (floor ey) > 0.8
                                    then ((ex, (fromIntegral (ceiling ey))), True)
                                    else ((ex, ey) , False)

normalDirection :: GameState -> Direction -> Position -> Position -> EnemyType -> (Position, Direction)
normalDirection gs currDir playerPos enemyPos eType = case eType of
      GoToPlayer  -> lookForPlayer gs currDir playerPos enemyPos (<) (>)
      _           -> randomEnemy gs currDir enemyPos

invertedDirection :: GameState -> Direction -> Position -> Position -> (Position, Direction)
invertedDirection gs currDir playerPos enemyPos = lookForPlayer gs currDir playerPos enemyPos (>) (<)

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