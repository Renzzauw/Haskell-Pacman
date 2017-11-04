module Enemy where
    
import Level
import Model

-- Function that returns a Direction for the enemy to move into based on the player position
lookForPlayer :: GameState -> Direction -> Position -> Position -> (Float -> Float -> Bool) -> (Float -> Float -> Bool) -> (Position, Direction)
lookForPlayer gs currDir (px, py) enemypos@(ex, ey) firstOp secondOp = case currDir of
                              DirUp       ->    upOrDown
                              DirDown     ->    upOrDown
                              DirLeft     ->    leftOrRight
                              DirRight    ->    leftOrRight
                              _           ->    noDir
      where newEnemyPosInX = if (ex - fromIntegral (floor ex)) < 0.2
                                    then (((fromIntegral (floor ex)), ey), True)
                                    else if ex - fromIntegral (floor ex) > 0.8
                                          then (((fromIntegral (ceiling ex)), ey), True)
                                          else ((ex, ey), False)                             -- wordt de hele tijd aangeroepen
            newEnemyPosInY = if (ey - fromIntegral (floor ey)) < 0.2
                                    then ((ex, (fromIntegral (floor ey))), True)
                                    else if ey - fromIntegral (floor ey) > 0.8
                                          then ((ex, (fromIntegral (ceiling ey))), True)
                                          else ((ex, ey) , False)                            -- wordt de hele tijd aangeroepen
            upOrDown    | firstOp (py - ey) (-0.05) && checkNewPosition gs DirUp (ex, ey - enemyVelocity) = (enemypos, DirUp)
                        | secondOp (py - ey) 0.05 && checkNewPosition gs DirDown (ex, ey + enemyVelocity) = (enemypos, DirDown)
                        | firstOp (px - ex) (-0.05) && checkNewPosition gs DirLeft (ex - enemyVelocity, ey) && snd newEnemyPosInY = (fst newEnemyPosInY, DirLeft)
                        | secondOp (px - ex) 0.05 && checkNewPosition gs DirRight (ex + enemyVelocity, ey) && snd newEnemyPosInY = (fst newEnemyPosInY, DirRight)
                        | otherwise = (enemypos, DirNone)
            leftOrRight | firstOp (px - ex) (-0.05) && checkNewPosition gs DirLeft (ex - enemyVelocity, ey) = (enemypos, DirLeft)
                        | secondOp (px - ex) 0.05 && checkNewPosition gs DirRight (ex + enemyVelocity, ey) = (enemypos, DirRight)
                        | firstOp (py - ey) (-0.05) && checkNewPosition gs DirUp (ex, ey - enemyVelocity) && snd newEnemyPosInX = (fst newEnemyPosInX, DirUp)
                        | secondOp (py - ey) 0.05 && checkNewPosition gs DirDown (ex, ey + enemyVelocity) && snd newEnemyPosInX = (fst newEnemyPosInX, DirDown)
                        | otherwise = (enemypos, DirNone)
            noDir       | firstOp (py - ey) (-0.05) && checkNewPosition gs DirUp (ex, ey - enemyVelocity) = (enemypos, DirUp)
                        | secondOp (py - ey) 0.05 && checkNewPosition gs DirDown (ex, ey + enemyVelocity) = (enemypos, DirDown)
                        | firstOp (px - ex) (-0.05) && checkNewPosition gs DirLeft (ex - enemyVelocity, ey) = (enemypos, DirLeft)
                        | secondOp (px - ex) 0.05 && checkNewPosition gs DirRight (ex + enemyVelocity, ey) = (enemypos, DirRight)
                        | otherwise = (enemypos, DirNone)

normalDirection :: GameState -> Direction -> Position -> Position -> (Position, Direction)
normalDirection gs currDir playerPos enemyPos = lookForPlayer gs currDir playerPos enemyPos (<) (>)

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