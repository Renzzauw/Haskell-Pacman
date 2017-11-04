module Enemy where
    
import Level
import Model

-- Function that returns a Direction for the enemy to move into based on the player position
lookForPlayer :: GameState -> Direction -> Position -> Position -> (Position, Direction)
lookForPlayer gs currDir (px, py) enemypos@(ex, ey) = case currDir of
                              DirUp       ->    upOrDown
                              DirDown     ->    upOrDown
                              DirLeft     ->    leftOrRight
                              DirRight    ->    leftOrRight
                              _           ->    noDir
      where surroundings = getSurroundingFields gs enemypos
            newEnemyPosInX = if (ex - fromIntegral (floor ex)) < 0.2
                                    then (((fromIntegral (floor ex)), ey), True)
                                    else if ex - fromIntegral (floor ex) > 0.8
                                          then (((fromIntegral (ceiling ex)), ey), True)
                                          else ((ex, ey), False)                             -- wordt de hele tijd aangeroepen
            newEnemyPosInY = if (ey - fromIntegral (floor ey)) < 0.2
                                    then ((ex, (fromIntegral (floor ey))), True)
                                    else if ey - fromIntegral (floor ey) > 0.8
                                          then ((ex, (fromIntegral (ceiling ey))), True)
                                          else ((ex, ey) , False)                            -- wordt de hele tijd aangeroepen
            upOrDown    | py - ey < -0.05 && checkNewPosition gs DirUp (ex, ey - enemyVelocity) = (enemypos, DirUp)
                        | py - ey > 0.05 && checkNewPosition gs DirDown (ex, ey + enemyVelocity) = (enemypos, DirDown)
                        | px - ex < -0.05 && checkNewPosition gs DirLeft (ex - enemyVelocity, ey) && snd newEnemyPosInY = (fst newEnemyPosInY, DirLeft)
                        | px - ex > 0.05 && checkNewPosition gs DirRight (ex + enemyVelocity, ey) && snd newEnemyPosInY = (fst newEnemyPosInY, DirRight)
                        | otherwise = (enemypos, DirNone)
            leftOrRight | px - ex < -0.05 && checkNewPosition gs DirLeft (ex - enemyVelocity, ey) = (enemypos, DirLeft)
                        | px - ex > 0.05 && checkNewPosition gs DirRight (ex + enemyVelocity, ey) = (enemypos, DirRight)
                        | py - ey < -0.05 && checkNewPosition gs DirUp (ex, ey - enemyVelocity) && snd newEnemyPosInX = (fst newEnemyPosInX, DirUp)
                        | py - ey > 0.05 && checkNewPosition gs DirDown (ex, ey + enemyVelocity) && snd newEnemyPosInX = (fst newEnemyPosInX, DirDown)
                        | otherwise = (enemypos, DirNone)
            noDir       | py - ey < -0.05 && checkNewPosition gs DirUp (ex, ey - enemyVelocity) = (enemypos, DirUp)
                        | py - ey > 0.05 && checkNewPosition gs DirDown (ex, ey + enemyVelocity) = (enemypos, DirDown)
                        | px - ex < -0.05 && checkNewPosition gs DirLeft (ex - enemyVelocity, ey) = (enemypos, DirLeft)
                        | px - ex > 0.05 && checkNewPosition gs DirRight (ex + enemyVelocity, ey) = (enemypos, DirRight)
                        | otherwise = (enemypos, DirNone)
                        
{-setPlayerDirectionToUp :: GameState -> GameState
setPlayerDirectionToUp gstate = if (x - fromIntegral (floor x)) < 0.2 && checkUpperFieldFree (fromIntegral (floor x)) y
                                    then gstate { player = Player (newPlayerPos (fromIntegral (floor x)) y) DirUp }
                                    else if (x - fromIntegral (floor x)) > 0.8 && checkUpperFieldFree (fromIntegral (ceiling x)) y
                                          then gstate { player = Player (newPlayerPos (fromIntegral (ceiling x)) y) DirUp }
                                          else gstate
      where   _level = level gstate
            _player = player gstate
            _playerDir = playerDir _player
            (x, y) = playerPos _player
            newPlayerPos _x _y = (_x, _y)
            checkUpperFieldFree x y = checkNewPlayerPosition gstate (newPlayerPos x (y - 1))-}
      
                                                {-if py < ey
                                                      then  if surroundings !! 0 /= WallField 
                                                                  then DirUp
                                                                  else  if px < ex
                                                                              then  if surroundings !! 2 /= WallField
                                                                                          then DirLeft
                                                                                          else DirNone
                                                                              else DirNone
                                                                  else  if px > ex
                                                                              then  if surroundings !! 3 /= WallField
                                                                                          then DirRight
                                                                                          else DirNone
                                                                              else DirNone
                                                                  else DirNone
                                                      else  if py > ey
                                                                  then  if surroundings !! 1 /= WallField
                                                                              then DirDown
                                                                              else  if px < ex
                                                                                    then  if surroundings !! 2 /= WallField
                                                                                                then DirLeft
                                                                                                else DirNone
                                                                                    else DirNone
                                                                              else  if px > ex
                                                                                    then  if surroundings !! 3 /= WallField
                                                                                                then DirRight
                                                                                                else DirNone
                                                                                    else DirNone
                                                                              else DirNone
                                                                  else DirNone
                                                      else  if px < ex
                                                                  then  if surroundings !! 2 /= WallField
                                                                              then  DirLeft
                                                                              else  DirNone
                                                                  else DirNone
                                                      else  if px > ex
                                                                  then  if surroundings !! 3 /= WallField
                                                                              then  DirRight
                                                                              else  DirNone
                                                                  else DirNone
                                                      else DirNone
      where surroundings = getSurroundingFields gs enemypos-}
                                                            
                                                                  

{-| py < ey && surroundings !! 0 == WallField = DirNone
                                                | py < ey && surroundings !! 0 /= WallField = DirUp
                                                | py > ey && surroundings !! 1 == WallField = DirNone
                                                | py > ey && surroundings !! 1 /= WallField = DirDown
                                                | px < ex && surroundings !! 2 == WallField = DirNone
                                                | px < ex && surroundings !! 2 /= WallField = DirLeft
                                                | px > ex && surroundings !! 3 == WallField = DirNone
                                                | px > ex && surroundings !! 3 /= WallField = DirRight
                                                | otherwise                                 = DirNone
      where surroundings = getSurroundingFields gs enemypos-}
                                                   
-- Function that returns a list of Points that surround a given Point
getSurroundingFields :: GameState -> Position -> [FieldType]
getSurroundingFields gs (x, y) = map fst fieldList
                                        where   _level = level gs
                                                up    = (_level !! ((floor y) - 1)) !! round x
                                                left  = (_level !! (round y)) !! ((floor x) - 1)
                                                right = (_level !! (round y)) !! ((ceiling x) + 1)
                                                down  = (_level !! ((ceiling y) + 1)) !! round x
                                                fieldList   = [up, down, left, right]

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

{-checkNewPlayerPosition :: GameState -> Position -> Bool
checkNewPlayerPosition gstate (x, y) = case field of
                                    WallField   -> False
                                    _           -> True
      where   _level = level gstate
            _playerDir = playerDir (player gstate)
            (field, _) = if _playerDir == DirUp
                              then (_level !! floor y) !! round x
                              else if _playerDir == DirDown
                                    then (_level !! ceiling y) !! round x
                                    else if _playerDir == DirRight
                                    then (_level !! round y) !! ceiling x
                                    else (_level !! round y) !! floor x-}    

-- Function that checks if the Enemy is on the same position as the player                                              
isPlayerDead :: GameState -> Bool
isPlayerDead gstate = elem True checkPoss
      where (px, py) = playerPos (player gstate)
            enemyPoss = map enemyPos (enemies gstate)
            checkPos (ex, ey) | abs (px - ex) < 0.3 && abs (py - ey) < 0.3 = True
                              | otherwise = False
            checkPoss = map checkPos enemyPoss