module Enemy where
    
import Level
import Model

-- Function that returns a Direction for the enemy to move into based on the player position
lookForPlayer :: GameState -> Position -> Position -> Direction
lookForPlayer gs (px, py) enemypos@(ex, ey)
      | py - ey < -0.05 && surroundings !! 0 /= WallField = DirUp
      | py - ey > 0.05 && surroundings !! 1 /= WallField = DirDown
      | px - ex < -0.05 && surroundings !! 2 /= WallField = DirLeft
      | px - ex > 0.05 && surroundings !! 3 /= WallField = DirRight
      | otherwise = DirNone
      where surroundings = getSurroundingFields gs enemypos
      
      
      
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
                                                up    = (_level !! (floor y)) !! round x
                                                left  = (_level !! (round y)) !! floor x
                                                right = (_level !! (round y)) !! ceiling x
                                                down  = (_level !! (ceiling y)) !! round x
                                                fieldList   = [up, down, left, right]
    
-- Function that checks if a Field is valid to move to for the enemy
isValidMoveField :: GameState -> Field -> Bool
isValidMoveField gs (fieldType, (x, y)) | fieldType == WallField                          = False -- Check if field is a wall
                                        | x > 0 && x < maxWidth && y > 0 && y < maxHeight = True  -- doublecheck for movement outside of bounds
                                        | otherwise                                       = False
                                        where lvl       = level gs
                                              maxWidth  = fromIntegral (length (head lvl))
                                              maxHeight = fromIntegral (length lvl)

-- Function that checks if the Enemy is on the same position as the player                                              
isPlayerDead :: Position -> Position -> Bool
isPlayerDead player enemy | player == enemy = True
                          | otherwise       = False
       