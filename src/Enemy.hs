module Enemy where
    
import Level
import Model

-- Function that returns a Direction for the enemy to move into based on the player position
lookForPlayer :: GameState -> Position -> Position -> Direction
lookForPlayer gs playerpos@(px, py) enemypos@(ex, ey) | py < ey && surroundings !! 0 == WallField = DirNone
                                                      | py < ey && surroundings !! 0 /= WallField = DirUp
                                                      | py > ey && surroundings !! 1 == WallField = DirNone
                                                      | py > ey && surroundings !! 1 /= WallField = DirDown
                                                      | px < ex && surroundings !! 2 == WallField = DirNone
                                                      | px < ex && surroundings !! 2 /= WallField = DirLeft
                                                      | px > ex && surroundings !! 3 == WallField = DirNone
                                                      | px > ex && surroundings !! 3 /= WallField = DirRight
                                                      | otherwise                                 = DirNone
                                                    where surroundings = getSurroundingFields gs enemypos
                                                   
-- Function that returns a list of Points that surround a given Point
getSurroundingFields :: GameState -> Position -> [FieldType]
getSurroundingFields gs (x, y) = map fst (filter (isValidMoveField gs) fieldList)
                                        where up    = ((getFieldType gs (x, (y - 1))), (x, (y - 1)))                                             
                                              left  = ((getFieldType gs ((x - 1), y)), ((x - 1), y))
                                              right = ((getFieldType gs ((x + 1), y)), ((x + 1), y))
                                              down  = ((getFieldType gs (x, (y + 1))), (x, (y + 1)))
                                              fieldList   = [up, down, left, right]

-- Function that returns the FieldType of a field at a given position                                              
getFieldType :: GameState -> Position -> FieldType
getFieldType gs (x, y) = fst (((level gs) !! round y) !! round x)
    
-- Function that checks if a Field is valid to move to for the enemy
isValidMoveField :: GameState -> Field -> Bool
isValidMoveField gs (fieldType, (x, y)) | fieldType == WallField                          = False -- Check if field is a wall
                                        | x > 0 && x < maxWidth && y > 0 && y < maxHeight = True  -- doublecheck for movement outside of bounds
                                        | otherwise                                       = False
                                        where lvl       = level gs
                                              maxWidth  = fromIntegral (length (lvl !! 0))
                                              maxHeight = fromIntegral (length lvl)

-- Function that checks if the Enemy is on the same position as the player                                              
isPlayerDead :: Position -> Position -> Bool
isPlayerDead player enemy | player == enemy = True
                          | otherwise       = False
       