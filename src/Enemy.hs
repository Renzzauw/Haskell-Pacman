module Enemy where
    
import Level
import Model
    
    {-
    -- Function that returns a list of all the possible combinations of movements to get to the player
    calculatePathsToPlayer :: Point -> Point -> [[Point]]
    calculatePathsToPlayer playerPos@(Pt pX pY) enemyPos@(Pt eX eY) | playerPos == enemyPos               = []
                                                                    | elem playerPos surroundings 
                                                                    | otherwise                           = map calculatePathsToPlayer surroundings
                                                                where surroundings = getSurroundingFields enemyPos
    
    -}
    
    -- Function that returns a list of all the possible combinations of movements to get to the player
calculatePathsToPlayer :: Position -> Position -> [[Position]]
calculatePathsToPlayer playerPos@(pX, pY) enemyPos@(eX, eY) = undefined
    {-
    -- Function that zips a path with their field types                                                            
    zipPointAndFieldType :: Level -> Point -> (Field, Point)
    zipPointAndFieldType level point@(Pt x y) = (field, point)
                                    where field = ((level !! x) !! y)
    -}
    
    
    -- Function that returns a list of Points that surround a given Point
getSurroundingFields :: GameState -> Field -> [Field]
getSurroundingFields gs (_, (x, y)) = filter (isValidMoveField gs) fieldList
                                        where topLeft     = ((getFieldType gs ((x - 1), (y - 1))), ((x - 1), (y - 1))) --YO MOET DIT - OF + ZIJN IVM AXIS BEGIN
                                              topMid      = ((getFieldType gs (x, (y - 1))), (x, (y - 1)))
                                              topRight    = ((getFieldType gs ((x + 1), (y - 1))), ((x + 1), (y - 1)))
                                              midLeft     = ((getFieldType gs ((x - 1), y)), ((x - 1), y))
                                              midRight    = ((getFieldType gs ((x + 1), y)), ((x + 1), y))
                                              bottomLeft  = ((getFieldType gs ((x - 1), (y + 1))), ((x - 1), (y + 1)))
                                              bottomMid   = ((getFieldType gs (x, (y + 1))), (x, (y + 1)))
                                              bottomRight = ((getFieldType gs ((x + 1), (y + 1))), ((x + 1), (y + 1)))
                                              fieldList   = [topLeft, topMid, topRight, midLeft, midRight, bottomLeft, bottomMid, bottomRight]
 
getFieldType :: GameState -> Position -> FieldType
getFieldType gs (x, y) = fst (((level gs) !! round y) !! round x)

-- Function that returns the shortest path from all possible paths from the enemy to the player                                   
getShortestPath :: [[Position]] -> [Position]
getShortestPath points = minimum points                                  
    
isPlayerDead :: Position -> Position -> Bool 
isPlayerDead player enemy   | player == enemy = True
                            | otherwise       = False
    
    -- Function that checks if a Field is valid to move to for the enemy
isValidMoveField :: GameState -> Field -> Bool
isValidMoveField gs (fieldType, (x, y)) | fieldType == WallField                          = False -- Check if field is a wall
                                        | x > 0 && x < maxWidth && y > 0 && y < maxHeight = True  -- doublecheck for movement outside of bounds
                                        | otherwise                                       = False
                                        where lvl       = level gs
                                              maxWidth  = fromIntegral (length (lvl !! 0))
                                              maxHeight = fromIntegral (length lvl)
       
    
    {-
    update :: GameState -> GameState
    update = do 
        let x = 
        x <- iets i,n een monad
        case x of 1
            
        let 
    -}
    
    {-
    checkFieldInDirection :: Level -> Direction -> Field
    checkFieldInDirection DirUp    = -- Methode om het veld boven de enemy te checken wat dat is, return deze directie niet als de enemy daar niet heen kan, anders wel.
                          DirDown  =
                          DirLeft  = 
                          DirRight =
                          _        = -- Return huidig Field
    -}