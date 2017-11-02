module Enemy (zipPointAndFieldType) where
    
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
    calculatePathsToPlayer :: Position -> Position -> [[Point]]
    calculatePathsToPlayer playerPos@(Pt pX pY) enemyPos@(Pt eX eY) = undefined
    {-
    -- Function that zips a path with their field types                                                            
    zipPointAndFieldType :: Level -> Point -> (Field, Point)
    zipPointAndFieldType level point@(Pt x y) = (field, point)
                                    where field = ((level !! x) !! y)
    -}
    
    -- Function that checks if a Field is valid to move to for the enemy
    isValidMoveField :: Field -> Bool
    isValidMoveField (Field (Pt x y) FieldType) | FieldType == WallField                                                           = False -- Check if field is a wall
                                                | x > 0 && x < getLevelWidth currlevel && y > 0 && getLevelHeight < getLevelHeight = True  -- doublecheck for movement outside of bounds
                                                | otherwise   
    
    -- Function that returns a list of Points that surround a given Point
    getSurroundingFields :: GameState -> Field -> [Field]
    getSurroundingFields gs (type, (Pt x y)) = filter isValidMoveField currlevel pointList
                                        where topLeft     = ((getFieldType gs (Pt (x - 1) (y - 1))), (Pt (x - 1) (y - 1))) --YO MOET DIT - OF + ZIJN IVM AXIS BEGIN
                                              topMid      = ((getFieldType gs (Pt x (y - 1))), (Pt x (y - 1)))
                                              topRight    = ((getFieldType gs (Pt (x + 1) (y - 1))), (Pt (x + 1) (y - 1)))
                                              midLeft     = ((getFieldType gs (Pt (x - 1) y)), (Pt (x - 1) y))
                                              midRight    = ((getFieldType gs (Pt (x + 1) y)), (Pt (x + 1) y))
                                              bottomLeft  = ((getFieldType gs (Pt (x - 1) (y + 1))), (Pt (x - 1) (y + 1)))
                                              bottomMid   = ((getFieldType gs (Pt x (y + 1))), (Pt x (y + 1)))
                                              bottomRight = ((getFieldType gs (Pt (x + 1) (y + 1))) , (Pt (x + 1) (y + 1)))
                                              pointList   = [topLeft, topMid, topRight, midLeft, midRight, bottomLeft, bottomMid, bottomRight]
    
    -- Function that returns the shortest path from all possible paths from the enemy to the player                                   
    getShortestPath :: [[Position]] -> [Position]
    getShortestPath points = minimum points                                  
    
    isPlayerDead :: Position -> Position -> Bool 
    isPlayerDead player enemy | player == enemy = True
                              | otherwise       = False
    
    -- Function that checks if a Field is valid to move to for the enemy
    isValidMoveField :: Field -> Bool
    isValidMoveField (Field (Pt x y) FieldType) | FieldType == WallField                                                           = False -- Check if field is a wall
                                                | x > 0 && x < getLevelWidth currlevel && y > 0 && getLevelHeight < getLevelHeight = True  -- doublecheck for movement outside of bounds
                                                | otherwise                                                                        = False
    
    
    
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