module Enemy where

-- New data types
data Point = Pt Int Int 
    deriving (Eq)
data Path = [Point]
    deriving (Eq)

-- Function that returns a list of all the possible combinations of movements to get to the player
calculatePathsToPlayer :: Point -> Point -> [[Point]]
calculatePathsToPlayer playerPos@(Pt pX pY) enemyPos@(Pt eX eY) | playerPos == enemyPos               = []
                                                                | otherwise                           = map calculatePathsToPlayer surroundings
                                                            where surroundings = getSurroundingFields enemyPos

-- Function that returns a list of Points that surround a given Point
getSurroundingFields :: Point -> [Point]
getSurroundingFields (Pt x y) = map filter (>= (Pt 0 0)) pointList
                            where left        = x - 1
                                  right       = x + 1
                                  top         = y - 1
                                  bottom      = y + 1
                                  topLeft     = Pt left top
                                  topMid      = Pt x top
                                  topRight    = Pt right top
                                  midLeft     = Pt left y 
                                  midRight    = Pt right y 
                                  bottomLeft  = Pt left bottom
                                  bottomMid   = Pt x bottom
                                  bottomRight = Pt right bottom
                                  pointList   = [topLeft, topMid, topRight, midLeft, midRight, bottomLeft, bottomMid, bottomRight]

-- Function that returns the shortest path from all possible paths from the enemy to the player                                   
getShortestPath :: [[Point]] -> [Point]
getShortestPath points = minimum points                                  

{-
checkFieldInDirection :: Level -> Direction -> Field
checkFieldInDirection DirUp    = -- Methode om het veld boven de enemy te checken wat dat is, return deze directie niet als de enemy daar niet heen kan, anders wel.
                      DirDown  =
                      DirLeft  = 
                      DirRight =
                      _        = -- Return huidig Field
-}