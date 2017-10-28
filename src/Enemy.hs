module Enemy where
{-
data Point = Pt Int Int 
    deriving (Eq)
data Path = [Point]
    deriving (Eq)

-- Function that returns a list of all the possible combinations of movements to get to the player
calculatePathsToPlayer :: Point -> Point -> [[Direction]]
calculatePathsToPlayer playerPos@(Pt pX pY) enemyPos@(Pt eX eY) | playerPos == enemyPos = []
                                                                | 

checkFieldInDirection :: Level -> Direction -> Field
checkFieldInDirection DirUp    = -- Methode om het veld boven de enemy te checken wat dat is, return deze directie niet als de enemy daar niet heen kan, anders wel.
                      DirDown  =
                      DirLeft  = 
                      DirRight =
                      _        = -- Return huidig Field
-}