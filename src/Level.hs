module Level where

-- Data types
data Field = Player | Wall | Point | BigPoint | Enemy | Empty

data Row = [Field]

data Level = [Row]

-- Function for converting a Char from the textfilefile to a Field
textToField :: Char -> Field 
textToField 'P' = Player
textToField 'W' = Wall
textToField '.' = Point
textToField '*' = BigPoint
textToField 'E' = Enemy
textToField ' ' = Empty

