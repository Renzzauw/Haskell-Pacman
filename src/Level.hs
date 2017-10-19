module Level where

-- Data types
data Field = Player | Wall | Point | BigPoint | Enemy | Empty

data Row = [Field]

data Level = [Row]

-- Function for converting a Char from the textfilefile to a Field
TextToField :: Char -> Field 
TextToField 'P' = Player
TextToField 'W' = Wall
TextToField '.' = Point
TextToField '*' = BigPoint
TextToField 'E' = Enemy
TextToField ' ' = Empty

