module Level where

-- Data types
data Field = Player | Wall | Point | BigPoint | Enemy | Empty

type Row = [Field]

type Level = [Row]

-- Function for converting a Char from the textfilefile to a Field
textToField :: Char -> Field 
textToField 'P' = Player
textToField 'W' = Wall
textToField '.' = Point
textToField '*' = BigPoint
textToField 'E' = Enemy
textToField ' ' = Empty

{-loadLevel :: FilePath -> Level
loadLevel filePath = do
    text <- readFile filePath
    let rows = lines text
    (map . map) textToField rows-}