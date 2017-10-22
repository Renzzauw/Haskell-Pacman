module Level where

-- Data types
data Field = Player | Wall | Point | BigPoint | Enemy | Empty

type Row = [Field]
type Level = [Row]

-- Function for converting a Char from the textfile to a Field
textToField :: Char -> Field 
textToField 'P' = Player
textToField 'W' = Wall
textToField '.' = Point
textToField '*' = BigPoint
textToField 'E' = Enemy
textToField ' ' = Empty

loadLevel :: FilePath -> IO Level
loadLevel filePath = do
    text <- readFile filePath
    let rows = lines text
    return $ (map . map) textToField rows