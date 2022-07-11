module HidatoCommon where

import Utils

data Cell a = Cell 
    {
        position :: (Int, Int),
        value :: a
    }

type CellTable = [[Cell Int]]

data HidatoBoard = Board 
    { board :: [[Int]]
    , minVal :: Int
    , maxVal :: Int
    } | Empty deriving (Show, Eq)

-----------------------------------------------------------

convertToHidatoBoard :: [[Int]] -> HidatoBoard
convertToHidatoBoard table = Board table (minimo) (maximo)
    where
        minimo = foldl1 (min) $ getNumbers table
        maximo = foldl1 (max) $ getNumbers table

-- convertToCellTable :: [[Int]] -> CellTable
-- convertToCellTable table = iter

-----------------------------------------------------------

showBoard :: HidatoBoard -> String
showBoard hidatoBoard = showCells cellList 0 0 (length cellList) (length line) maxSpace
    where
        cellList = board hidatoBoard
        line = head cellList
        maxSpace = length (show (maxVal hidatoBoard))

-------------------------- Returns the cells in a String --------------------------
showCells :: [[Int]] -> Int -> Int-> Int-> Int -> Int -> String
showCells cellList pRow pCol endRow endCol maxSpace
    | pRow == endRow = ""
    | pCol == endCol = "\n" ++ showCells cellList (pRow + 1) 0 endRow (length line) maxSpace
    | otherwise = spaces ++ element ++ showCells cellList pRow (pCol + 1) endRow endCol maxSpace
        where
            line = cellList !! (pRow + 1)
            el =  cellList !! pRow !! pCol
            element = if el == -1 then " " else show el
            spaces =  concat [" " | _ <- [0..(maxSpace - length element)]]

------------------------------------------------------------------------------------

-- 
validBoard :: HidatoBoard -> Bool
validBoard hidatoBoard = zerosCount == realZerosCount
    where
        -- En el getNumbers se tiene que restar 2 porque cuenta al mayor y menor elemento
        -- y se resta 1 a la resta (maxVal - minVal) para saber la cantidad de números
        -- con los que se debería llegar de minVal a maxVal
        zerosCount = (maxVal hidatoBoard) - (minVal hidatoBoard) - length (getNumbers (board hidatoBoard)) + 1
        realZerosCount = getZerosCount $ board hidatoBoard

validInitialBoard :: HidatoBoard -> Bool
validInitialBoard hidatoBoard = validBoard hidatoBoard && isUnique numbers
    where
        numbers = getNumbers $ board hidatoBoard

--------------------------

directions :: [(Int, Int)]
directions = [(0,1),(1,0),(0,-1),(-1,0),(1,1),(1,-1),(-1,1),(-1,-1)]

directionFilter :: [[Int]] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
directionFilter table (x,y) moves = do
    let val = (table !! x) !! y -- Indexa el board en la posición dada
    let nextVal = val + 1 -- Calcula el siguiente valor
    -- Las posiciones validas a las que se puede mover
    let validP = filter (validPos table ) $ map (sumPos (x,y)) moves
    -- Si el siguiente valor esta puesto en la tabla
    if nextVal `elem` getNumbers table
        then filter (\(x',y') -> ((table !! x') !! y') == nextVal) validP
    else filter (\(x',y') -> ((table !! x') !! y') == 0) validP

extractHidatoBoard :: Maybe HidatoBoard -> HidatoBoard
extractHidatoBoard x =
    case x of
        Nothing -> Empty
        Just val -> val
