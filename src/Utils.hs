-- Quitar la linea inmediata de abajo
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Utils where

{-# LANGUAGE BlockArguments #-}
import Data.Maybe (fromJust)
-- import Data.List (elemIndex)
import Data.List
import System.Random (StdGen, Random (randomR))
-- import Data.Unique

rotationDir:: [(Int,Int)]->Int-> [(Int,Int)]
rotationDir [] _ = []
rotationDir (x:xs) n = do
    let large = length (x:xs)
    let condition = n < 0 || n >= large
    if condition then do
        let newN = n `mod`  large
        rotationDir (x:xs) newN
    else do
        let (y,ys) = splitAt n (x:xs)
        ys<>y

--------------------------- Finds the position of an element  ---------------------------
findElement :: [[Int]] -> Int -> (Int, Int)
findElement cells el = (x, y)
    where
        x = findLine cells el 0
        y = fromJust $ elemIndex el (cells!!x)
        
------------------------- Returns the row of an element in the list -------------------------
findLine :: [[Int]] -> Int -> Int -> Int 
findLine cells el pos
    | pos == length cells = -1
    -- Aquí se usa la función `elem` en forma infija para que se vea mas bonito
    | el `elem` (cells !! pos) = pos
    | otherwise = findLine cells el (pos + 1)

-------------------------------------------------------------------------------------------

getNumbers :: [[Int]] -> [Int]
getNumbers table = concatMap (filter (> 0)) table

getZerosCount :: [[Int]] -> Int
getZerosCount board = length $ filter (== 0) $ concat board

---------------------------------------------------------------------------

isUnique :: [Int] -> Bool
isUnique [] = True
isUnique [_] = True
isUnique (x:xs) = x `notElem` xs && isUnique xs

----------------

-- Dice si dos posiciones son vecinas
neighborPositions :: (Int, Int) -> (Int, Int) -> Bool
neighborPositions (x,y) (x', y') = do
    let dx = abs $ x - x'
    let dy = abs $ y - y'
    dx <= 1 && dy <= 1

---------------------------------------------------------------------------

-- Obtiene la dimension de una matriz
dimension :: [[Int]] -> (Int, Int)
dimension xs = (length xs, length (head xs))

-- Dice si la posición dada es valida en la matriz dada
validPos :: [[Int]] -> (Int, Int) -> Bool
validPos t (x,y) = x >= 0 && x < (fst (dimension t)) && y >= 0 && y < (snd (dimension t))

-- Nueva matriz resultante de reemplazar
replaceAt :: Eq a => (Int, Int) -> a -> [[a]] -> [[a]]
replaceAt (x, y) num table = a1 ++ (newLine:b1)
    where
        -- Linea dividiendo por filas
        (a1, line:b1) = splitAt x table
        -- posición dividiendo la linea por columnas
        (a2, _:b2) = splitAt y line
        newLine = a2 ++ (num:b2)
        -- TODO: Hacer el pattern Matching exhaustivo

-----------------------------------------------------------------------------

sumPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

---------------------------------------------------------------------------

shuffle :: Eq a => [a] -> StdGen -> [a]
shuffle [] _ = []
shuffle (x:xs) seed = do
    let (randomIndex, newSeed) = randomR (0, (length (x:xs)) - 1) seed
    if randomIndex == 0 then (x:(shuffle xs newSeed))
    else do
        let (_:firstPartTail, lastPartHead:rest) = splitAt randomIndex (x:xs)
        let newTail = firstPartTail <> (x:rest)
        (lastPartHead:(shuffle newTail newSeed))

----------------------------------------------------------------------------

