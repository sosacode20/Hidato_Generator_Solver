-- {-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module HidatoGenerator where

-- {-# LANGUAGE BlockArguments #-}
import HidatoCommon
-- import qualified Utils
import Data.Maybe (isJust, fromJust, isNothing)
import Utils
import System.Random
-- import Data.Time.Clock (getCurrentTime)
-- import System.CPUTime (getCPUTime)
-- import
-- import GHC.Float (timesDouble)

squareBoard :: Int -> Int -> HidatoBoard
squareBoard n m =  
    Board {board = [[0 | _ <- [1..n]] | _ <- [1..m]], minVal = 1, maxVal = n*m}

findHidatoSolution :: HidatoBoard -> (Int, Int) -> Maybe ([Cell Int], HidatoBoard, Bool)
findHidatoSolution hidatoBoard (x,y)
    | not (validBoard hidatoBoard) = Nothing
    | gameEnd hidatoBoard = Just ([Cell (x,y) (board hidatoBoard !! x !! y)] , hidatoBoard, False)
    | otherwise = do
        let dirs = directionFilter (board hidatoBoard) (x,y) directions
        -- timesDouble
        -- getCurrentTime
        -- x <- getCurrentTime
        let nextNumber = (board hidatoBoard !! x !! y) + 1 -- 5
        if dirs == [] then Nothing
        else do
            findHidatoSolution' hidatoBoard dirs nextNumber

findHidatoSolution' :: HidatoBoard -> [(Int, Int)] -> Int -> Maybe ([Cell Int], HidatoBoard, Bool)
findHidatoSolution' _ [] _ = Nothing
findHidatoSolution' hidatoBoard (x:xs) num = do
    let hidatoBoard' = Utils.replaceAt x num $ board hidatoBoard
    let res = findHidatoSolution (Board hidatoBoard' (minVal hidatoBoard) (maxVal hidatoBoard)) x
    if isJust res then do
        let (cells, solution, moreThanOne) = fromJust res
        let newCells = cells <> [Cell x num]
        if moreThanOne then return (newCells, solution, moreThanOne)
        else do
            let otherSolution = findHidatoSolution' hidatoBoard xs num
            if isNothing otherSolution then res
            else return (newCells, solution, True)
    else findHidatoSolution' hidatoBoard xs num

ran :: Random a => (a, a) -> (a, StdGen)
ran (x,y) = randomR (x,y) (mkStdGen 643)

generateUniqueHidato :: HidatoBoard -> ((Int,Int) -> (Int, StdGen)) -> Maybe HidatoBoard
generateUniqueHidato hidatoBoard fun = do
    let initialPos = findElement (board hidatoBoard) (minVal hidatoBoard)
    let sol = findHidatoSolution hidatoBoard initialPos
    if isNothing sol then Nothing
    else do
        let (cells, solution, moreThanOne) = fromJust sol
        if moreThanOne then do
            -- let (r, _) = randomR (0, (length cells) - 1) (mkStdGen 0) :: (Int, StdGen)
            let (r,seed) = fun (0, (length cells) - 1)
            let cellTaken = cells !! r
            let newHidatoBoard = addCellToHidatoBoard hidatoBoard cellTaken
            generateUniqueHidato newHidatoBoard (\(x,y) -> randomR (x,y) (seed))
            -- Continuar la lógica
        else return hidatoBoard

---------------------------------------------------------------------------


findHidatosSolutions :: HidatoBoard -> (Int, Int) -> [Maybe HidatoBoard]
findHidatosSolutions hidatoBoard (x,y)
    | not (validBoard hidatoBoard) = [Nothing]
    | gameEnd hidatoBoard = [Just hidatoBoard]
    | otherwise = do
        let dirs = directionFilter (board hidatoBoard) (x,y) directions -- (6,7) --> (7,6)
        let nextNumber = (board hidatoBoard !! x !! y) + 1 -- 5
        if dirs == [] then [Nothing]
        else do
            solveHidatos' hidatoBoard dirs nextNumber

solveHidatos' :: HidatoBoard -> [(Int, Int)] -> Int -> [Maybe HidatoBoard]
-- TODO: Resolver esto
-- solveHidato' hidatoBoard [] num = Just hidatoBoard
solveHidatos' _ [] _ = [Nothing]
solveHidatos' hidatoBoard (x:xs) num = do
    let hidatoBoard' = Utils.replaceAt x num $ board hidatoBoard
    let res = findHidatosSolutions (Board hidatoBoard' (minVal hidatoBoard) (maxVal hidatoBoard)) x
    -- let resHidato = extractHidatoBoard res
    let otherSolutions = solveHidatos' hidatoBoard xs num
    res <> otherSolutions

-- findSolutionsAndAddCells

--------------------------------------------------------------
--------------------------------------------------------------
rotationDir:: [(Int,Int)]->Int-> [(Int,Int)]
rotationDir (x:xs) n = do
    let large = length (x:xs)
    let condition = n < 0 || n >= large
    if condition then do
        let newN = n `mod`  large
        rotationDir (x:xs) newN
    else do
        let (y,ys) = splitAt n (x:xs)
        ys<>y

