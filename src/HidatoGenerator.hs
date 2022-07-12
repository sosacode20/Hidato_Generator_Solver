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

findHidatoSolution :: HidatoBoard -> (Int, Int) -> StdGen -> Maybe ([Cell Int], HidatoBoard, Bool)
findHidatoSolution hidatoBoard (x,y) seed
    | not (validBoard hidatoBoard) = Nothing
    | gameEnd hidatoBoard = Just ([Cell (x,y) (board hidatoBoard !! x !! y)] , hidatoBoard, False)
    | otherwise = do
        let dirs = directionFilter (board hidatoBoard) (x,y) directions
        -- timesDouble
        -- getCurrentTime
        -- let (r, newSeed) = randomR (0, (length dirs) - 1) seed
        -- let newDirs = rotationDir dirs r
        -- x <- getCurrentTime
        let nextNumber = (board hidatoBoard !! x !! y) + 1 -- 5
        if dirs == [] then Nothing
        else do
            findHidatoSolution' hidatoBoard dirs nextNumber seed

findHidatoSolution' :: HidatoBoard -> [(Int, Int)] -> Int -> StdGen -> Maybe ([Cell Int], HidatoBoard, Bool)
findHidatoSolution' _ [] _ _= Nothing
findHidatoSolution' hidatoBoard (x:xs) num  seed = do
    let hidatoBoard' = Utils.replaceAt x num $ board hidatoBoard
    let res = findHidatoSolution (Board hidatoBoard' (minVal hidatoBoard) (maxVal hidatoBoard)) x seed
    let (_, newSeed) = randomR (0, (length (x:xs)) - 1) seed
    if isJust res then do
        let (cells, solution, moreThanOne) = fromJust res
        let newCells = cells <> [Cell x num]
        if moreThanOne then return (newCells, solution, moreThanOne)
        else do
            let otherSolution = findHidatoSolution' hidatoBoard xs num newSeed
            if isNothing otherSolution then res
            else return (newCells, solution, True)
    else findHidatoSolution' hidatoBoard xs num newSeed

ran :: Random a => (a, a) -> (a, StdGen)
ran (x,y) = randomR (x,y) (mkStdGen 643)

generateUniqueHidato :: HidatoBoard -> StdGen -> Maybe HidatoBoard
generateUniqueHidato hidatoBoard seed = do
    let initialPos = findElement (board hidatoBoard) (minVal hidatoBoard)
    let sol = findHidatoSolution hidatoBoard initialPos seed
    if isNothing sol then Nothing
    else do
        let (cells, _, moreThanOne) = fromJust sol
        if moreThanOne then do
            -- let (r, _) = randomR (0, (length cells) - 1) (mkStdGen 0) :: (Int, StdGen)
            -- let (r,seed) = fun (0, (length cells) - 1)
            let (r, newSeed) = randomR (0, (length cells) - 1) seed
            let cellTaken = cells !! r
            let newHidatoBoard = addCellToHidatoBoard hidatoBoard cellTaken
            generateUniqueHidato newHidatoBoard newSeed
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

