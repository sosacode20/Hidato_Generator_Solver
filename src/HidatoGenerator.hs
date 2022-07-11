-- {-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module HidatoGenerator where

-- {-# LANGUAGE BlockArguments #-}
import HidatoCommon
import qualified Utils

squareBoard :: Int -> Int -> HidatoBoard
squareBoard n m =  
    Board {board = [[0 | _ <- [1..n]] | _ <- [1..m]], minVal = 1, maxVal = n*m}


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
