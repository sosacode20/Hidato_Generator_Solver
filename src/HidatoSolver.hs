module HidatoSolver where

import HidatoCommon
import qualified Utils

solveHidato :: HidatoBoard -> (Int, Int) -> Maybe HidatoBoard
solveHidato hidatoBoard (x,y)
    | not (validBoard hidatoBoard) = Nothing
    | (maxVal hidatoBoard) - (minVal hidatoBoard) - length (Utils.getNumbers (board hidatoBoard)) + 1 == 0 = Just hidatoBoard
    | otherwise = do
        let dirs = directionFilter (board hidatoBoard) (x,y) directions -- (6,7) --> (7,6)
        let nextNumber = (board hidatoBoard !! x !! y) + 1 -- 5
        if dirs == [] then Nothing
        else do
            solveHidato' hidatoBoard dirs nextNumber

solveHidato' :: HidatoBoard -> [(Int, Int)] -> Int -> Maybe HidatoBoard
-- TODO: Resolver esto
-- solveHidato' hidatoBoard [] num = Just hidatoBoard
solveHidato' _ [] _ = Nothing
solveHidato' hidatoBoard (x:xs) num = do
    let hidatoBoard' = Utils.replaceAt x num $ board hidatoBoard
    let res = solveHidato (Board hidatoBoard' (minVal hidatoBoard) (maxVal hidatoBoard)) x
    let resHidato = extractHidatoBoard res
    if resHidato /= Empty
        then res
    else solveHidato' hidatoBoard xs num
