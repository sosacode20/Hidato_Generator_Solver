module MainSolver where

import Utils
import HidatoCommon
import HidatoSolver

table :: [[Int]]
table = [
    [0, 33, 35, 0, 0, -1, -1, -1],
    [0,0,24,22,0, -1,-1,-1],
    [0,0,0,21,0,0,-1,-1],
    [0,26,0,13,40,11,-1,-1],
    [27,0,0,0,9,0,1,-1],
    --[27,0,0,13,9,0,1,-1],
    [-1,-1,0,0,18,0,0,-1],
    [-1,-1,-1,-1,0,7,0,0],
    [-1,-1,-1,-1,-1,-1,5,0]
    ]


main :: IO()
main = do
    -- let dim = dimension table
    let hidatoBoard = convertToHidatoBoard table
    let validB = validInitialBoard hidatoBoard
    print(validB)
    putStrLn (showBoard hidatoBoard)
    if not validB
        then putStrLn "Hidato incorrecto"
    else do
        let initialPos = findElement (board hidatoBoard) (minVal hidatoBoard)
        print initialPos
        -- print(board)
        putStrLn "\n----------------------------------------------------\n"
        let solution = solveHidato hidatoBoard initialPos
        let solutionHidato = extractHidatoBoard solution
        if solutionHidato == Empty
            then putStrLn "Algo malo paso"
        else do
            putStrLn "Solution:\n"
            putStrLn (showBoard solutionHidato)
    -- print(dim)
