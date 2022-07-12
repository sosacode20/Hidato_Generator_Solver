module MainGenerator where

import HidatoGenerator
import HidatoCommon
import Text.Read
-- import HidatoGenerator
import Utils
import Data.Maybe (fromJust, isNothing)
import System.Random (mkStdGen)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDayTime))
-- import System.Random

table :: [[Int]]
-- table = [
--   [0,0,0,0,17],
--   [0,0,0,0,-1],
--   [-1,-1,0,0,0],
--   [1,0,3,0,0]
--   ]
-- table = [
--   [0,0,0,0,17],
--   [0,0,0,0,-1],
--   [-1,-1,1,-1,-1],
--   [0,0,0,0,0]
--   ]
-- table = [
--     [0, 33, 35, 0, 0, -1, -1, -1],
--     [0,0,24,22,0, -1,-1,-1],
--     [0,0,0,21,0,0,-1,-1],
--     [0,26,0,13,40,11,-1,-1],
--     [27,0,0,0,9,0,1,-1],
--     --[27,0,0,13,9,0,1,-1],
--     [-1,-1,0,0,18,0,0,-1],
--     [-1,-1,-1,-1,0,7,0,0],
--     [-1,-1,-1,-1,-1,-1,5,0]
--     ]
table = [
    [0, 0, 0, 0, 0, -1, -1, -1],
    [0,0,0,0,0, -1,-1,-1],
    [0,0,0,0,0,0,-1,-1],
    [0,0,0,0,40,0,-1,-1],
    [0,0,0,0,0,0,1,-1],
    --[27,0,0,13,9,0,1,-1],
    [-1,-1,0,0,0,0,0,-1],
    [-1,-1,-1,-1,0,0,0,0],
    [-1,-1,-1,-1,-1,-1,0,0]
    ]


printNumber :: String -> String
printNumber s = do
  let p = readMaybe s :: Maybe Int
  case p of
    Just n -> show n
    Nothing -> "Invalid Integer"

main :: IO ()
main = do
    putStrLn "\nWelcome to the generator!\n"
    -- let hidato = squareBoard 10 10
    let hidatoBoard = convertToHidatoBoard table
    let initialPos = findElement (board hidatoBoard) (minVal hidatoBoard)
    -- let uniqueSolutions = findSolution hidatoBoard initialPos
    currTime <- getCurrentTime
    let timed = floor $ utctDayTime currTime :: Int
    let seed = mkStdGen (timed)
    -- let uniqueHidato = generateUniqueHidato hidatoBoard ran
    let uniqueHidato = generateUniqueHidato hidatoBoard seed
    if isNothing uniqueHidato then putStrLn "No hay un tablero único??... Mhmm, Extraño\n\n"
    else do
      putStrLn "Hidato con solucion unica:\n\n"
      putStrLn $ showBoard $ fromJust uniqueHidato
      putStrLn "\n\n--------------------------------\n\n"
      -- let allSolutions = findHidatosSolutions uniqueHidato initialPos
    
    let uniqueSolutions = findHidatoSolution hidatoBoard initialPos seed
    if isNothing uniqueSolutions then putStrLn "El Hidato no tiene solución"
    else do
      let (cells, solution, moreThanOne) = fromJust uniqueSolutions
      putStrLn "Una solución es:\n\n"
      putStrLn $ showBoard solution
      putStrLn "\n\nLas posiciones agregadas fueron:\n\n"
      print cells
      if moreThanOne then putStrLn "Hay mas de una solución\n"
      else do
        putStrLn "\n\n----------------------------------------"

        -- let newHidato = addCellsToHidatoBoard hidatoBoard cells
        -- putStrLn $ "El hidato con solución única es: \n\n" <> (showBoard newHidato)
        -- putStrLn $ "La solución única de este Hidato es:\n\n" <> (showBoard solution)

    -- let solutions = findHidatosSolutions hidatoBoard initialPos
    -- let sol = filter isJust solutions
    -- printHidatosSolutions sol
    -- -- let possibleNumber = getLine
    -- -- putStrLn $ possibleNumber >>= printNumber
    -- -- getLine >>= (\line -> putStrLn $ printNumber line)
    -- -- fromMaybe 
    -- -- let parsedNumber = readMaybe possibleNumber :: Maybe Int
    -- putStrLn (showBoard hidato)