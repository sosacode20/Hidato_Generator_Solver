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

-- Este main se usa el random para encontrar un Hidato valido
-- con solución única y por ende hay veces que pone muchos ceros
-- y otras veces pone muy pocos
main :: IO ()
main = do
    putStrLn "\nWelcome to the generator!\n"
    -- let hidato = squareBoard 10 10
    let hidatoBoard = convertToHidatoBoard table
    let initialPos = findElement (board hidatoBoard) (minVal hidatoBoard)

    currTime <- getCurrentTime
    let timed = floor $ utctDayTime currTime :: Int
    let seed = mkStdGen (timed)

    let uniqueHidato = generateUniqueHidato hidatoBoard seed
    if isNothing uniqueHidato then putStrLn "No se puede generar un tablero único... El tablero inicial esta invalido\n\n"
    else do
      putStrLn "Hidato con solución única:\n\n"
      putStrLn $ showBoard $ fromJust uniqueHidato
      putStrLn "\n\n--------------------------------\n\n"
    
    let uniqueSolutions = findHidatoSolution (fromJust uniqueHidato) initialPos seed
    if isNothing uniqueSolutions then putStrLn "El Hidato no tiene solución"
    else do
      let (_, solution, moreThanOne) = fromJust uniqueSolutions
      putStrLn "Una solución es:\n\n"
      putStrLn $ showBoard solution
      if moreThanOne then putStrLn "Hay mas de una solución\n"
      else do
        putStrLn "\n\n----------------------------------------"
