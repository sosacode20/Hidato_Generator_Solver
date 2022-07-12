module Main where

import System.Random
import Data.Time (getCurrentTime, UTCTime (utctDayTime))
-- import System.Random.Stateful (globalStdGen)

table :: [[Int]]
table = [
  [0,0,0,0,17],
  [0,0,0,0,-1],
  [-1,-1,0,0,0],
  [1,0,0,0,0]
  ]
-- table = [
--   [0,11,0,0,17],
--   [13,0,15,0,-1],
--   [-1,-1,8,0,0],
--   [1,0,0,0,6]
--   ]
-- table = [
--     [0, 0, 0, 0, 0, -1, -1, -1],
--     [0,0,0,0,0, -1,-1,-1],
--     [0,0,0,0,0,0,-1,-1],
--     [0,0,0,0,40,39,-1,-1],
--     [0,0,0,0,0,0,1,-1],
--     --[27,0,0,13,9,0,1,-1],
--     [-1,-1,0,0,0,0,0,-1],
--     [-1,-1,-1,-1,0,0,0,0],
--     [-1,-1,-1,-1,-1,-1,0,0]
--     ]
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
-- import Random

-- randomNumber min max = rand

printNRandom :: Int -> StdGen -> IO ()
printNRandom 0 _ = putStrLn ""
printNRandom n seed = do
    let (r, newSeed) = randomR (0,13) seed :: (Int,StdGen)
    print r
    printNRandom (n-1) newSeed


main :: IO ()
main = do
    let n = 20
    currTime <- getCurrentTime
    let timed = floor $ utctDayTime currTime :: Int
    let seed = mkStdGen (timed)
    -- let lis = [2..26] :: [Int]s
    
    printNRandom n seed

