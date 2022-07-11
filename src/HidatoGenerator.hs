-- {-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module HidatoGenerator where

-- {-# LANGUAGE BlockArguments #-}
import HidatoCommon

squareBoard :: Int -> Int -> HidatoBoard
squareBoard n m =  
    Board {board = [[0 | _ <- [1..n]] | _ <- [1..m]], minVal = 1, maxVal = n*m}
