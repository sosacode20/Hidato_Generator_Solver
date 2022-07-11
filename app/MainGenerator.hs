module MainGenerator where

import HidatoGenerator
import HidatoCommon


main :: IO ()
main = do
    putStrLn "\nWelcome to the generator!\n"
    let hidato = squareBoard 10 10
    putStrLn (showBoard hidato)