-- {-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module HidatoGenerator where

-- {-# LANGUAGE BlockArguments #-}
import HidatoCommon
-- import qualified Utils
import Data.Maybe (isJust, fromJust, isNothing)
import Utils
import System.Random
-- import GHC.Base (divInt)

squareBoard :: Int -> Int -> HidatoBoard
squareBoard n m =  
    Board {board = [[0 | _ <- [1..n]] | _ <- [1..m]], minVal = 1, maxVal = n*m}

findHidatoSolution :: HidatoBoard -> (Int, Int) -> StdGen -> Maybe ([Cell Int], HidatoBoard, Bool)
findHidatoSolution hidatoBoard (x,y) seed
    | not (validBoard hidatoBoard) = Nothing -- Si no es valido el tablero en algún punto entonces Nothing
    | gameEnd hidatoBoard = Just ([Cell (x,y) (board hidatoBoard !! x !! y)] , hidatoBoard, False) -- Si el juego termino entonces devuelve la solución encontrada
    | otherwise = do
        let dirs = directionFilter (board hidatoBoard) (x,y) directions -- Busca las posiciones a las que se puede mover en el siguiente paso
        
        let nextNumber = (board hidatoBoard !! x !! y) + 1 -- Calcula el siguiente numero
        if dirs == [] then Nothing -- Si no hay mas lugar a donde moverse entonces se devuelve Nothing
        else do
            findHidatoSolution' hidatoBoard dirs nextNumber seed -- Aquí se busca soluciones por algunas de las siguientes posiciones

findHidatoSolution' :: HidatoBoard -> [(Int, Int)] -> Int -> StdGen -> Maybe ([Cell Int], HidatoBoard, Bool)
findHidatoSolution' _ [] _ _= Nothing
findHidatoSolution' hidatoBoard (x:xs) num  seed = do
    let hidatoBoard' = Utils.replaceAt x num $ board hidatoBoard -- agrega la posición a la tabla
    let res = findHidatoSolution (Board hidatoBoard' (minVal hidatoBoard) (maxVal hidatoBoard)) x seed -- Encuentra si tiene solución o no
    let (_, newSeed) = randomR (0, (length (x:xs)) - 1) seed -- Genera una nueva semilla
    if isJust res then do -- Si tiene solución
        let (cells, solution, moreThanOne) = fromJust res -- Extrae el valor de la solución
        let newCells = cells <> [Cell x num] -- Concatena las celdas que agregaste en ese momento con la celda actual
        if moreThanOne then return (newCells, solution, moreThanOne) -- Si hay mas de una solución retorno la primera que se encontró
        else do -- Si no hay mas soluciones entonces por la rama de poner la posición x solo se llega a una solución y hay que ver si por xs hay otras soluciones
            let otherSolution = findHidatoSolution' hidatoBoard xs num newSeed -- Mira a ver si por xs hay mas soluciones
            if isNothing otherSolution then res -- Si por xs no se encontró alguna solución entonces se devuelve la 1ra
            else return (newCells, solution, True) -- Si se encontró una solución por xs entonces devuelvo la 1ra solución por la posición 'x' y entonces doy True para que se sepa que hay mas soluciones
    else findHidatoSolution' hidatoBoard xs num newSeed -- Si no tiene solución por x entonces busca por xs

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
            let (_, newSeed) = randomR (0, (length cells) - 1) seed
            -- let cellTaken = cells !! r
            -- let newHidatoBoard = addCellToHidatoBoard hidatoBoard cellTaken
            let shuffledCells = shuffle cells seed
            let cellsTaken = take (1) shuffledCells
            let newHidatoBoard = addCellsToHidatoBoard hidatoBoard cellsTaken
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

