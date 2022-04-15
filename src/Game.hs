module Game
    (
        pipeline,
        check
    )
where

import Utils
import SudokuTypes
import Data.List ((\\), sort, concat, and)

affinity :: Cell -> Cell -> Bool
affinity (Cell a b c) (Cell d e f)
    | a == d      = True
    | b == e      = True 
    | byr && byc  = True
    | otherwise   = False
    where
        byr = ((a // 3) == (d // 3))
        byc = ((b // 3) == (e // 3))

related :: Table -> Cell -> [Int]
related table target
    | length cells > 0 = [1..9] \\ (map value cells)
    | otherwise        = [1..9]
    where
        cells = filter (affinity target) table

pipeline :: Sudoku -> Sudoku
pipeline sudoku 
    | done sudoku   = sudoku
    | otherwise     = backtrack sudoku op cell
    where 
        Sudoku ceros nums = sudoku
        foo = map (\(a, b) -> (length a, a, b))
        relations = [(related nums cell, cell) | cell <- ceros]
        (cant, op, cell) = head $ sort $ foo relations

backtrack :: Sudoku -> [Int] -> Cell -> Sudoku
backtrack original l cell
    | l == []       = original 
    | done sudoku   = sudoku
    | otherwise     = backtrack original (tail l) cell
    where
        sudoku = pipeline $ moveCell original cell (head l)

moveCell :: Sudoku -> Cell -> Int -> Sudoku
moveCell s cell v = 
    let Sudoku a b = s 
    in  Sudoku (a \\ [cell]) (changeValue cell v : b)

check :: Table -> Bool
check table = and [elem (value cell) (related (table \\ [cell]) cell) | cell <- table]
