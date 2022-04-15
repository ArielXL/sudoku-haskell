module SudokuTypes
    (
        Cell(..),
        Table(..),
        Sudoku(..),
        done,
        value,
        changeValue
    )
where

import Data.List (intercalate)

data Cell = Cell Int Int Int deriving(Eq, Ord)
type Table = [Cell]
data Sudoku = Sudoku Table Table

instance Show Cell where  
    show (Cell a b c) = "(" ++ (intercalate ", " (map (show) [a, b, c])) ++ ")"

done :: Sudoku -> Bool
done (Sudoku a _) = length a == 0

changeValue :: Cell -> Int -> Cell
changeValue (Cell a b _) v = (Cell a b v)

value :: Cell -> Int
value (Cell _ _ v) = v
