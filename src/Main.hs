import Game
import SudokuIO
import System.Environment

main = do
    args <- getArgs
    sudoku <- fmap loadSudoku $ readFile (args !! 0)
    putStrLn "Your initial sudoku is:"
    putStrLn $ formatSudoku sudoku
    putStrLn "One solution is:"
    putStr $ formatSudoku $ pipeline sudoku
