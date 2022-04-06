module Generator where

import System.IO
import Control.Monad

import Util

generateSudoku :: Int -> IO (Maybe Sudoku, Maybe Sudoku)
generateSudoku randomSeed = do
    let sudoku   = fromString " 1 4  3  6  23 81 58   9 27  6    74734  6 8    97 6 1 62    4547 651  39 1 42 6 "
    let solution = fromString "219487356647235819583169427196823574734516982825974631362798145478651293951342768"
    pure (sudoku, solution)