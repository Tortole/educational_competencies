module Sudoku where

import           Control.Monad
import           Data.List
import           Debug.Trace
import           System.Random
import           Text.Printf

type Board = [[Int]]
type Pos = (Int, Int)

sudokuGenerationStartingCells = 10
maxReductionRetry = 5
targetRemainingCells = 30
targetCellsRemoval = 9 * 9 - targetRemainingCells

randomSudokuExercise :: IO Board
-- randomSudokuExercise = do
--   completedSudoku <- randomCompletedSudoku
--   retrySudokuReduction completedSudoku

randomSudokuExercise = do
  completedSudoku <- randomCompletedSudoku
  sudoku <- reduceSudoku targetCellsRemoval maxReductionRetry completedSudoku
  if countCells sudoku /= 81 - targetCellsRemoval
    then randomSudokuExercise
    else return sudoku

retrySudokuReduction :: Board -> IO Board
retrySudokuReduction completedSudoku = do
  sudoku <- reduceSudoku targetCellsRemoval maxReductionRetry completedSudoku
  if countCells sudoku /= 81 - targetCellsRemoval
    then retrySudokuReduction completedSudoku
    else return sudoku


reduceSudoku :: Int -> Int -> Board -> IO Board
reduceSudoku 0 _      board = return board
reduceSudoku _ 0      board = return board
reduceSudoku k retryN board = do
  let targetRemainingCells = countCells board - k
  board' <- tryRemoveOneCellAndKeepUniqueSolution board
  printf "--- reduced sudoku (%d -> %d) (remaining retry %d) ---"
         (countCells board)
         targetRemainingCells
         retryN
  putStrLn ""
  drawBoard board'

  if board == board'
    then reduceSudoku k (retryN - 1) board'
    else reduceSudoku (k - 1) maxReductionRetry board'


tryRemoveOneCellAndKeepUniqueSolution :: Board -> IO Board
tryRemoveOneCellAndKeepUniqueSolution board = do
  board' <- removeRandomCell board
  return $ if hasMoreThanOneSolution board then board else board'

hasMoreThanOneSolution :: Board -> Bool
hasMoreThanOneSolution = (> 1) . length . take 2 . findSudokuSolutions

randomCompletedSudoku :: IO Board
randomCompletedSudoku = do
  -- fill random n cells
  board <- fillRandomCells sudokuGenerationStartingCells emptyBoard
  print board

  -- then try to find one solution
  let solutions   = take 1 $ findSudokuSolutions board
  let hasSolution = length solutions > 0
  if hasSolution
    then do
      putStrLn $ "--- generated sudoku ---"
      drawBoard $ head solutions
      return $ head solutions
    else do
      putStrLn $ "Retrying a new random sudoku..."
      randomCompletedSudoku


fillRandomCells :: Int -> Board -> IO Board
fillRandomCells 0 board = return board
fillRandomCells n board = do
  num    <- randomCellNum
  board' <- fillRandomCell num board
  fillRandomCells (n - 1) board'

emptyBoard :: Board
emptyBoard = replicate 9 $ replicate 9 0

removeRandomCell :: Board -> IO Board
removeRandomCell board = do
  pos <- randomPos
  if isCellEmpty pos board
    then removeRandomCell board
    else return $ fillBoardNum pos 0 board

fillRandomCell :: Int -> Board -> IO Board
fillRandomCell num board = do
  pos <- randomPos
  if isCellEmpty pos board
    then return $ fillBoardNum pos num board
    else fillRandomCell num board

randomCellNum :: IO Int
randomCellNum = randomRIO (1, 9)

randomPos :: IO Pos
randomPos = do
  i <- randomRIO (0, 9 * 9 - 1)
  return $ divMod i 9

findSudokuSolutions :: Board -> [Board]
findSudokuSolutions board
  | not $ validateBoard board
  = []
  | isBoardFilled board
  = [board]
  | otherwise
  = let (i, j) = head $ getSortedEmptyCells board
    -- trace (getBoardView board) $ return ()
    in
      concat
        [ findSudokuSolutions $ fillBoardNum (i, j) num board
        | num <- [1 .. 9]
        ]

-- Try all numbers on an empty cell. 
-- If no luck, fail it.
solveSudoku :: Board -> Maybe Board
solveSudoku board
  | not $ validateBoard board = Nothing
  | isBoardFilled board = Just board
  | otherwise = do
    -- pick one empty cell
    let (i, j) = head $ getSortedEmptyCells board
    -- trace (getBoardView board) $ return ()

    -- try all numbers
    -- lazily get the first valid solution with msum
    msum $ [ solveSudoku $ fillBoardNum (i, j) num board | num <- [1 .. 9] ]

fillBoardNum :: (Int, Int) -> Int -> Board -> Board
fillBoardNum (i, j) num board = getBoardFromList $ xs1 ++ [num] ++ xs2
 where
  i'             = i * 9 + j
  (xs1, _ : xs2) = splitAt i' $ getBoardNums board

isBoardFilled :: Board -> Bool
isBoardFilled board = notElem 0 $ getBoardNums board

-- 1. no duplicted number on each row & column
-- 2. no duplicated number in each 3x3 block
validateBoard :: Board -> Bool
validateBoard board =
  noDuplicatedNumInRows && noDuplicatedNumInCols && noDuplicatedNumInBlocks
 where
  noDuplicatedNumInRows   = all isUniqueList $ getBoardRows board
  noDuplicatedNumInCols   = all isUniqueList $ getBoardCols board
  noDuplicatedNumInBlocks = all isUniqueList $ getBoardBlocks board

isUniqueList :: [Int] -> Bool
isUniqueList xs = let xs' = filter (/= 0) xs in xs' == nub xs'

-- put empty cells with less choices first
getSortedEmptyCells :: Board -> [Pos]
getSortedEmptyCells board = sortOn f . getEmptyCells $ board
 where
  f (i, j) =
    sum
      $ map fromEnum
      $ map validateBoard
      $ [ fillBoardNum (i, j) num board | num <- [1 .. 9] ]

countCells :: Board -> Int
countCells = length . filter (/= 0) . getBoardNums

isCellEmpty :: Pos -> Board -> Bool
isCellEmpty pos board = getBoardNum pos board == 0

getEmptyCells :: Board -> [Pos]
getEmptyCells board = map (\i -> divMod i 9) idxs
  where idxs = elemIndices 0 $ getBoardNums board

getBoardNum :: Pos -> Board -> Int
getBoardNum (i, j) board = board !! i !! j

getBoardNums :: Board -> [Int]
getBoardNums = concat . getBoardRows

getBoardRows :: Board -> [[Int]]
getBoardRows = id

getBoardBlocks :: Board -> [[Int]]
getBoardBlocks board =
  [ [ getBoardNum (ii * 3 + 0, jj * 3 + 0) board
    , getBoardNum (ii * 3 + 0, jj * 3 + 1) board
    , getBoardNum (ii * 3 + 0, jj * 3 + 2) board
    , getBoardNum (ii * 3 + 1, jj * 3 + 0) board
    , getBoardNum (ii * 3 + 1, jj * 3 + 1) board
    , getBoardNum (ii * 3 + 1, jj * 3 + 2) board
    , getBoardNum (ii * 3 + 2, jj * 3 + 0) board
    , getBoardNum (ii * 3 + 2, jj * 3 + 1) board
    , getBoardNum (ii * 3 + 2, jj * 3 + 2) board
    ]
  | ii <- [0 .. 2]
  , jj <- [0 .. 2]
  ]

getBoardCols :: Board -> [[Int]]
getBoardCols board =
  map (\j -> [ getBoardNum (i, j) board | i <- [0 .. 8] ]) [0 .. 8]

getBoardView :: Board -> String
getBoardView board = concat
  [ (++ if (i == 2 || i == 5) then ("\n" ++ lineBreak ++ "\n") else "\n")
      $ concat
          [ let num       = getBoardNum (i, j) board
                numStr    = if num == 0 then " " else show num
                separator = if (j == 2 || j == 5) then "|" else ""
            in  numStr ++ separator
          | j <- [0 .. 8]
          ]
  | i <- [0 .. 8]
  ]
  where lineBreak = replicate 11 '-'

drawBoard :: Board -> IO ()
drawBoard = putStrLn . getBoardView

getBoardFromList :: [a] -> [[a]]
getBoardFromList = chunks 9

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n xs = (take n xs) : (chunks n (drop n xs))