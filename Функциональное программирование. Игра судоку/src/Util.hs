module Util where

newtype Sudoku = Sudoku String

-- Вывод судоку
instance Show Sudoku where
    show (Sudoku s) = "\n" ++ concatMap showThird grouped ++ line
        where grouped         = groupBy boxsize . map (groupBy boxsize) . groupBy boardsize $ s
              showThird t     = line ++ concatMap showRow t
              showRow r       = concatMap showRowThird r ++ "|\n"
              showRowThird rt = "|" ++ concatMap showCell rt
              showCell c      = " " ++ (c : " ")
              line            = concat (replicate boardsize "---") ++ concat (replicate boxsize "-") ++ "-\n"

-- Разбивает список на группу списков, заданной длины
groupBy :: Int -> [a] -> [[a]]
groupBy n [] = []
groupBy n xs = take n xs : groupBy n (drop n xs)

boardsize :: Int
boardsize = 9

boxsize :: Int
boxsize = 3

cellvals :: String
cellvals = "123456789"

blankval :: Char
blankval = ' '

-- Проверка строки судоку на правильность
valid :: String -> Bool
valid s = (length s == (boardsize * boardsize)) &&
          all (`elem` blankval:cellvals) s

-- Создание судоку из строки
fromString :: String -> Maybe Sudoku
fromString s | valid s   = Just $ Sudoku s
             | otherwise = Nothing

-- Приведение судоку к строке
toString :: Sudoku -> String
toString (Sudoku s) = s
