module Main where

import Data.GI.Base
import qualified GI.Gtk as G

import UserInterface
import Util

main :: IO ()
main = do
    G.init Nothing

    -- Создание интерфейса
    ui <- buildSudokuUI
    -- Связывание ячеек и всплывающие окно
    cellsBind (cells ui) (popover ui)
    -- Связывание кнопок в всплывающем окне с всплывающем окном
    numbersBind (numberButtons ui) (popover ui)
    -- Назначение кнопки очистки в всплывающем окне
    on (inputClear ui) #clicked $ writePopoverRelativeCell (popover ui) blankval
    -- Назначение кнопки решения
    on (solveButton ui) #clicked $ solveAllCells (cells ui)
    -- Назначение кнопки проверки
    on (checkButton ui) #clicked $ checkAllCells (cells ui)
    -- Назначение кнопки новой игры
    on (newGameButton ui) #clicked $ newGame (cells ui) (popover ui)

    #showAll (window ui)
    G.main
