module UserInterface where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.GI.Base
import qualified Data.Text as T
import Data.Typeable
import Data.Maybe
import qualified GI.Gtk as G

import Paths_sudoku
import Generator
import Util

newtype BuilderCastException = UnknownIdException String 
    deriving (Show, Typeable)

instance Exception BuilderCastException

type Cell = G.Button
type Cells = [Cell]

data SudokuUI = SudokuUI { window        :: G.Window
                         , cells         :: Cells
                         , popover       :: G.Popover
                         , numberButtons :: [G.Button]
                         , inputClear    :: G.Button
                         , solveButton   :: G.Button
                         , checkButton   :: G.Button
                         , newGameButton :: G.Button
                         }

buildSudokuUI :: IO SudokuUI
buildSudokuUI = do
    uiFile  <- T.pack <$> getDataFileName "gui/sudoku.ui"
    builder <- G.builderNewFromFile uiFile

    window          <- buildMainWindow  builder "mainWindow"
    cells           <- builderGetsTyped builder cellNames       G.Button
    popover         <- builderGetTyped  builder "inputPopover"  G.Popover
    numberButtons   <- builderGetsTyped builder numberNames     G.Button
    inputClear      <- builderGetTyped  builder "inputClear"    G.Button
    solveButton     <- builderGetTyped  builder "solveButton"   G.Button
    checkButton     <- builderGetTyped  builder "checkButton"   G.Button
    newGameButton   <- builderGetTyped  builder "newGameButton" G.Button

    pure $ SudokuUI window 
                    cells 
                    popover
                    numberButtons 
                    inputClear 
                    solveButton
                    checkButton 
                    newGameButton

cellNames :: [T.Text]
cellNames = map ((T.pack . (++) "cell") . show) [1..81]

numberNames :: [T.Text]
numberNames = map (T.pack . (++) "input" . show) [1..9]

-- Создаёт элементы GTK по id
builderGetTyped :: (G.IsBuilder b, GObject o, MonadIO m) => b -> T.Text -> (ManagedPtr o -> o) -> m o
builderGetTyped builder objectId gtype = 
    liftIO $ fromJust <$> G.builderGetObject builder objectId >>= G.unsafeCastTo gtype

-- Создаёт элементы GTK по списку id
builderGetsTyped :: (GObject o, G.IsBuilder b, MonadIO m) => b -> [T.Text] -> (ManagedPtr o -> o) -> m [o]
builderGetsTyped builder objectIds gtype = 
    mapM (\o -> builderGetTyped builder o gtype) objectIds

-- Создаёт основное меню и привязывает css файл
buildMainWindow :: (MonadIO m, G.IsBuilder b) => b -> T.Text -> m G.Window
buildMainWindow builder name = liftIO $ do
    window  <- builderGetTyped builder name G.Window
    on window #destroy G.mainQuit
    cssFile <- T.pack <$> getDataFileName "gui/theme.css"
    screen <- G.windowGetScreen window
    cssProvider <- G.cssProviderNew
    G.cssProviderLoadFromPath cssProvider cssFile
    G.styleContextAddProviderForScreen screen cssProvider 1000
    pure window

writeCell :: Cell -> Char -> IO ()
writeCell cell char = #setLabel cell (T.singleton char)

-- Записывает число в ячейку, выбранное всплывающем окном
writePopoverRelativeCell :: G.Popover -> Char -> IO ()
writePopoverRelativeCell popover char = do
    widget <- #getRelativeTo popover
    cell <- G.unsafeCastTo G.Button widget
    writeCell cell char
    #hide popover

-- Записывает решение в ячейку
solveCell :: Cell -> IO ()
solveCell cell = do
    char <- T.head <$> #getName cell
    writeCell cell char

-- Записывает решение во все ячейки
solveAllCells :: Cells -> IO ()
solveAllCells = mapM_ solveCell

-- Связывает ячейки и всплывающее окно
cellsBind :: Cells -> G.Popover -> IO ()
cellsBind cells popover = mapM_ (\c -> on c #focusInEvent $ focusInHandler c) cells
    where focusInHandler cell _ = do
            set popover [#relativeTo := cell]
            #show popover
            pure False

-- Связывает кнопки всплывающего окна и само всплывающее окно
numbersBind :: [G.Button] -> G.Popover -> IO ()
numbersBind buttons popover = mapM_ (\b -> on b #clicked $ numberButtonInsert b popover) buttons

-- Записывает в ячейку число, выбранное в всплывающем окне
numberButtonInsert :: G.Button -> G.Popover -> IO ()
numberButtonInsert button popover = do
    label <- #getLabel button
    writePopoverRelativeCell popover $ T.head label

-- Проверяет ячейку и маркирует ёё красным ответа, если она неверная
checkCell :: Cell -> IO Bool
checkCell cell = do
    solution <- T.head <$> (G.toWidget cell >>= #getName)
    actual <- T.head <$> #getLabel cell
    let isCorrect = actual == solution
    style <- #getStyleContext cell
    if not isCorrect
        then #addClass style "incorrect"
        else pure ()
    forkIO $ threadDelay 800000 >> #removeClass style "incorrect"
    pure isCorrect

-- Проверяет все клетки и отмечает их зелённым цветом, если они верны
checkAllCells :: Cells -> IO ()
checkAllCells cells = do
    allAreCorrect <- and <$> mapM checkCell cells
    if allAreCorrect
        then mapM_ (\cell -> do
            style <- #getStyleContext cell
            #addClass style "correct"
            forkIO $ threadDelay 800000 >> #removeClass style "correct"
        ) cells
        else pure ()

-- Запускает новую игру с генерацией нового поля
newGame :: Cells -> G.Popover -> IO ()
newGame cells popover = do
    #hide popover
    (Just sudoku, Just solution) <- generateSudoku 0
    writeSudoku cells sudoku
    writeSolution cells solution

-- Записывает судоку в ячейки и деактивирует ячейки с числами
writeSudoku :: Cells -> Sudoku -> IO ()
writeSudoku cells sudoku = do
    let sudokuChars = toString sudoku
    zipWithM_ (\c sc -> do
            writeCell c sc
            if sc == blankval
                then set c [#sensitive := True]
                else set c [#sensitive := False]
        ) cells sudokuChars

-- Сохраняет правильное решение в именах ячеек
writeSolution :: Cells -> Sudoku -> IO ()
writeSolution cells sudoku = do
    let sudokuChars = toString sudoku
    zipWithM_ (\c sc -> #setName c (T.singleton sc)) cells sudokuChars
