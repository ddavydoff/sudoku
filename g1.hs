-- |
-- Module      : g1
-- License     : Freeware
-- Maintainer  : Denis Davydoff (dendavydoff@gmi.com)
-- Stability   : Stable
-- Portability : Maybe
--
-- Sudoku solver
--

import Data.Map
import Data.Set
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Sudoku

main :: IO ()
main = do
    startSudoku

fieldSize@(fieldWidth, fieldHeight) = (27, 16) :: (Int, Int)

createField1 :: Field
createField1 = [[ 0, 0, 0, 0, 0, 0, 4, 0, 0], [ 3, 0, 6, 0, 0, 0, 0, 0, 0], [ 0, 0, 0, 1, 9, 6, 0, 3, 0], [ 0, 7, 0, 0, 0, 0, 0, 1, 0], [ 8, 0, 0, 2, 5, 0, 0, 9, 0], [ 0, 4, 0, 0, 0, 0, 8, 0, 0], [ 0, 6, 0, 4, 0, 9, 0, 0, 8], [ 0, 0, 5, 0, 0, 0, 0, 2, 0], [ 0, 0, 0, 5, 0, 0, 0, 0, 7]]::MatInt2d

createField2 :: Field
createField2 = solver createField1

type Field = [[Int]]

data SudokuState = GS
    { field1    :: Field
    , field2    :: Field
    , gameOver  :: Bool
    }

startSudoku :: IO ()

startSudoku = play (InWindow "SUDOKU" windowSize (240, 160)) white 30 (initState) renderer handler updater
windowSize = both (* (round cellSize)) fieldSize
cellSize = 24 :: Float

initState = GS createField1 createField2  False

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b) 

updater _ = id
--handler _ = id

cellToScreen = both ((* cellSize) . fromIntegral)

handler (EventKey (MouseButton RightButton) Down _ mouse) gs@GS
    { field1 = field1
    , field2 = field2
    } = gs
handler _ gs = gs

renderer GS { field1 = field1
    , field2 = field2 } = applyViewPortToPicture viewPort $ pictures $ cells1 ++ cells2 ++ grid1 ++ grid2 where
    grid1 = [uncurry translate (cellToScreen (x+2, y+4)) $ color black $ rectangleWire cellSize cellSize | x <- [0 .. 8], y <- [0 .. 8]]
    grid2 = [uncurry translate (cellToScreen (x+14, y+4)) $ color black $ rectangleWire cellSize cellSize | x <- [0 .. 8], y <- [0 .. 8]]
    cells1 = [uncurry translate (cellToScreen (x+2, y+4)) $ drawCell1 x y | x <- [0 .. 8], y <- [0 .. 8]]
    cells2 = [uncurry translate (cellToScreen (x+14, y+4)) $ drawCell2 x y | x <- [0 .. 8], y <- [0 .. 8]]
    drawCell1 x y = if (field1!!(8-y)!!x==0) then color white $ rectangleSolid cellSize cellSize else pictures [ color green $ rectangleSolid cellSize cellSize, label $ show (field1!!(8-y)!!x)]
    drawCell2 x y = if (field2!!(8-y)!!x==0) then color white $ rectangleSolid cellSize cellSize else pictures [ color green $ rectangleSolid cellSize cellSize, label $ show (field2!!(8-y)!!x)]
    label = translate (-5) (-5) . scale 0.15 0.15 . color black . text

viewPort = ViewPort (both (negate . (/ 2) . (subtract cellSize)) $ cellToScreen fieldSize) 0 1
