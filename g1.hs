import Data.Map
import Data.Set
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.Random

main :: IO ()
main = do
    gen <- getStdGen
    startGame gen

fieldSize@(fieldWidth, fieldHeight) = (27, 16) :: (Int, Int)

createField1 :: Field
createField1 = Data.Map.empty

createField2 :: Field
createField2 = Data.Map.empty

type Field = Map Cell CellState 
type Cell = (Int, Int)

data CellState = Int

data GameState = GS
    { field1    :: Field
    , field2    :: Field
    , gameOver :: Bool
    }

startGame :: StdGen -> IO ()
startGame gen = play (InWindow "SUDOKU" windowSize (240, 160)) white 30 (initState gen) renderer handler updater
windowSize = both (* (round cellSize)) fieldSize
cellSize = 24 :: Float

initState gen = GS createField1 createField2  False

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b) --вспомогательная функция, которая ещё пригодится 

updater _ = id

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
    drawCell1 x y = color white $ rectangleSolid cellSize cellSize
    drawCell2 x y = pictures [ color green $ rectangleSolid cellSize cellSize, label $ show 9]
    label = translate (-5) (-5) . scale 0.15 0.15 . color black . text

viewPort = ViewPort (both (negate . (/ 2) . (subtract cellSize)) $ cellToScreen fieldSize) 0 1
