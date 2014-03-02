module Board where

import System.IO (IOMode (..), withFile, hPutStrLn)
import Control.Monad.RWS.Strict (gets, modify, liftIO)
import Data.Array.IArray
import System.Random
import Graphics.UI.SDL

import Types
import Utils

randomBoard :: Int -> Int -> IO Board
randomBoard w h = do
    seed <- newStdGen
    return $ listArray ((1,1), (w, h)) $ zip (randomRs (1, 6) seed) (repeat True)

drawBoard :: Surface -> Board -> IO ()
drawBoard surface board = do
    drawRect surface (Rect (bx1*s - 1) (by1*s - 1) (bx2*s + 2) (by2*s + 2)) 0
    mapM_ (\i@(x,y) -> drawRect surface (Rect (x*s) (y*s) s s) (fst $ board ! i)) $ indices board
    where s = tileSize
          ((bx1, by1), (bx2, by2)) = bounds board

changeColor :: Int -> Game ()
changeColor col = do
    liftIO $ withFile "log.txt" AppendMode (\h -> hPutStrLn h ("Changed color to: " ++ show col))
    board <- gets stateBoard
    let newBoard = changeIndexColor board (1, 1) col
    modify $ \s -> s {
        stateBoard = newBoard // [(i, (col, True)) | i <- indices newBoard, not (snd $ newBoard ! i)]
        }

changeIndexColor :: Board -> (Int, Int) -> Int -> Board
changeIndexColor board index col = foldl (\acc i -> changeIndexColor acc i col) newBoard nodes
    where newBoard = setColor board index col
          nodes    = adjacentNodes board index

setColor :: Board -> (Int, Int) -> Int -> Board
setColor board index col = board // [(i, (col, False)) | i <- [index]]

inBounds :: (Int, Int) -> ((Int, Int), (Int, Int)) -> Bool
inBounds (i, j) ((x1, y1), (x2, y2)) =
    let inX = (i >= x1 && i <= x2)
        inY = (j >= y1 && j <= y2)
    in  inX && inY

adjacentNodes :: Board -> (Int, Int) -> [(Int, Int)]
adjacentNodes board index@(x, y) = filter (\i -> inBounds i (bounds board) && mutable i && sameCol i) nodes
    where nodes      = (x - 1, y):(x + 1, y):(x, y - 1):(x, y + 1):[]
          mutable ni = snd $ board ! ni
          sameCol ni = (fst $ board ! ni) == (fst $ board ! index)
