module UI where

import Graphics.UI.SDL
import Types
import Board
import Utils

rects = map (\y -> (y, Rect ((boardWidth + 2) * tileSize) (y * tileSize * 2) tileSize tileSize)) [1..6]

buttons :: [Button]
buttons = map (\(c, r) -> (r, (\s -> drawFunc s r c), changeColor c)) rects

drawFunc :: Surface -> Rect -> Int -> IO ()
drawFunc surface rect@(Rect x y w h) col = do
    drawRect surface (Rect (x - 1) (y - 1) (w + 2) (h + 2)) 0
    drawRect surface rect col

pressed :: (Int, Int) -> Game ()
pressed pos = mapM_ (\(r, _, c) -> if intersects pos r then c else return ()) buttons

drawUI :: Surface -> IO ()
drawUI surface = mapM_ (\(_, d, _) -> d surface) buttons
