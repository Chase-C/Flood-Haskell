module Utils where

import System.Random
import Control.Monad
import Graphics.UI.SDL

boardWidth :: Int
boardWidth = 14
boardHeight :: Int
boardHeight = 14

tileSize :: Int
tileSize = 32

screenWidth :: Int
screenWidth = (boardWidth + 4) * tileSize
screenHeight :: Int
screenHeight = (boardHeight + 2) * tileSize

getRandom :: Int -> Int -> IO Int
getRandom l u = do
    seed <- getStdGen
    let (n, s) = randomR (l, u) seed
    setStdGen s
    return n

intersects :: (Int, Int) -> Rect -> Bool
intersects (px, py) (Rect rx ry w h) =
    let inX = (px >= rx && px <= rx + w)
        inY = (py >= ry && py <= ry + h)
    in  inX && inY

drawRect :: Surface -> Rect -> Int -> IO ()
drawRect surface rect colInt = do
    color <- lookUpColor surface colInt
    void $ fillRect surface (Just rect) color

lookUpColor :: Surface -> Int -> IO Pixel
lookUpColor s 0 = (mapRGB . surfaceGetPixelFormat) s 0x00 0x00 0x00
lookUpColor s 1 = (mapRGB . surfaceGetPixelFormat) s 0xff 0x00 0x00
lookUpColor s 2 = (mapRGB . surfaceGetPixelFormat) s 0x00 0xff 0x00
lookUpColor s 3 = (mapRGB . surfaceGetPixelFormat) s 0x00 0x00 0xff
lookUpColor s 4 = (mapRGB . surfaceGetPixelFormat) s 0xff 0xff 0x00
lookUpColor s 5 = (mapRGB . surfaceGetPixelFormat) s 0x00 0xff 0xff
lookUpColor s _ = (mapRGB . surfaceGetPixelFormat) s 0xff 0x00 0xff
