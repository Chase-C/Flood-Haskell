module Main where

import System.IO                (IOMode (..), withFile, hPutStrLn)
import Control.Monad            (void, unless)
import Control.Monad.RWS.Strict (RWST, ask, asks, evalRWST, get, gets, liftIO, modify, put)
import Data.List
import Data.Word
import Data.Typeable
import Data.Maybe

import Graphics.UI.SDL as SDL

import Types
import Board
import UI
import Utils

main :: IO ()
main = withInit [InitEverything] $ do
    screen <- setVideoMode screenWidth screenHeight 32 [SWSurface]
    buffer <- createRGBSurface [SWSurface] screenWidth screenHeight 32 0 0 0 0
    enableEvent SDLMouseMotion False -- Without this, the game gets stuck in the event loop when the mouse moves

    board <- randomBoard 14 14
    let gameEnv   = GameEnv {
            envScreen = screen,
            envBuffer = buffer
            }
        gameState = GameState {
            stateRunning = True,
            stateBoard   = board
            }
    withFile "log.txt" WriteMode (\h -> return ())
    runGame gameEnv gameState

runGame :: GameEnv -> GameState -> IO ()
runGame ge gs = do
    void $ evalRWST run ge gs

run :: Game ()
run = do
    draw
    nextEvent
    running <- gets stateRunning
    unless (not running) run

nextEvent :: Game ()
nextEvent = do
    event <- liftIO waitEvent
    liftIO $ withFile "log.txt" AppendMode (\h -> hPutStrLn h ("Event: " ++ show event))
    case event of
        (MouseButtonUp x y ButtonLeft) -> do
            (mx, my, _) <- liftIO getMouseState
            pressed (mx, my)
        Quit -> do
            modify $ \s -> s { stateRunning = False }
            return ()
        _  -> return ()

draw :: Game ()
draw = do
    screen <- asks envScreen
    buffer <- asks envBuffer
    board  <- gets stateBoard

    liftIO $ do
        white <- (mapRGB . surfaceGetPixelFormat) screen 0xff 0xff 0xff
        void $ fillRect buffer (Just $ Rect 0 0 screenWidth screenHeight) white
        drawBoard buffer board
        drawUI    buffer
        void $ blitSurface buffer Nothing screen Nothing
        SDL.flip screen
