module Main where

import System.IO                (IOMode (..), withFile, hPutStrLn)
import Control.Monad            (void, unless)
import Control.Monad.RWS.Strict (RWST, ask, asks, evalRWST, get, gets, liftIO, modify, put)
import Data.Time.Clock

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

    board    <- randomBoard boardWidth boardHeight
    currTime <- getCurrentTime
    let diffTime = currTime `diffUTCTime` currTime
    let gameEnv   = GameEnv {
            envScreen = screen,
            envBuffer = buffer
            }
        gameState = GameState {
            stateRunning     = True,
            stateBoard       = board,
            stateDrawLoops   = 0,
            stateDrawTime    = diffTime,
            stateUpdateLoops = 0,
            stateUpdateTime  = diffTime
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
            startTime   <- liftIO getCurrentTime

            (mx, my, _) <- liftIO getMouseState
            pressed (mx, my)

            endTime         <- liftIO getCurrentTime
            totalUpdateTime <- gets stateUpdateTime
            loops           <- gets stateUpdateLoops
            let thisUpdateTime = endTime `diffUTCTime` startTime
            modify $ \s -> s {
                stateUpdateLoops = loops + 1,
                stateUpdateTime  = totalUpdateTime + thisUpdateTime
            }

        Quit -> do
            modify $ \s -> s { stateRunning = False }
            printMetrics
            return ()
        _  -> return ()

draw :: Game ()
draw = do
    startTime <- liftIO getCurrentTime
    screen    <- asks envScreen
    buffer    <- asks envBuffer
    board     <- gets stateBoard

    liftIO $ do
        white <- (mapRGB . surfaceGetPixelFormat) screen 0xff 0xff 0xff
        void $ fillRect buffer (Just $ Rect 0 0 screenWidth screenHeight) white
        drawBoard buffer board
        drawUI    buffer
        void $ blitSurface buffer Nothing screen Nothing
        SDL.flip screen

    endTime       <- liftIO getCurrentTime
    totalDrawTime <- gets stateDrawTime
    loops         <- gets stateDrawLoops
    let thisDrawTime = endTime `diffUTCTime` startTime
    modify $ \s -> s {
        stateDrawLoops = loops + 1,
        stateDrawTime  = totalDrawTime + thisDrawTime
    }
