module Types where

import Control.Monad.RWS.Strict
import Data.Time.Clock
import Data.Array.IArray

import Graphics.UI.SDL

type Game = RWST GameEnv () GameState IO

data GameState = GameState {
    stateRunning     :: !Bool,
    stateBoard       :: !Board,
    stateDrawLoops   :: !Int,
    stateDrawTime    :: !NominalDiffTime,
    stateUpdateLoops :: !Int,
    stateUpdateTime  :: !NominalDiffTime
    }

data GameEnv = GameEnv {
    envScreen :: !Surface,
    envBuffer :: !Surface
    }

type Board  = Array (Int, Int) (Int, Bool)
type Button = (Rect, Surface -> IO (), Game ()) -- (Bounding rectangle, Draw function, Callback function)
