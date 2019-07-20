module Stella where

import AcornAtom
import Control.Lens
import Control.Monad.IO.Class
import Data.IORef
import Data.Int
import Metrics
import Prelude hiding (last, and)
import System.Clock

-- add nanoseconds
addTime :: TimeSpec -> Int64 -> TimeSpec
addTime (TimeSpec a b) c =
    let d = b + c
    in if d >= 1000000000 then TimeSpec (a+1) (d-1000000000) else TimeSpec a d

gtTime :: TimeSpec -> TimeSpec -> Bool
gtTime (TimeSpec sec0 nsec0) (TimeSpec sec1 nsec1) =
    sec0 > sec1 || sec0 == sec1 && nsec0 > nsec1

-- If the emulator is just starting, or restarting after a pause,
-- then the time for the next frame needs to be pushed forward
-- until after the current time.
resetNextFrame :: MonadAcorn ()
resetNextFrame = do
    liftIO $ print "Resetting clock"
    t <- liftIO $ getTime Realtime
    let nt = addTime t (1000000000 `div` fps)
    nextFrameTimeRef <- view nextFrameTime
    liftIO $ writeIORef nextFrameTimeRef nt
