{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module Main where

import Prelude hiding (last, init, null)
import AcornAtom hiding (ram, rom)
import Binary
import Control.Monad
import qualified Data.Array as A
import Control.Monad.Reader
import qualified Data.Foldable as F
import Data.Array.IO
import Data.IORef
import qualified Data.Set.Ordered as O
import System.Directory
import Control.Lens
import Data.Binary hiding (get)
import Debugger
import System.Exit
import Display
import Control.Concurrent
import Control.Concurrent.STM
import Emulation
import Events
import Keys
import Metrics
import Stella
import Step
import System.Random
import System.Console.CmdArgs hiding ((+=))
import Graphics.UI.GLFW

data Args = Args { command :: Maybe String,
                   debugStart :: Bool,
                   workingDirectory :: String } deriving (Show, Data, Typeable)


clargs :: Args
clargs = Args { command = Nothing,
                debugStart = False,
                workingDirectory = "." }

loopEmulation :: TQueue UIKey -> MonadAcorn ()
loopEmulation queue = do
        maybeKey <- liftIO $ atomically $ tryReadTQueue queue
        case maybeKey of
            Nothing -> return ()
            Just queuedKey -> do
                let UIKey {uiKey = key, uiState = motion} = queuedKey
                handleKey motion key
        loopUntil 1000

        loopEmulation queue

-- 40K RAM
-- 24K ROM
createMemory :: IO (IOUArray Int Word8, IOUArray Int Word8)
createMemory = do
    ram <- newArray (0, 0x9fff) 0 :: IO (IOUArray Int Word8)
    rom <- newArray (0, 0x5fff) 0 :: IO (IOUArray Int Word8)

    -- Randomise BASIC random seed area.
    -- Real hardware relies on SRAM being initialised randomly.
    newStdGen >>= zipWithM_ (writeArray ram) [0..0x9fff] . randoms

    readBinary ram "utility.bin" (0x9800 - 0x0000)
    return (rom, ram)

loadROMS :: IOUArray Int Word8 -> IO ()
loadROMS romArray = do
    readBinary romArray "acorn_roms/Atom_Kernel.rom" (0xf000 - 0xa000)
    readBinary romArray "acorn_roms/Atom_Basic.rom" (0xc000 - 0xa000)
    readBinary romArray "acorn_roms/Atom_FloatingPoint.rom" (0xd000 - 0xa000)
    readBinary romArray "acorn_roms/Atom_pcharme.rom" (0xa000 - 0xa000)
--     readBinary romArray "acorn_roms/Atom_Toolkit.rom" (0xa000 - 0xa000)
--     readBinary romArray "utility.bin" (0xa000 - 0xa000)

keyCallback :: IORef (O.OSet Key) -> AcornAtom -> TQueue UIKey
             -> Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyCallback key_set atomState queue _window key someInt action mods = do
  let pressed = isPressed action
  flip runReaderT atomState $ unM $ 
    case key of
      Key'RightAlt -> updateRept pressed
      _ -> do
          done <- newUpdatePPIA key_set key pressed
          liftIO $ atomically $ writeTQueue queue (UIKey key someInt action mods)

startingState :: IO AcornAtom
startingState = do
    fontData <- readFont "font.txt"

    let screenScale' = (4, 4)
    let mode = 0

    rc <- init -- init video
    unless rc $ die "Couldn't init graphics"
    window <- makeMainWindow screenScale'

    (prog, attrib, tex', textureData') <- initResources mode fontData

    (rom, ram) <- createMemory
    loadROMS rom

    let graphicsState' =  GraphicsState {
        _sdlWindow = window,
        _textureData = textureData',
        _tex = tex',
        _glProg = prog,
        _glAttrib = attrib,
        _windowWidth = screenWidth,
        _windowHeight = screenHeight
    }

    initState ram rom 0x0000 graphicsState'

main :: IO ()
main = do
    args' <- cmdArgs clargs
    state <- startingState
    let window = state ^. graphicsState . sdlWindow

    let directory = workingDirectory args'
    let startCommand = command args'

    --  Not at all clear this should work with GLFW
    --  though it appears to on OSX and Linux
    queue <- newTQueueIO
    key_set <- newIORef O.empty
    setKeyCallback window (Just (keyCallback key_set state queue))
    void $ forkIO $ forever pollEvents

    withCurrentDirectory directory $
        void $ withAtom state $ do
            initHardware
            case startCommand of
                Nothing -> return()
                Just c -> void $ execLine c
            when (debugStart args') runDebugger
            resetNextFrame
            loopEmulation queue

    destroyWindow window
    -- XXX Free malloced data?
    terminate
