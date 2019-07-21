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
import Control.Monad.Reader
import Data.Array.IO
import System.Directory
import Control.Lens
import Data.Binary hiding (get)
import Debugger
import System.Exit
import Display
import Control.Concurrent
import Emulation
import Events
import Keys
import Metrics
import Stella
import Step
import System.Console.CmdArgs hiding ((+=))
import Graphics.UI.GLFW
import Data.IORef
import Data.Dequeue

data Args = Args { options :: String,
                   command :: Maybe String,
                   debugStart :: Bool,
                   workingDirectory :: String } deriving (Show, Data, Typeable)


clargs :: Args
clargs = Args { options = ".alcator-options",
                command = Nothing,
                debugStart = False,
                workingDirectory = "." }

loopEmulation :: IORef (BankersDequeue UIKey) -> MonadAcorn ()
loopEmulation queueRef = do
        queue <- liftIO $ readIORef queueRef
        unless (null queue) $ do
            let Just (queuedKey, queue') = popFront queue
            liftIO $ writeIORef queueRef queue'
            let UIKey {uiKey = key, uiState = motion} = queuedKey
            handleKey motion key
        loopUntil 1000

        loopEmulation queueRef

-- 40K RAM
-- 24K ROM
createMemory :: IO (IOUArray Int Word8, IOUArray Int Word8)
createMemory = do
    ram <- newArray (0, 0x9fff) 0 :: IO (IOUArray Int Word8)
    rom <- newArray (0, 0x5fff) 0 :: IO (IOUArray Int Word8)
    return (rom, ram)

loadROMS :: IOUArray Int Word8 -> IO ()
loadROMS romArray = do
    readBinary romArray "acorn_roms/Atom_Kernel.rom" (0xf000 - 0xa000)
    readBinary romArray "acorn_roms/Atom_Basic.rom" (0xc000 - 0xa000)
    readBinary romArray "acorn_roms/Atom_FloatingPoint.rom" (0xd000 - 0xa000)
--     readBinary romArray "acorn_roms/Atom_pcharme.rom" (0xa000 - 0xa000)
--     readBinary romArray "acorn_roms/Atom_Toolkit.rom" (0xa000 - 0xa000)
    readBinary romArray "utility.bin" (0xa000 - 0xa000)

keyCallback :: AcornAtom -> IORef (BankersDequeue UIKey)
             -> Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyCallback atomState queueRef _window key someInt action mods = do
  let pressed = isPressed action
  flip runReaderT atomState $ unM $ 
    case key of
      Key'RightAlt -> updateRept pressed
      _ -> do
          done <- updatePPIA key pressed
          unless done $ liftIO $ atomicModifyIORef' queueRef (\q -> (pushBack q (UIKey key someInt action mods), ()))

startingState :: Args -> IO AcornAtom
startingState args' = do
    fontData <- readFont "font.txt"

    let optionsFile = options args'

    putStrLn $ "Reading options from '" ++ optionsFile ++ "'"
    optionsString <- readFile optionsFile
    let options' = read optionsString :: Options
    let screenScale' = screenScale options'
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
--         _xscale = fst screenScale',
--         _yscale = snd screenScale',
        _windowWidth = screenWidth,
        _windowHeight = screenHeight
    }

    initState ram rom 0x0000 graphicsState'

main :: IO ()
main = do
    args' <- cmdArgs clargs
    state <- startingState args'
    let window = state ^. graphicsState . sdlWindow

    let directory = workingDirectory args'
    let startCommand = command args'

    --  Not at all clear this should work with GLFW
    --  though it appears to on OSX and Linux
    queueRef <- newIORef @(BankersDequeue UIKey) empty
    setKeyCallback window (Just (keyCallback state queueRef))
    void $ forkIO $ forever pollEvents

    withCurrentDirectory directory $
        void $ withAtom state $ do
            initHardware
            case startCommand of
                Nothing -> return()
                Just c -> void $ execLine c
            when (debugStart args') runDebugger
            resetNextFrame
            loopEmulation queueRef

    destroyWindow window
    -- XXX Free malloced data?
    terminate
