{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module Main where

import AcornAtom hiding (ram, rom)
import Binary
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Data.Array.IO
import Data.Binary hiding (get)
import Data.Char
import Data.IORef
import Debugger
import Display
import Emulation
import Events
import Graphics.UI.GLFW hiding (getTime)
import Keys
import Metrics
import Prelude hiding (last, init, null)
import Stella
import Step
import System.Clock
import System.Console.CmdArgs hiding ((+=))
import System.Directory
import System.Exit
import System.Random
import qualified Data.ByteString.Internal as BS (c2w)
import qualified Data.Set.Ordered as O

data Args = Args { command :: Maybe String,
                   debugStart :: Bool,
                   workingDirectory :: String } deriving (Show, Data, Typeable)

clargs :: Args
clargs = Args { command = Nothing,
                debugStart = False,
                workingDirectory = "." }

loopEmulation :: Window -> TQueue Word8 -> IORef (O.OSet Key) ->
                 TQueue UIKey -> MonadAcorn ()
loopEmulation window key_buffer' key_set queue = do
  maybeKey <- liftIO $ atomically $ tryReadTQueue queue
  case maybeKey of
      Nothing -> return ()
      Just queuedKey -> do
        case queuedKey of
          UIKey { uiKey=key,
                  uiState=KeyState'Pressed,
                  uiMods=ModifierKeys { modifierKeysSuper=True} } ->
                    superKey window key_buffer' key
          UIKey {uiKey=key, uiState=motion, uiMods=mods} -> do
            case interpretKey' queuedKey of
              Nothing -> return ()
              Just k -> liftIO $ atomically $ writeTQueue key_buffer' (BS.c2w k)
            void $ newUpdatePPIA key_set key (isPressed motion)

  loopUntil 1000

  loopEmulation window key_buffer' key_set queue

interpretKey' UIKey {uiKey=key, uiState=motion, uiMods=mods} = interpretKey key motion mods

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

rom_list :: [(FilePath, Word16)]
rom_list = [
    ("acorn_roms/Atom_Kernel.rom", 0xf000),
    ("acorn_roms/Atom_Basic.rom", 0xc000),
    ("acorn_roms/Atom_FloatingPoint.rom", 0xd000),
    ("acorn_roms/Atom_pcharme.rom", 0xa000)]
--     readBinary romArray "acorn_roms/Atom_Toolkit.rom" (0xa000 - 0xa000)
--     readBinary romArray "utility.bin" (0xa000 - 0xa000)

loadROMS :: IOUArray Int Word8 -> IO ()
loadROMS rom_array = forM_ rom_list $ \(rom_file, rom_address) ->
                      readBinary rom_array rom_file (rom_address - 0xa000)

translate :: Char -> Char
translate x | x == chr 10 = chr 13
translate x = x

pasteKey :: Window -> TQueue Word8 -> IO ()
pasteKey window key_buffer' = do
  s <- getClipboardString window
  case s of
    Nothing -> return ()
    Just pasted_string -> forM_ pasted_string $ \k ->
        atomically $ writeTQueue key_buffer' (BS.c2w $ translate k)

superKey :: Window -> TQueue Word8 -> Key -> MonadAcorn ()
superKey window key_buffer' Key'V = liftIO $ pasteKey window key_buffer'
superKey _ _ Key'D = do
  Emulation.dumpState
  runDebugger
  resetNextFrame
superKey _ _ _ = return ()

keyCallback :: AcornAtom -> TQueue UIKey
             -> Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyCallback atomState queue _window key someInt action mods =
  flip runReaderT atomState $ unM $ do
    t <- liftIO $ getTime Realtime
    liftIO $ atomically $ writeTQueue queue (UIKey key someInt action mods t)

startingState :: TQueue Word8 -> IO AcornAtom
startingState key_buffer' = do
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

    initState ram rom 0x0000 graphicsState' key_buffer'

main :: IO ()
main = do
    args' <- cmdArgs clargs
    key_buffer' <- newTQueueIO
    state <- startingState key_buffer'
    let window = state ^. graphicsState . sdlWindow

    let directory = workingDirectory args'
    let startCommand = command args'

    --  Not at all clear this should work with GLFW
    --  though it appears to on OSX and Linux
    queue <- newTQueueIO
    key_set <- newIORef O.empty
    setKeyCallback window (Just $ keyCallback state queue)
    void $ setWindowCloseCallback window $ Just $ \_ -> exitSuccess
    void $ forkIO $ forever pollEvents

    withCurrentDirectory directory $
        void $ withAtom state $ do
            initHardware
            case startCommand of
                Nothing -> return()
                Just c -> void $ execLine c
            when (debugStart args') runDebugger
            resetNextFrame
            loopEmulation window key_buffer' key_set queue

    destroyWindow window
    -- XXX Free malloced data?
    terminate
