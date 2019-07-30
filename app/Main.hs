{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module Main where

import AcornAtom hiding (ram, rom)
import qualified Data.ByteString.Internal as BS (c2w)
import Binary
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Data.Array.IO
import Data.Binary hiding (get)
import Data.IORef
import Debugger
import Data.Char
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
import qualified Data.Array as A
import qualified Data.Foldable as F
import qualified Data.Set.Ordered as O

data Args = Args { command :: Maybe String,
                   debugStart :: Bool,
                   workingDirectory :: String } deriving (Show, Data, Typeable)


clargs :: Args
clargs = Args { command = Nothing,
                debugStart = False,
                workingDirectory = "." }

loopEmulation :: IORef (O.OSet Key) -> TQueue UIKey -> MonadAcorn ()
loopEmulation key_set queue = do
        maybeKey <- liftIO $ atomically $ tryReadTQueue queue
        case maybeKey of
            Nothing -> return ()
            Just queuedKey -> do
                let UIKey {uiKey = key, uiState = motion} = queuedKey
--                 liftIO $ print (key, motion)
                newUpdatePPIA key_set key (isPressed motion)
                handleKey motion key
        loopUntil 1000

        loopEmulation key_set queue

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

interpretKey :: Key -> KeyState -> ModifierKeys -> Maybe Char
interpretKey _ KeyState'Released _ = Nothing

interpretKey Key'1 _ ModifierKeys {modifierKeysShift=False} = Just '1'
interpretKey Key'1 _ ModifierKeys {modifierKeysShift=True} = Just '!'
interpretKey Key'2 _ ModifierKeys {modifierKeysShift=False} = Just '2'
interpretKey Key'2 _ ModifierKeys {modifierKeysShift=True} = Just '@'
interpretKey Key'3 _ ModifierKeys {modifierKeysShift=False} = Just '3'
interpretKey Key'3 _ ModifierKeys {modifierKeysShift=True} = Just '#'
interpretKey Key'4 _ ModifierKeys {modifierKeysShift=False} = Just '4'
interpretKey Key'4 _ ModifierKeys {modifierKeysShift=True} = Just '$'
interpretKey Key'5 _ ModifierKeys {modifierKeysShift=False} = Just '5'
interpretKey Key'5 _ ModifierKeys {modifierKeysShift=True} = Just '%'
interpretKey Key'6 _ ModifierKeys {modifierKeysShift=False} = Just '6'
interpretKey Key'6 _ ModifierKeys {modifierKeysShift=True} = Just '^'
interpretKey Key'7 _ ModifierKeys {modifierKeysShift=False} = Just '7'
interpretKey Key'7 _ ModifierKeys {modifierKeysShift=True} = Just '&'
interpretKey Key'8 _ ModifierKeys {modifierKeysShift=False} = Just '8'
interpretKey Key'8 _ ModifierKeys {modifierKeysShift=True} = Just '*'
interpretKey Key'9 _ ModifierKeys {modifierKeysShift=False} = Just '9'
interpretKey Key'9 _ ModifierKeys {modifierKeysShift=True} = Just '('
interpretKey Key'0 _ ModifierKeys {modifierKeysShift=False} = Just '0'
interpretKey Key'0 _ ModifierKeys {modifierKeysShift=True} = Just ')'
interpretKey Key'Minus _ ModifierKeys {modifierKeysShift=False} = Just '-'
interpretKey Key'Minus _ ModifierKeys {modifierKeysShift=True} = Nothing
interpretKey Key'Equal _ ModifierKeys {modifierKeysShift=False} = Just '='
interpretKey Key'Equal _ ModifierKeys {modifierKeysShift=True} = Just '+'
interpretKey Key'Backspace _ ModifierKeys {modifierKeysShift=False} = Just (chr 127)

interpretKey Key'Q _ ModifierKeys {modifierKeysShift=False} = Just 'Q'
interpretKey Key'Q _ ModifierKeys {modifierKeysShift=True} = Just 'q'
interpretKey Key'W _ ModifierKeys {modifierKeysShift=False} = Just 'W'
interpretKey Key'W _ ModifierKeys {modifierKeysShift=True} = Just 'w'
interpretKey Key'E _ ModifierKeys {modifierKeysShift=False} = Just 'E'
interpretKey Key'E _ ModifierKeys {modifierKeysShift=True} = Just 'e'
interpretKey Key'R _ ModifierKeys {modifierKeysShift=False} = Just 'R'
interpretKey Key'R _ ModifierKeys {modifierKeysShift=True} = Just 'r'
interpretKey Key'T _ ModifierKeys {modifierKeysShift=False} = Just 'T'
interpretKey Key'T _ ModifierKeys {modifierKeysShift=True} = Just 't'
interpretKey Key'Y _ ModifierKeys {modifierKeysShift=False} = Just 'Y'
interpretKey Key'Y _ ModifierKeys {modifierKeysShift=True} = Just 'y'
interpretKey Key'U _ ModifierKeys {modifierKeysShift=False} = Just 'U'
interpretKey Key'U _ ModifierKeys {modifierKeysShift=True} = Just 'u'
interpretKey Key'I _ ModifierKeys {modifierKeysShift=False} = Just 'I'
interpretKey Key'I _ ModifierKeys {modifierKeysShift=True} = Just 'i'
interpretKey Key'O _ ModifierKeys {modifierKeysShift=False} = Just 'O'
interpretKey Key'O _ ModifierKeys {modifierKeysShift=True} = Just 'o'
interpretKey Key'P _ ModifierKeys {modifierKeysShift=False} = Just 'P'
interpretKey Key'P _ ModifierKeys {modifierKeysShift=True} = Just 'p'
interpretKey Key'LeftBracket _ ModifierKeys {modifierKeysShift=False} = Just '['
interpretKey Key'LeftBracket _ ModifierKeys {modifierKeysShift=True} = Just '{'
interpretKey Key'RightBracket _ ModifierKeys {modifierKeysShift=False} = Just ']'
interpretKey Key'RightBracket _ ModifierKeys {modifierKeysShift=True} = Just '}'
interpretKey Key'Backslash _ ModifierKeys {modifierKeysShift=False} = Just '\\'
interpretKey Key'Backslash _ ModifierKeys {modifierKeysShift=True} = Nothing

interpretKey Key'A _ ModifierKeys {modifierKeysShift=False} = Just 'A'
interpretKey Key'A _ ModifierKeys {modifierKeysShift=True} = Just 'a'
interpretKey Key'S _ ModifierKeys {modifierKeysShift=False} = Just 'S'
interpretKey Key'S _ ModifierKeys {modifierKeysShift=True} = Just 's'
interpretKey Key'D _ ModifierKeys {modifierKeysShift=False} = Just 'D'
interpretKey Key'D _ ModifierKeys {modifierKeysShift=True} = Just 'd'
interpretKey Key'F _ ModifierKeys {modifierKeysShift=False} = Just 'F'
interpretKey Key'F _ ModifierKeys {modifierKeysShift=True} = Just 'f'
interpretKey Key'G _ ModifierKeys {modifierKeysShift=False} = Just 'G'
interpretKey Key'G _ ModifierKeys {modifierKeysShift=True} = Just 'g'
interpretKey Key'H _ ModifierKeys {modifierKeysShift=False} = Just 'H'
interpretKey Key'H _ ModifierKeys {modifierKeysShift=True} = Just 'h'
interpretKey Key'J _ ModifierKeys {modifierKeysShift=False} = Just 'J'
interpretKey Key'J _ ModifierKeys {modifierKeysShift=True} = Just 'j'
interpretKey Key'K _ ModifierKeys {modifierKeysShift=False} = Just 'K'
interpretKey Key'K _ ModifierKeys {modifierKeysShift=True} = Just 'k'
interpretKey Key'L _ ModifierKeys {modifierKeysShift=False} = Just 'L'
interpretKey Key'L _ ModifierKeys {modifierKeysShift=True} = Just 'l'
interpretKey Key'Semicolon _ ModifierKeys {modifierKeysShift=False} = Just ';'
interpretKey Key'Semicolon _ ModifierKeys {modifierKeysShift=True} = Just ':'
interpretKey Key'Apostrophe _ ModifierKeys {modifierKeysShift=False} = Just '\''
interpretKey Key'Apostrophe _ ModifierKeys {modifierKeysShift=True} = Just '"'
interpretKey Key'Enter _ ModifierKeys {modifierKeysShift=False} = Just '\r'

interpretKey Key'Z      _ ModifierKeys {modifierKeysShift=False} = Just 'Z'
interpretKey Key'Z      _ ModifierKeys {modifierKeysShift=True}  = Just 'z'
interpretKey Key'X      _ ModifierKeys {modifierKeysShift=False} = Just 'X'
interpretKey Key'X      _ ModifierKeys {modifierKeysShift=True}  = Just 'x'
interpretKey Key'C      _ ModifierKeys {modifierKeysShift=False} = Just 'C'
interpretKey Key'C      _ ModifierKeys {modifierKeysShift=True}  = Just 'c'
interpretKey Key'V      _ ModifierKeys {modifierKeysShift=False} = Just 'V'
interpretKey Key'V      _ ModifierKeys {modifierKeysShift=True}  = Just 'v'
interpretKey Key'B      _ ModifierKeys {modifierKeysShift=False} = Just 'B'
interpretKey Key'B      _ ModifierKeys {modifierKeysShift=True}  = Just 'b'
interpretKey Key'N      _ ModifierKeys {modifierKeysShift=False} = Just 'N'
interpretKey Key'N      _ ModifierKeys {modifierKeysShift=True}  = Just 'n'
interpretKey Key'M      _ ModifierKeys {modifierKeysShift=False} = Just 'M'
interpretKey Key'M      _ ModifierKeys {modifierKeysShift=True}  = Just 'm'
interpretKey Key'Comma  _ ModifierKeys {modifierKeysShift=False} = Just ','
interpretKey Key'Comma  _ ModifierKeys {modifierKeysShift=True}  = Just '<'
interpretKey Key'Period _ ModifierKeys {modifierKeysShift=False} = Just '.'
interpretKey Key'Period _ ModifierKeys {modifierKeysShift=True}  = Just '>'
interpretKey Key'Slash  _ ModifierKeys {modifierKeysShift=False} = Just '/'
interpretKey Key'Slash  _ ModifierKeys {modifierKeysShift=True}  = Just '?'

interpretKey Key'Space _ ModifierKeys {modifierKeysShift=False} = Just ' '

interpretKey _ _ _ = Nothing

translate :: Char -> Char
translate x | x == chr 10 = chr 13
translate x = x

keyCallback :: TQueue Word8 -> IORef (O.OSet Key) -> AcornAtom -> TQueue UIKey
             -> Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyCallback key_buffer' key_set atomState queue window key someInt action mods = do
--   print key
  let pressed = isPressed action
  flip runReaderT atomState $ unM $ 
    case key of
      Key'RightAlt -> updateRept pressed
      _ -> do
          t <- liftIO $ getTime Realtime
          liftIO $ atomically $ writeTQueue queue (UIKey key someInt action mods t)
          if (key == Key'V && modifierKeysSuper mods && action == KeyState'Pressed)
              then do
                s <- liftIO $ getClipboardString window
                case s of
                    Nothing -> return ()
                    Just t ->
                        forM_ (map translate t) $ \k -> liftIO $ atomically $ writeTQueue key_buffer' (BS.c2w k)
              else case interpretKey key action mods of
                    Nothing -> return ()
                    Just k -> liftIO $ atomically $ writeTQueue key_buffer' (BS.c2w k)

-- charCallback :: TQueue Word8 -> Window -> Char -> IO ()
-- charCallback key_buffer' _window key = do
--   print key
--   atomically $ writeTQueue key_buffer' (BS.c2w key)


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
    setKeyCallback window (Just (keyCallback key_buffer' key_set state queue))
--     setCharCallback window (Just (charCallback key_buffer'))
    void $ forkIO $ forever pollEvents

    withCurrentDirectory directory $
        void $ withAtom state $ do
            initHardware
            case startCommand of
                Nothing -> return()
                Just c -> void $ execLine c
            when (debugStart args') runDebugger
            resetNextFrame
            loopEmulation key_set queue

    destroyWindow window
    -- XXX Free malloced data?
    terminate
