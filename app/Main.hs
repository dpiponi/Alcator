{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}

module Main where

import Prelude hiding (last, init, null)
import AcornAtom
import Binary
import Control.Monad
import Control.Monad.Reader
import Data.Array.IO
import System.Directory
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
-- import SDL.Event
import Step
import System.Console.CmdArgs hiding ((+=))
-- import qualified SDL
import Graphics.UI.GLFW
import Data.IORef
import Data.Dequeue

data Args = Args { file :: String, options :: String, command :: Maybe String, debugStart :: Bool,  workingDirectory :: String } deriving (Show, Data, Typeable)

clargs :: Args
clargs = Args { file = "adventure.bin",
                options = ".alcator-options",
                command = Nothing,
                debugStart = False,
                workingDirectory = "." }

main :: IO ()
main = do
    fontData <- readFont "font.txt"
    args' <- cmdArgs clargs

    let optionsFile = options args'
    let startCommand = command args'
    let directory = workingDirectory args'
    putStrLn $ "Reading options from '" ++ optionsFile ++ "'"
    putStrLn $ "Debug = " ++ show (debugStart args')
    optionsString <- readFile optionsFile
    let options' = read optionsString :: Options
    print options'
    let screenScaleX' = screenScaleX options'
    let screenScaleY' = screenScaleY options'
    let controllerTypeString = controllerTypes options'
    let controllerType = read controllerTypeString
    let alpha = motionBlurAlpha options'

    rc <- init -- init video
    when (not rc) $ die "Couldn't init graphics"
    queueRef <- newIORef empty
    window <- makeMainWindow screenScaleX' screenScaleY' queueRef

    (prog, attrib, tex', textureData') <- initResources alpha fontData

    romArray <- newArray (0, 0x5fff) 0 :: IO (IOUArray Int Word8)
    ramArray <- newArray (0, 0x9fff) 0 :: IO (IOUArray Int Word8)
    readBinary romArray "acorn_roms/Atom_Kernel.rom" (0xf000 - 0xa000)
    readBinary romArray "acorn_roms/Atom_Basic.rom" (0xc000 - 0xa000)
    readBinary romArray "acorn_roms/Atom_FloatingPoint.rom" (0xd000 - 0xa000)
--     readBinary romArray "acorn_roms/Atom_pcharme.rom" (0xa000 - 0xa000)
--     readBinary romArray "acorn_roms/Atom_Toolkit.rom" (0xa000 - 0xa000)
    readBinary romArray "utility.bin" (0xa000 - 0xa000)
--     readBinary ramArray "software/BB/PINBALL" (0x2900-22)
--     readBinary ramArray "software/BB/GALAXBB" (0x2900-22)
--     readBinary ramArray "software/AS/ADVENT/ADVENTUR" (0x2900-22)
--     readBinary ramArray "software/BB/INVADBB" (0x2900-22)
--     readBinary ramArray "software/BB/LUNARBB" (0x2900-22)
--     readBinary ramArray "software/L9/DUNGEON/DUNGEON" (0xe00-22)
--     readBinary ramArray "software/L9/DUNGEON/DUNGEON" (0x400-22)
--     readBinary ramArray "JSW2CODE" (0x400-22)
--     readBinary ramArray "elite.atm" (0x400-22)
--     readBinary ramArray "software/BB/CHESSBB" (0x2900-22)
--     readBinary ramArray "software/BB/INVADBB" (0x2900-22)
--     readBinary ramArray "acorn_roms/Atom_Demo.rom" (0x2900)

    withCurrentDirectory directory $ do
        state <- initState screenScaleX' screenScaleY'
                           (screenWidth*screenScaleX') (screenHeight*screenScaleY')
                           ramArray
                           romArray
                           0x0000 window prog attrib tex' textureData'
                           controllerType

        let keyCallback atomState _window key someInt action mods = do
                  let pressed = isPressed action
                  flip runReaderT atomState $ unM $ 
                    case key of
                      Key'RightAlt -> updateRept pressed
                      _ -> do
                          done <- updatePPIA key pressed
                          when (not done) $ liftIO $ atomicModifyIORef' queueRef (\q -> (pushBack q (UIKey key someInt action mods), ()))
        setKeyCallback window (Just (keyCallback state))

        --  Not at all clear this should work with GLFW
        --  though it appears to on OSX
        let poller = pollEvents >> poller
        void $ forkIO poller

        let loop = do
                queue <- liftIO $ readIORef queueRef
                when (not (null queue)) $ do
                    let Just (queuedKey, queue') = popFront queue
                    liftIO $ writeIORef queueRef queue'
                    let UIKey {uiKey = key, uiState = motion} = queuedKey
                    handleKey motion key
                loopUntil 1000

                loop

        _ <- flip runReaderT state $ unM $ do
                initHardware
                case startCommand of
                    Nothing -> return()
                    Just c -> void $ execLine c
                when (debugStart args') runDebugger
                resetNextFrame
                loop

        destroyWindow window
        -- XXX Free malloced data?
        terminate
