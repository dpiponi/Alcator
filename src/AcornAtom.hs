{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AcornAtom(
                 MonadAcorn(..),
                 AcornAtom(..),
                 GraphicsState(..),
                 graphicsState,
                 key_buffer,
                 withAtom,
                 getX,
                 putX,
                 getPC,
                 putC,
                 getC,
                 putZ,
                 getZ,
                 frozen,
                 thawInto,
                 putI,
                 getI,
                 putD,
                 getD,
                 putB,
                 getB,
                 putV,
                 getV,
                 putN,
                 getN,
                 getA,
                 putA,
                 getS,
                 putS,
                 getP,
                 putP,
                 getY,
                 putY,
                 putPC,
                 addPC,
--                  xscale,
--                  yscale,
                 load,
                 store,
                 modify,
                 useAtomDebug,
                 modifyClock,
                 ram,
                 rom,
                 nextFrameTime,
                 useClock,
                 putAtomDebug,
                 sdlWindow,
                 textureData,
                 windowWidth,
                 windowHeight,
                 tex,
                 glProg,
                 glAttrib,
                 stellaDebug,
                 clock,
                 modifyAtomDebug
                 ) where

import Control.Lens
import Control.Monad.Reader
import System.Clock
import Data.Array.Base
import Data.Array.IO
import Data.Array
import Data.Bits.Lens
import Data.IORef
import Data.Int
import Data.Word
import qualified Graphics.UI.GLFW as GLFW
import Foreign.Ptr
import DebugState
import GHC.Generics
import qualified Graphics.Rendering.OpenGL as GL
import Asm
import Codec.Serialise
import Control.Concurrent.STM

-- Need to make left and right separately configurable
data GraphicsState = GraphicsState {
    _sdlWindow :: !GLFW.Window,
    _textureData :: Ptr Word8,
    _tex :: !GL.TextureObject,
    _glProg :: !GL.Program,
    _glAttrib :: !GL.AttribLocation,
    _windowWidth :: !Int,
    _windowHeight :: !Int
}

$(makeLenses ''GraphicsState)

data AcornAtom = AcornAtom {
    _clock :: IORef Int64,
    _stellaDebug :: IORef DebugState,

    _nextFrameTime :: IORef TimeSpec,

    _ram :: IOUArray Int Word8,
    _rom :: IOUArray Int Word8,
    _boolArray :: Segment Bool,
    _intArray :: Segment Int,
    _word8Array :: Segment Word8,
    _word16Array :: Segment Word16,
    _word64Array :: Segment Word64,

    _graphicsState :: GraphicsState,

    _key_buffer :: TQueue Word8
}

$(makeLenses ''AcornAtom)

withAtom :: AcornAtom -> MonadAcorn a -> IO a
withAtom state = flip runReaderT state . unM

data SerialisableAcornAtom = SerialisableAcornAtom {
    _s_clock :: Int64,
    _s_nextFrameTime :: TimeSpec,
    _s_ram :: [Word8],
    _s_rom :: [Word8],
    _s_boolArray :: [Bool],
    _s_intArray :: [Int],
    _s_word8Array :: [Word8],
    _s_word16Array :: [Word16],
    _s_word64Array :: [Word64]
} deriving (Generic)

frozen :: AcornAtom -> IO SerialisableAcornAtom
frozen AcornAtom {
         _clock=clock0,
         _nextFrameTime = nextFrameTime0,
         _rom=rom0,
         _ram=ram0,
         _boolArray=boolArray0,
         _intArray=intArray0,
         _word8Array=word8Array0,
         _word16Array=word16Array0,
         _word64Array=word64Array0 } = do
         clock1 <- readIORef clock0
         nextFrameTime1 <- readIORef nextFrameTime0
         ram1 <- freeze ram0
         rom1 <- freeze rom0
         boolArray1 <- freeze boolArray0
         intArray1 <- freeze intArray0
         word8Array1 <- freeze word8Array0
         word16Array1 <- freeze word16Array0
         word64Array1 <- freeze word64Array0
         return SerialisableAcornAtom {
            _s_clock=clock1,
            _s_nextFrameTime=nextFrameTime1,
            _s_ram=Data.Array.elems ram1,
            _s_rom=Data.Array.elems rom1,
            _s_boolArray=Data.Array.elems boolArray1,
            _s_intArray=Data.Array.elems intArray1,
            _s_word8Array=Data.Array.elems word8Array1,
            _s_word16Array=Data.Array.elems word16Array1,
            _s_word64Array=Data.Array.elems word64Array1 }

copyArray :: (MArray a e m, Ix i, Num i, Enum i) =>
                   a i e -> [e] -> m ()
copyArray mut = zipWithM_ (writeArray mut) [0..]

thawInto :: AcornAtom -> SerialisableAcornAtom -> IO ()
thawInto AcornAtom {
            _clock=clock1,
            _nextFrameTime=nextFrameTime1,
            _ram=ram1,
            _rom=rom1,
            _boolArray=boolArray1,
            _intArray=intArray1,
            _word8Array=word8Array1,
            _word16Array=word16Array1,
            _word64Array=word64Array1
            } SerialisableAcornAtom {
            _s_clock=clock0,
            _s_nextFrameTime=nextFrameTime0,
            _s_ram=ram0,
            _s_rom=rom0,
            _s_boolArray=boolArray0,
            _s_intArray=intArray0,
            _s_word8Array=word8Array0,
            _s_word16Array=word16Array0,
            _s_word64Array=word64Array0 } = do
                writeIORef clock1 clock0
                writeIORef nextFrameTime1 nextFrameTime0
                copyArray ram1 ram0
                copyArray rom1 rom0
                copyArray boolArray1 boolArray0
                copyArray intArray1 intArray0
                copyArray word8Array1 word8Array0
                copyArray word16Array1 word16Array0
                copyArray word64Array1 word64Array0

instance Serialise SerialisableAcornAtom
instance Serialise TimeSpec

newtype MonadAcorn a = M { unM :: ReaderT AcornAtom IO a }
      deriving (Functor, Applicative, Monad, MonadReader AcornAtom, MonadIO)

-- Do I still want Reg type class?
instance Reg Word8 MonadAcorn where
    {-# INLINE load #-}
    load r = do
        value <- view word8Array
        liftIO $ unsafeRead value (unTyped r)
    {-# INLINE store #-}
    store r v = do
        value <- view word8Array
        liftIO $ unsafeWrite value (unTyped r) v

instance Reg Int MonadAcorn where
    {-# INLINE load #-}
    load r = do
        value <- view intArray
        liftIO $ unsafeRead value (unTyped r)
    {-# INLINE store #-}
    store r v = do
        value <- view intArray
        liftIO $ unsafeWrite value (unTyped r) v

instance Reg Word16 MonadAcorn where
    {-# INLINE load #-}
    load r = do
        value <- view word16Array
        liftIO $ unsafeRead value (unTyped r)
    {-# INLINE store #-}
    store r v = do
        value <- view word16Array
        liftIO $ unsafeWrite value (unTyped r) v

instance Reg Word64 MonadAcorn where
    {-# INLINE load #-}
    load r = do
        value <- view word64Array
        liftIO $ unsafeRead value (unTyped r)
    {-# INLINE store #-}
    store r v = do
        value <- view word64Array
        liftIO $ unsafeWrite value (unTyped r) v

instance Reg Bool MonadAcorn where
    {-# INLINE load #-}
    load r = do
        value <- view boolArray
        liftIO $ unsafeRead value (unTyped r)
    {-# INLINE store #-}
    store r v = do
        value <- view boolArray
        liftIO $ unsafeWrite value (unTyped r) v

{-# INLINE useAtomDebug #-}
useAtomDebug :: Getting b DebugState b -> MonadAcorn b
useAtomDebug lens' = do
    atari <- ask
    stellaDebug' <- liftIO $ readIORef (atari ^. stellaDebug)
    return $! stellaDebug' ^. lens'

{-# INLINE putAtomDebug #-}
putAtomDebug :: ASetter DebugState DebugState a a -> a -> MonadAcorn ()
putAtomDebug lens' value = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. stellaDebug) (set lens' value)

{-# INLINE modifyAtomDebug #-}
modifyAtomDebug :: ASetter DebugState DebugState a b -> (a -> b) -> MonadAcorn ()
modifyAtomDebug lens' modifier = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. stellaDebug) (over lens' modifier)

{-# INLINE useClock #-}
useClock :: Getting b Int64 b -> MonadAcorn b
useClock lens' = do
    atari <- ask
    clock' <- liftIO $ readIORef (atari ^. clock)
    return $! clock' ^. lens'

{-# INLINE modifyClock #-}
modifyClock :: ASetter Int64 Int64 a b -> (a -> b) -> MonadAcorn ()
modifyClock lens' modifier = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. clock) (over lens' modifier)

-- 6502 registers

-- {-# INLINE getX #-}
getX :: MonadAcorn Word8
getX = load x

-- {-# INLINE putX #-}
putX :: Word8 -> MonadAcorn ()
putX r = x @= r 

getPC :: MonadAcorn Word16
putC :: Bool -> MonadAcorn ()
getC :: MonadAcorn Bool
putZ :: Bool -> MonadAcorn ()
getZ :: MonadAcorn Bool
putI :: Bool -> MonadAcorn ()
getI :: MonadAcorn Bool
putD :: Bool -> MonadAcorn ()
getD :: MonadAcorn Bool
putB :: Bool -> MonadAcorn ()
getB :: MonadAcorn Bool
putV :: Bool -> MonadAcorn ()
getV :: MonadAcorn Bool
putN :: Bool -> MonadAcorn ()
getN :: MonadAcorn Bool
getA :: MonadAcorn Word8
putA :: Word8 -> MonadAcorn ()
getS :: MonadAcorn Word8
putS :: Word8 -> MonadAcorn ()
getP :: MonadAcorn Word8
putP :: Word8 -> MonadAcorn ()
getY :: MonadAcorn Word8
putY :: Word8 -> MonadAcorn ()
putPC :: Word16 -> MonadAcorn ()
addPC :: Int -> MonadAcorn ()
--
{-# INLINE getPC #-}
getPC = load pc
{-# INLINE putC #-}
putC b = do { p' <- load p; p @= (p' & bitAt 0 .~ b) }
{-# INLINE getC #-}
getC = do { p' <- load p; return (p' ^. bitAt 0) }
{-# INLINE putZ #-}
putZ b = do { p' <- load p; p @= (p' & bitAt 1 .~ b) }
{-# INLINE getZ #-}
getZ = do { p' <- load p; return (p' ^. bitAt 1) }
{-# INLINE putI #-}
putI b = do { p' <- load p; p @= (p' & bitAt 2 .~ b) }
{-# INLINE getI #-}
getI = do { p' <- load p; return (p' ^. bitAt 2) }
{-# INLINE putD #-}
putD b = do { p' <- load p; p @= (p' & bitAt 3 .~ b) }
{-# INLINE getD #-}
getD = do { p' <- load p; return (p' ^. bitAt 3) }
{-# INLINE putB #-}
putB b = do { p' <- load p; p @= (p' & bitAt 4 .~ b) }
{-# INLINE getB #-}
getB = do { p' <- load p; return (p' ^. bitAt 4) }
{-# INLINE putV #-}
putV b = do { p' <- load p; p @= (p' & bitAt 6 .~ b) }
{-# INLINE getV #-}
getV = do { p' <- load p; return (p' ^. bitAt 6) }
{-# INLINE putN #-}
putN b = do { p' <- load p; p @= (p' & bitAt 7 .~ b) }
{-# INLINE getN #-}
getN = do { p' <- load p; return (p' ^. bitAt 7) }
{-# INLINE getA #-}
getA = load a
{-# INLINE putA #-}
putA r = a @= r
{-# INLINE getS #-}
getS = load s
{-# INLINE putS #-}
putS r = s @= r
{-# INLINE getP #-}
getP = load p
{-# INLINE putP #-}
putP r = p @= r 
{-# INLINE getY #-}
getY = load y
{-# INLINE putY #-}
putY r = y @= r
{-# INLINE putPC #-}
putPC r = pc @= r
{-# INLINE addPC #-}
addPC n = modify pc (+ fromIntegral n)
