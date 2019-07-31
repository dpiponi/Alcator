{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}

module Emulation(
                 brk,
                 displayChars,
                 dumpMemory,
                 dumpState,
                 loadBinary,
                 illegal,
                 incPC,
                 jmp,
                 jmpIndirect,
                 jsr,
                 nmi,
                 nop,
                 pha,
                 php,
                 pla,
                 plp,
                 irq,
                 pureWriteRom,
                 readAbs,
                 readAbsX,
                 readAbsY,
                 readImm,
                 readIndX,
                 readIndY,
                 readMemory,
                 readMemoryTick,
                 readZeroPage,
                 readZeroPageX,
                 readZeroPageY,
                 rti,
                 rts,
                 tick,
                 translateChar,
                 writeAbs,
                 writeAbsX,
                 writeAbsY,
                 writeIndX,
                 writeIndY,
                 bra,
                 set,
                 writeMemory,
                 initHardware,
                 initState,
                 withAbs,
                 withAbsX,
                 withAbsY,
                 withAcc,
                 withZeroPage,
                 withZeroPageX,
                 withIndX, -- * undocumented *
                 writeZeroPage,
                 writeZeroPageX,
                 writeZeroPageY
                ) where

import AcornAtom
import Asm hiding (a, s, x)
import CPU
import Control.Concurrent
import Control.Lens hiding (set, op, index, noneOf)
import Control.Monad.Reader
import Data.Array.IO hiding (index)
import Data.Bits hiding (bit)
import Data.Char
import Data.IORef
import Control.Concurrent.STM
import Data.Int
import Data.Word
import DebugState
import Disasm hiding (make16)
import Display
import Foreign.Storable
import Graphics.UI.GLFW hiding (getTime)
import Memory
import Metrics
import Numeric
import Prelude hiding (last, and)
import Stella
import System.Clock
import System.IO
import Text.Parsec
import Text.Printf
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BS (c2w, w2c)

readMemory :: Word16 -> MonadAcorn Word8
illegal :: Word8 -> MonadAcorn ()

-- {-# INLINE readMemory #-}
readMemory addr = pureReadMemory (memoryType addr) addr

-- {-# INLINE writeMemory #-}
writeMemory :: Word16 -> Word8 -> MonadAcorn ()
writeMemory addr' v = do
    let addr = addr'
    pureWriteMemory (memoryType addr) addr v

writeMemoryTick :: Word16 -> Word8 -> MonadAcorn ()
writeMemoryTick addr v = do
    tick 1
    writeMemory addr v

-- {-# INLINE tick #-}
tick :: Int -> MonadAcorn ()
tick n = do
    modifyClock id (+ fromIntegral n)
    c <- useClock id
    -- Misses if tick 2 used.
    when (c `mod` 16667 == 0) renderDisplay

-- Host instruction stuff..
writeWord32 :: Word16 -> Word32 -> MonadAcorn ()
writeWord32 i w = do
    writeMemory i (fromIntegral w)
    writeMemory (i+1) (fromIntegral $ w `shift` (-8))
    writeMemory (i+2) (fromIntegral $ w `shift` (-16))
    writeMemory (i+3) (fromIntegral $ w `shift` (-24))

hostFileName :: String -> MonadAcorn String
hostFileName = return 

saveBlock :: Word16 -> String -> MonadAcorn ()
saveBlock blockAddr hostName = do
    startData32 <- word32At (blockAddr+0xa)
    endData32 <- word32At (blockAddr+0xe)
    let start_addr = i16 startData32
    let end_addr = i16 endData32
    liftIO $ putStr $ printf " Save %04x:%04x to '%s'" start_addr end_addr hostName
    h <- liftIO $ openBinaryFile hostName WriteMode
    forM_ [start_addr..end_addr-1] $ \i -> do
        x <- readMemory i
        liftIO $ hPutChar h (BS.w2c x)
    liftIO $ hClose h

loadFile :: Word16 -> String -> MonadAcorn ()
loadFile blockAddr hostName = do
    loadAddr32 <- word32At (blockAddr+0x2)
--     execAddr32 <- word32At (blockAddr+0x6)
--     startData32 <- word32At (blockAddr+0xa)
--     addressType <- readMemory (blockAddr+0x6)

    -- If caller-specified execution address ends in zero
    -- use user-specified load address
    -- otherwise use load address in file
    let start_address = fromIntegral loadAddr32
    bytes <- loadBinary hostName
    let len = B.length bytes
    let end = start_address+fromIntegral len
    liftIO $ putStr $ printf " Load %04x:%04x from '%s'" start_address end hostName
    forM_ (Prelude.zip [start_address..end-1] (Prelude.map BS.w2c $ B.unpack bytes)) $ \(i, d) -> writeMemory i (BS.c2w d)

data Command = LOAD String Int -- <-- XXX needs to me Maybe Int
             | SAVE String Int Bool Int Int Int
             | RUN String -- XXX pass args

number :: Stream s m t => Int -> ParsecT s u m Char -> ParsecT s u m Int
number base baseDigit
    = do
        digits <- many1 baseDigit
        let n = Prelude.foldl (\x d -> base*x + digitToInt d) 0 digits
        seq n (return n)

filename_literal :: ParsecT String u Identity String
filename_literal = (char '"' >> (many1 (noneOf "\"") <* char '"'))
           <|> many1 (noneOf " ")

ignoreCase :: Stream s m Char => String -> ParsecT s u m String
ignoreCase [] = return []
ignoreCase (c : cs) | isUpper c = do
    m <- char (toLower c) <|> char c
    ms <- ignoreCase cs
    return (m : ms)
ignoreCase (c : cs) | isLower c = do
    m <- char '.' <|> char c <|> char (toUpper c)
    if m == '.'
        then
            return "."
        else do
            ms <- ignoreCase cs
            return (m : ms)
ignoreCase (c : cs) = do
    m <- char c
    ms <- ignoreCase cs
    return (m : ms)

removeStars :: String -> String
removeStars ('*' : cs) = removeStars cs
removeStars cs = cs

{-# INLINE i32 #-}
i32 :: Integral a => a -> Word32
i32 = fromIntegral

{-# INLINE make32 #-}
make32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
make32 b0 b1 b2 b3 = (i32 b3 `shift` 24)+(i32 b2 `shift` 32)+(i32 b1 `shift` 8)+i32 b0

{-# INLINABLE stringAt #-}
stringAt :: Word16 -> MonadAcorn String
stringAt addr = do
    let loop :: String -> Int -> MonadAcorn String
        loop cmd i = do
                    byte <- readMemory (addr+i16 i)
                    if byte == 0x0d || byte == 0x00
                        then return cmd
                        else loop (cmd ++ [BS.w2c byte]) (i+1)
    loop "" 0

{-# INLINE word16At #-}
word16At :: Word16 -> MonadAcorn Word16
word16At addr = make16 <$> readMemory addr <*> readMemory (addr+1)

{-# INLINE word32At #-}
word32At :: Word16 -> MonadAcorn Word32
word32At addr = make32 <$>
      readMemory addr     <*>
      readMemory (addr+1) <*>
      readMemory (addr+2) <*>
      readMemory (addr+3)

-- {-# INLINE putWord32 #-}
-- putWord32 :: Word16 -> Word32 -> MonadAcorn ()
-- putWord32 addr w = do
--     writeMemory addr (i8 w)
--     writeMemory (addr+1) (i8 (w `shift` (-8)))
--     writeMemory (addr+2) (i8 (w `shift` (-16)))
--     writeMemory (addr+3) (i8 (w `shift` (-24)))

{-# INLINE writeWord16 #-}
writeWord16 :: Word16 -> Word16 -> MonadAcorn ()
writeWord16 i w = do
    writeMemory i (fromIntegral w)
    writeMemory (i+1) (fromIntegral $ w `shift` (-8))

-- XXX Ignore case of commands
parseCommand :: ParsecT String u Identity Command
parseCommand = (LOAD <$> (ignoreCase "Load" >> spaces >> filename_literal)
                     <*> option 0 (spaces >> number 16 hexDigit))
               <|>
               (SAVE <$> (ignoreCase "Save" >> spaces >> (filename_literal <* spaces))
                     <*> (number 16 hexDigit <* spaces)
                     <*> option False (char '+' >> spaces >> return True)
                     <*> (number 16 hexDigit <* spaces)
                     <*> option 0 (number 16 hexDigit <* spaces)
                     <*> option 0 (number 16 hexDigit <* spaces))
               <|>
               (RUN <$> (ignoreCase "Run" >> spaces >> (filename_literal <* spaces)))

{-# INLINABLE osfile #-}
osfile :: MonadAcorn ()
osfile = do
    arg_a <- getA
    arg_x <- getX
    arg_y <- getY

    liftIO $ putStrLn $ printf "OSFILE A=%02x X=%02x Y=%02x" arg_a arg_x arg_y

    let blockAddr = make16 arg_x arg_y
    stringAddr <- word16At blockAddr
    rawFilename <- stringAt stringAddr
    hostName <- hostFileName rawFilename
    
    -- Note that the 'end' field points to the last byte,
    -- not the first byte after the end.
    case arg_a of
        0x00 -> saveBlock blockAddr hostName
        0xff -> loadFile blockAddr hostName

        _ -> error $ "Unknown OSFILE call " ++ show arg_a ++ "," ++ show arg_x ++ "," ++ show arg_y

loadBinary :: String -> MonadAcorn B.ByteString
loadBinary filename = liftIO $ do
    h <- openBinaryFile filename ReadMode
    B.hGetContents h <* hClose h

execStarCommand :: Command -> MonadAcorn ()
execStarCommand (LOAD filename _loadAddress) = do

    bytes <- loadBinary filename
    let bytes' = B.unpack $ B.take 22 bytes
    let addr = i16 (bytes'!!16) + 256*i16 (bytes'!!17)
    liftIO $ putStrLn $ "Loading at " ++ showHex addr ""
    forM_ (Prelude.zip [addr..] (Prelude.drop 22 $ Prelude.map BS.w2c $ B.unpack bytes)) $ \(i, d) ->
        writeMemory i (BS.c2w d)
    liftIO $ print "Done"
    p0 <- getPC
    putPC $ p0+2

execStarCommand (RUN filename) = do
    bytes <- loadBinary filename
    let bytes' = B.unpack $ B.take 22 bytes
    let addr = i16 (bytes'!!16) + 256*i16 (bytes'!!17)
    let exec_addr = i16 (bytes'!!18) + 256*i16 (bytes'!!19)
    liftIO $ do
      putStrLn $ "Loading at " ++ showHex addr ""
      putStrLn $ "Running from " ++ showHex exec_addr ""
    forM_ (Prelude.zip [addr..] (Prelude.drop 22 $ Prelude.map BS.w2c $ B.unpack bytes)) $ \(i, d) ->
        writeMemory i (BS.c2w d)
    liftIO $ print "Done"
    putPC exec_addr

execStarCommand (SAVE filename startAddress relative endAddress execAddress reloadAddress) = do
    putA 0
    -- Control block at &02EE
    putX 0xee
    putY 0x02
    let addrFilename = 0x200 :: Word16
    -- Write filename
    forM_ (Prelude.zip [addrFilename..] filename) $
            \(i, d) -> writeMemory (fromIntegral i) (BS.c2w d)
    -- Terminate filename with zero
    writeMemory (fromIntegral addrFilename+fromIntegral (Prelude.length filename)) 0
    -- Write address of filename
    writeWord16 0x2ee addrFilename
    writeWord32 (0x2ee+2) (fromIntegral $ if reloadAddress == 0 then startAddress else reloadAddress)
    writeWord32 (0x2ee+6) (fromIntegral execAddress)
    writeWord32 (0x2ee+0xa) (fromIntegral startAddress)
    writeWord32 (0x2ee+0xe) (fromIntegral $ if relative then startAddress+endAddress else endAddress)
    osfile
    p0 <- getPC
    putPC $ p0+2

{-# INLINABLE oscli #-}
oscli :: MonadAcorn ()
oscli = do
    let addr = 0x100
    cmd <- stringAt addr
    liftIO $ putStrLn $ printf "OSCLI: %s" cmd
    let cmd' = removeStars cmd
    let cmd'' = parse parseCommand "" cmd'
    case cmd'' of
        Right cmd''' -> execStarCommand cmd'''
        Left _ -> do
            liftIO $ putStrLn $  "Unknown * command:" ++ cmd
            p0 <- getPC
            putPC $ p0+2
    return ()

osrdch :: MonadAcorn ()
osrdch = do
    key_buffer' <- view key_buffer
    mKey <- liftIO $ atomically $ tryReadTQueue key_buffer'
    case mKey of
        Nothing -> do
            putC True
        Just key -> do
            putA key
            putC False
    p0 <- getPC
    putPC $ p0+2

-- {- INLINE illegal -}
illegal i =
    if i == 0x02
      then do
        pp <- getPC
        -- Retroactively fix PC
        putPC $ pp-1
        p0 <- getPC
        op <- readMemory (p0+1)
        case op of
            0x04 -> oscli
            0x05 -> osrdch
            _ -> do 
              putPC $ p0+2
              liftIO $ putStrLn $ "Host call with op 0x" ++ showHex op ""
      else do
        dumpState
        error $ "Illegal opcode 0x" ++ showHex i ""

-- debugStr :: Int -> String -> MonadAcorn ()
-- debugStrLn :: Int -> String -> MonadAcorn ()

-- {-# INLINE incPC #-}
incPC :: MonadAcorn ()
incPC = addPC 1

-- {-# INLINABLE read16 #-}
read16 :: Word16 -> MonadAcorn Word16
read16 addr = do
    lo0 <- readMemory addr
    hi0 <- readMemory (addr+1)
    return $ make16 lo0 hi0

readMemoryTick :: Word16 -> MonadAcorn Word8
readMemoryTick addr = tick 1 >> readMemory addr

readZpTick :: Word8 -> MonadAcorn Word8
readZpTick addr = tick 1 >> readMemory (i16 addr)

-- {-# INLINABLE read16tick #-}
read16tick :: Word16 -> MonadAcorn Word16
read16tick addr = make16 <$>
    readMemoryTick addr <*>
    readMemoryTick (addr+1)

-- {-# INLINABLE read16zpTick #-}
read16zpTick :: Word8 -> MonadAcorn Word16
read16zpTick addr = make16 <$>
    readZpTick addr <*>
    readZpTick (addr+1) -- wraps

-- http://www.emulator101.com/6502-addressing-modes.html

-- Note, a 6502 performs a read or write *every* clock cycle
-- regardless of what instruction is being executed.

-- 6 clock cycles...
-- {-# INLINABLE writeIndX #-}
writeIndX :: Word8 -> MonadAcorn ()
writeIndX src = do
    index <- getX
    addr <- fetchByteTick

    discard $ readMemoryTick (i16 addr)

    addrX <- read16zpTick (addr+index)

    writeMemoryTick addrX src
    incPC

-- 3 clock cycles
-- {-# INLINABLE writeZeroPage #-}
writeZeroPage :: Word8 -> MonadAcorn ()
writeZeroPage src = do
    addr <- fetchByteTick

    writeMemoryTick (i16 addr) src
    incPC

-- 4 clock cycles
-- {-# INLINABLE writeAbs #-}
writeAbs :: Word8 -> MonadAcorn()
writeAbs src = do
    addr <- getPC >>= read16tick

    writeMemoryTick addr src
    addPC 2

-- 6 clock cycles
-- {-# INLINABLE writeIndY #-}
writeIndY :: Word8 -> MonadAcorn ()
writeIndY src = do
    index <- getY
    addr' <- fetchByteTick

    addr <- read16zpTick addr'

    let (halfAddrY, addrY) = halfSum addr index

    discard $ readMemoryTick halfAddrY

    writeMemoryTick addrY src
    incPC

fetchByteTick :: MonadAcorn Word8
fetchByteTick = getPC >>= readMemoryTick

-- 4 clock cycles
-- {-# INLINABLE writeZeroPageX #-}
writeZeroPageX :: Word8 -> MonadAcorn ()
writeZeroPageX src = do
    index <- getX
    addr <- fetchByteTick

    discard $ readZpTick addr

    writeMemoryTick (i16 $ addr+index) src -- writezp
    incPC

-- 4 clock cycles
-- {-# INLINABLE writeZeroPageY #-}
writeZeroPageY :: Word8 -> MonadAcorn ()
writeZeroPageY src = do
    index <- getY
    addr <- fetchByteTick

    discard $ readZpTick addr

    writeMemoryTick (i16 $ addr+index) src
    incPC

-- 5 clock cycles
-- {-# INLINABLE writeAbsY #-}
writeAbsY :: Word8 -> MonadAcorn ()
writeAbsY src = do
    index <- getY
    addr <- getPC >>= read16tick

    let (halfAddrY, addrY) = halfSum addr index
    discard $ readMemoryTick halfAddrY

    writeMemoryTick addrY src
    addPC 2

-- 5 clock cycles
-- {-# INLINABLE writeAbsX #-}
writeAbsX :: Word8 -> MonadAcorn ()
writeAbsX src = do
    index <- getX
    addr <- getPC >>= read16tick

    let (halfAddrX, addrX) = halfSum addr index
    discard $ readMemoryTick halfAddrX

    writeMemoryTick addrX src
    addPC 2

-- 6 clock cycles
-- {-# INLINABLE readIndX #-}
readIndX :: MonadAcorn Word8
readIndX = do
    index <- getX
    addr0 <- fetchByteTick
    discard $ readZpTick addr0
    incPC
    read16zpTick (addr0+index) >>= readMemoryTick

-- * undocumented *
-- {-# INLINABLE withIndX #-}
withIndX :: (Word8 -> MonadAcorn Word8) -> MonadAcorn ()
withIndX op = do
    index <- getX
    addr0 <- fetchByteTick
    discard $ readZpTick addr0
    incPC
    addrX <- read16zpTick (addr0+index)
    src <- readMemoryTick addrX
    dst <- op src
    writeMemoryTick addrX dst

-- 3 clock cycles
-- {-# INLINABLE readZeroPage #-}
readZeroPage :: MonadAcorn Word8
readZeroPage = do
    addr <- fetchByteTick
    incPC
    readZpTick addr

-- 2 clock cycles
-- {-# INLINABLE readImm #-}
readImm :: MonadAcorn Word8
readImm = fetchByteTick <* incPC

-- XXX consider applicable ops like *>
-- 4 clock cycles
-- {-# INLINABLE readAbs #-}
readAbs :: MonadAcorn Word8
readAbs = getPC <* addPC 2 >>= read16tick >>= readMemoryTick

-- 5-6 clock cycles
-- {-# INLINABLE readIndY #-}
readIndY :: MonadAcorn Word8
readIndY = do
    addr <- fetchByteTick >>= read16zpTick

    index <- getY
    let (halfAddrY, addrY) = halfSum addr index

    when (halfAddrY /= addrY) $ discard $ readMemoryTick halfAddrY

    incPC
    readMemoryTick addrY

-- 4 clock cycles
-- {-# INLINABLE readZeroPageX #-}
readZeroPageX :: MonadAcorn Word8
readZeroPageX = do
    index <- getX
    addr <- fetchByteTick

    discard $ readZpTick addr -- wraps

    incPC
    readZpTick (addr+index) -- wraps

-- 4 clock cycles
-- {-# INLINABLE readZeroPageY #-}
readZeroPageY :: MonadAcorn Word8
readZeroPageY = do
    index <- getY
    addr <- fetchByteTick

    discard $ readMemoryTick (i16 addr)

    incPC
    readMemoryTick (i16 $ addr+index)

-- 4-5 clock cycles
-- {-# INLINABLE readAbsX #-}
readAbsX :: MonadAcorn Word8
readAbsX = do
    index <- getX
    addr <- getPC >>= read16tick
    addPC 2

    let (halfAddrX, addrX) = halfSum addr index
    when (halfAddrX /= addrX) $ discard $ readMemoryTick halfAddrX

    readMemoryTick addrX

-- 4-5 clock cycles
-- {-# INLINABLE readAbsY #-}
readAbsY :: MonadAcorn Word8
readAbsY = do
    index <- getY
    addr <- getPC >>= read16tick
    addPC 2 -- XXX suspicious

    let (halfAddrY, addrY) = halfSum addr index
    when ( halfAddrY /= addrY) $ discard $ readMemoryTick halfAddrY

    readMemoryTick addrY

-- 2-4 clock cycles
-- {-# INLINABLE bra #-}
bra :: MonadAcorn Bool -> Bool -> MonadAcorn ()
bra getFlag value = do
    offset <- fetchByteTick
    f <- getFlag
    incPC

    when (value == f) $ do
        spinPC

        oldP <- getPC
        let (halfAddr, addr) = halfSignedSum oldP offset
        when (halfAddr /= addr) $ discard $ readMemoryTick halfAddr
        putPC addr

-- 2 clock cycles
-- {-# INLINABLE set #-}
set :: (Bool -> MonadAcorn ()) -> Bool -> MonadAcorn ()
set putFlag value = spinPC >> putFlag value

-- 2 clock cycles
-- {-# INLINABLE nop #-}
nop :: MonadAcorn ()
nop = spinPC

{-
-- 3 clock cycles. Undocumented.
-- {-# INLINABLE nop #-}
dop :: MonadAcorn ()
nop = do
    tick 1
    discard $ getPC >>= readMemory
-}

-- 3 clock cycles
-- {-# INLINABLE jmp #-}
jmp :: MonadAcorn ()
jmp = getPC >>= read16tick >>= putPC

-- 5 clock cycles
-- NB address wraps around in page XXX
-- Not correct here.
-- Looks like the torture test might not catch this.
-- Aha! That's why ALIGN is used before addresses!
-- {-# INLINABLE jmpIndirect #-}
jmpIndirect :: MonadAcorn ()
jmpIndirect = getPC >>= read16tick >>= read16tick >>= putPC

-- {-# INLINABLE uselessly #-}
uselessly :: m () -> m ()
uselessly = id

-- 5 clock cycles
-- {-# INLINABLE withZeroPage #-}
withZeroPage :: (Word8 -> MonadAcorn Word8) -> MonadAcorn ()
withZeroPage op = do
    addr <- fetchByteTick
    src <- readMemoryTick (i16 addr)

    uselessly $ writeMemoryTick (i16 addr) src

    op src >>= writeMemoryTick (i16 addr)
    incPC

-- 2 clock cycles
-- {-# INLINABLE withAcc #-}
withAcc :: (Word8 -> MonadAcorn Word8) -> MonadAcorn ()
withAcc op = spinPC >> getA >>= op >>= putA

-- 6 clock cycles
-- {-# INLINE withAbs #-}
withAbs :: (Word8 -> MonadAcorn Word8) -> MonadAcorn ()
withAbs op = do
    addr <- getPC >>= read16tick
    
    src <- readMemoryTick addr

    uselessly $ writeMemoryTick addr src

    dst <- op src
    addPC 2 -- XXX suspicious
    writeMemoryTick addr dst

-- 6 clock cycles
withZeroPageX :: (Word8 -> MonadAcorn Word8) -> MonadAcorn ()
withZeroPageX op = do
    index <- getX
    addr <- fetchByteTick
    let addrX = addr+index

    discard $ readMemoryTick (i16 addr)

    src <- readMemoryTick (i16 addrX)

    uselessly $ writeMemoryTick (i16 addrX) src

    dst <- op src
    writeMemoryTick (i16 addrX) dst
    incPC
 
-- 7 clock cycles
-- {-# INLINE withAbsX #-}
withAbsX :: (Word8 -> MonadAcorn Word8) -> MonadAcorn ()
withAbsX op = do
    p0 <- getPC
    index <- getX
    addr <- read16tick p0
    let (halfAddrX, addrX) = halfSum addr index
    discard $ readMemoryTick halfAddrX
    src <- readMemoryTick addrX
    uselessly $ writeMemoryTick addrX src
    addPC 2
    dst <- op src
    writeMemoryTick addrX dst

-- 7 clock cycles
-- {-# INLINE withAbsY #-}
-- * undocumented *
withAbsY :: (Word8 -> MonadAcorn Word8) -> MonadAcorn ()
withAbsY op = do
    p0 <- getPC
    index <- getY
    addr <- read16tick p0

    let (halfAddrY, addrY) = halfSum addr index

    discard $ readMemoryTick halfAddrY

    src <- readMemoryTick addrY

    uselessly $ writeMemoryTick addrY src

    addPC 2
    dst <- op src
    writeMemoryTick addrY dst

-- 7 clock cycles
-- {-# INLINABLE brk #-}
brk :: MonadAcorn ()
brk = do
--     p0 <- getPC
--     discard $ readMemoryTick p0
    discard $ fetchByteTick
    incPC

    p1 <- getPC
    pushTick $ hi p1
    incPC

    incPC
    pushTick $ lo p1

    putB True
    incPC
    getP >>= pushTick . (.|. 0x20) -- always on bit
    putI True

    read16tick 0xfffe >>= putPC -- irq/brk XXX

-- Am I using wrong address for IRQ. Should it be 0xfffe for IRQ, 0xfffa for NMI?
-- XXX not supported correctly for now
-- {-# INLINABLE irq #-}
irq :: MonadAcorn ()
irq = do
    fi <- getI
    unless fi $ nmi False

-- {-# INLINABLE pushTick #-}
pushTick :: Word8 -> MonadAcorn ()
pushTick v = do
    sp <- getS
    writeMemoryTick (0x100+i16 sp) v
    putS (sp-1)

-- {-# INLINABLE pullTick #-}
pullTick :: MonadAcorn Word8
pullTick = do
    sp <- getS
    let sp' = sp+1
    putS sp'
    readMemoryTick (0x100+i16 sp') -- XXX make wrap

spinPC :: MonadAcorn ()
spinPC = discard fetchByteTick

-- 3 clock cycles
-- {-# INLINABLE pha #-}
pha :: MonadAcorn ()
pha = spinPC >> getA >>= pushTick

-- 3 clock cycles
-- {-# INLINABLE php #-}
php :: MonadAcorn ()
php = spinPC >> getP >>= pushTick . (.|. 0x30)

spinS :: MonadAcorn ()
spinS = do
    s <- getS
    discard $ readMemoryTick (0x100+i16 s)

pulP :: MonadAcorn ()
pulP = spinS >> pullTick >>= putP

-- 4 clock cycles
-- {-# INLINABLE plp #-}
plp :: MonadAcorn ()
plp = spinPC >> pulP

-- 4 clock cycles
-- {-# INLINABLE pla #-}
pla :: MonadAcorn ()
pla = spinPC >> spinS >> pullTick >>= setNZ >>= putA

-- {-# INLINABLE nmi #-}
nmi :: Bool -> MonadAcorn ()
nmi sw = do
    p0 <- getPC
    pushTick $ hi p0
    pushTick $ lo p0
    putB sw
    getP >>= pushTick . (.|. 0x20) -- always on bit
    putI True
    read16 0xfffe >>= putPC -- irq/brk XXX
    tick 4

-- 6 clock cycles
-- {-# INLINABLE rti #-}
rti :: MonadAcorn ()
rti = spinPC >> pulP >> make16 <$> pullTick <*> pullTick >>= putPC

-- 6 clock cycles
-- {-# INLINABLE jsr #-}
jsr :: MonadAcorn ()
jsr = do
    pcl <- fetchByteTick
    incPC

    spinS

    p2 <- getPC

    pushTick $ hi p2
    pushTick $ lo p2

    pch <- readMemoryTick p2

    putPC $ make16 pcl pch

-- 6 clock cycles
-- {-# INLINABLE rts #-}
rts :: MonadAcorn ()
rts = do
    spinPC
    spinS

    p0 <- make16 <$> pullTick <*> pullTick
    
    discard $ readMemoryTick p0
    putPC (p0+1)

initState :: IOUArray Int Word8 ->
             IOUArray Int Word8 ->
             Word16 ->
             GraphicsState ->
             TQueue Word8 ->
             IO AcornAtom
initState ram' rom' initialPC graphicsState' key_buffer' = do
          stellaDebug' <- newIORef DebugState.start
          t <- liftIO $ getTime Realtime
          let nt = addTime t (1000000000 `div` 60)
          nextFrameTime' <- newIORef nt
          clock' <- newIORef 0
          boolArray' <- newArray (0, maxBool) False
          intArray' <- newArray (0, maxInt) 0      -- Overkill
          word64Array' <- newArray (0, maxWord64) 0
          word16Array' <- newArray (0, maxWord16) 0      -- Overkill
          word8Array' <- newArray (0, maxWord8) 0
          liftIO $ st word16Array' pc initialPC
          return $ AcornAtom {
              _nextFrameTime = nextFrameTime',
              _rom = rom',
              _ram = ram',
              _stellaDebug = stellaDebug',
              _clock = clock',
              _boolArray = boolArray',
              _intArray = intArray',
              _word64Array = word64Array',
              _word16Array = word16Array',
              _word8Array = word8Array',
              _graphicsState = graphicsState',
              _key_buffer = key_buffer'
          }

-- {-# INLINE pureReadRom #-}
pureReadRom :: Word16 -> MonadAcorn Word8
pureReadRom addr = do
    atom <- ask
    let m = atom ^. rom
    liftIO $ readArray m (iz addr - 0xa000) -- Rom starts ac 0xc000

-- {-# INLINE pureWriteRom #-}
-- | pureWriteRom sees address in full 6507 range 0x0000-0x1fff
-- You can write to Super Chip "ROM"
pureWriteRom :: Word16 -> Word8 -> MonadAcorn ()
pureWriteRom addr v = do
    atom <- ask
    let m = atom ^. rom
    liftIO $ writeArray m (iz addr - 0xa000) v

-- {-# INLINE pureReadMemory #-}
pureReadMemory :: MemoryType -> Word16 -> MonadAcorn Word8
pureReadMemory PPIA addr =
    case addr of
        0xb000 -> load ppia0
        0xb001 -> do
            row <- load keyboard_row
            load (keyboard_matrix + fromIntegral row)

            -- Port C - #B002
            -- Output bits:      Function:
            --      0          Tape output
            --      1          Enable 2.4 kHz to cassette output
            --      2          Loudspeaker
            --      3          Not used
            --
            -- Input bits:       Function:
            --      4          2.4 kHz input
            --      5          Cassette input
            --      6          REPT key (low when pressed)
            --      7          60 Hz sync signal (low during flyback)

        0xb002 -> do
            c <- useClock id
            -- flyback
            -- 20000/frame PAL   16667/US
            let s = c `mod` 16667 -- clock cycles per 60 Hz
            -- The 0x40 is the REPT key
            -- PAL vertical blanking 1600 us
            -- NTSC vertical blanking 1333us
            ppia2' <- load ppia2
            let someBits = ppia2' `xor` 0x40
            return $ if s < 1333
              then someBits
              else someBits .|. 0x80
        _ -> return 0
pureReadMemory VIA _ = return 0
pureReadMemory ROM  addr = pureReadRom addr
pureReadMemory RAM  addr = do
    atom <- ask
    let m = atom ^. ram
    liftIO $ readArray m (iz addr)

displayChars :: String
displayChars = "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]↑← !\"*$%&'()*+,-./0123456789:;<=>?"

translateChar :: Int -> Char
translateChar c | c < 64 = displayChars !! c
translateChar c | c < 128 = '.'
translateChar c | c < 192 = displayChars !! (c - 128)
translateChar _ = '.'


-- {-# INLINE pureWriteMemory #-}
pureWriteMemory :: MemoryType -> Word16 -> Word8 -> MonadAcorn ()
pureWriteMemory PPIA addr v =
    case addr of
        0xb000 -> do
            store ppia0 v
            store keyboard_row (v .&. 0xf)
        _ -> return ()
pureWriteMemory VIA _ _ = return ()
pureWriteMemory ROM  addr v = pureWriteRom addr v
pureWriteMemory RAM  addr v = do
    atom <- ask
    let m = atom ^. ram
    let realAddress = iz addr
    liftIO $ writeArray m realAddress v


-- {-# INLINABLE dumpMemory #-}
dumpMemory :: MonadAcorn ()
dumpMemory = do
    regPC <- getPC
    b0 <- readMemory regPC
    b1 <- readMemory (regPC+1)
    b2 <- readMemory (regPC+2)
    liftIO $ do
      putStr $ "PC = " ++ showHex regPC ""
      putStr   "(PC) = "
      putStr $ showHex b0 "" ++ " "
      putStr $ showHex b1 "" ++ " "
      putStrLn $ showHex b2 ""
    let (_, mne, _) = disasm regPC [b0, b1, b2]
    liftIO $ putStrLn mne

-- {-# INLINABLE dumpRegisters #-}
dumpRegisters :: MonadAcorn ()
dumpRegisters = do
    regPC <- getPC
    liftIO $ putStr $ " pc = " ++ showHex regPC ""
    regP <- getP
    liftIO $ do
        putStr $ " flags = " ++ showHex regP ""
        putStr $ "(N=" ++ showHex ((regP `shift` (-7)) .&. 1) ""
        putStr $ ",V=" ++ showHex ((regP `shift` (-6)) .&. 1) ""
        putStr $ ",B=" ++ showHex (regP `shift` (-4) .&. 1) ""
        putStr $ ",D=" ++ showHex (regP `shift` (-3) .&. 1) ""
        putStr $ ",I=" ++ showHex (regP `shift` (-2) .&. 1) ""
        putStr $ ",Z=" ++ showHex (regP `shift` (-1) .&. 1) ""
        putStr $ ",C=" ++ showHex (regP .&. 1) ""
    regA <- getA 
    liftIO $ putStr $ ") A = " ++ showHex regA ""
    regX <- getX
    liftIO $ putStr $ " X = " ++ showHex regX ""
    regY <- getY
    liftIO $ putStrLn $ " Y = " ++ showHex regY ""
    regS <- getS
    liftIO $ putStrLn $ " N = " ++ showHex regS ""

-- {-# INLINABLE dumpState #-}
dumpState :: MonadAcorn ()
dumpState = do
    dumpMemory
    dumpRegisters

renderDisplay :: MonadAcorn ()
renderDisplay = do
    GraphicsState { _sdlWindow=window,
                    _glProg=prog,
                    _glAttrib=attrib,
                    _tex=tex',
                    _textureData=ptr } <- view graphicsState
    --
    -- Copy 6K of video RAM
    forM_ [0..6143::Int] $ \i -> do
        byte <- readMemory (0x8000 + i16 i)
        liftIO $ pokeElemOff ptr (fromIntegral i) byte

    liftIO $ updateTexture tex' ptr
    (w', h') <- liftIO $ getFramebufferSize window
    mode <- load ppia0
    liftIO $ draw (mode .&. 0xf0) w' h' prog attrib

    waitUntilNextFrameDue
    liftIO $ swapBuffers window
    return ()

-- Note this fixes the *frame* rate.
-- It has nothing to do with the simulated clock
waitUntilNextFrameDue :: MonadAcorn ()
waitUntilNextFrameDue = do
  nextFrameTimeRef <- view nextFrameTime

  liftIO $ do
    nextFrameTime' <- readIORef nextFrameTimeRef
    t <- getTime Realtime
    let frameTimeAfter = addTime nextFrameTime' (1000000000 `div` fps)
    writeIORef nextFrameTimeRef frameTimeAfter

    let TimeSpec {sec=secondsToGo,
                  nsec=nanosecondsToGo} = diffTimeSpec nextFrameTime' t
    let timeToGo = fromIntegral secondsToGo +
                   fromIntegral nanosecondsToGo / 1e9 :: Double
    when (nextFrameTime' `gtTime` t) $
      threadDelay $ floor (1000000.0 * timeToGo)

initHardware :: MonadAcorn ()
initHardware = do
    -- Clear keyboard
    forM_ [0..9 :: Int] $ \i ->
        store (keyboard_matrix + fromIntegral i) 0xff
    pclo <- readMemory 0xfffc
    pchi <- readMemory 0xfffd
    let initialPC = fromIntegral pclo+(fromIntegral pchi `shift` 8)
    liftIO $ putStrLn $ "Starting at address: 0x" ++ showHex initialPC ""
    store pc initialPC
