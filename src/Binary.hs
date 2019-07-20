{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Binary where

import Prelude hiding (words)
import System.IO
-- import Control.Monad
import Data.Array.IO
import Data.Char
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Vector as V

import Data.Word
import qualified Data.ByteString.Internal as BS (c2w)

-- Read binary data from file into an IOUArray at
-- provided 16-bit offset
readBinary :: IOUArray Int Word8 -> FilePath -> Word16 -> IO ()
readBinary arr filename origin =
    withBinaryFile filename ReadMode $ \handle -> do
      let offset = fromIntegral origin
      contents <- hGetContents handle
      let words = V.map BS.c2w $ V.fromList contents
      V.forM_ (V.indexed words) $ \(i, c) ->
          writeArray arr (i+offset) c

-- Read text file with font data like...
--   XXX      X  
--  X   X    X X 
--      X   X   X
--   XX X   X   X
--  X X X   XXXXX
--  X X X   X   X
--   XXX    X   X ...
readFont :: FilePath -> IO (Ptr Word8)
readFont filename = 
    withFile filename ReadMode $ \handle -> do
      contents <- hGetContents handle
      let tex_data = V.map @Int (\x -> if x == 88 then 0xff else 0x00) $
                     V.filter (\x -> x == 88 || x == 32) $
                     V.map (fromIntegral . ord) $
                     V.fromList contents
      fontData <- mallocBytes (256*96)
      V.forM_ (V.indexed tex_data) $ uncurry (pokeElemOff fontData)
      return fontData
