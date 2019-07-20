{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Asm where

import Data.Word
import Data.Array.IO
import Data.Array.Base
import Codec.Serialise
import GHC.Generics

newtype TypedIndex t = TO { unTyped :: Int } deriving (Ord, Ix, Eq, Num, Generic, Enum)
instance Serialise (TypedIndex a)

p, a, x, y, s, ppia0, keyboard_matrix, keyboard_row, keyboard_matrix_end, ppia2 :: TypedIndex Word8
p = 1
a = 2
x = 3
y = 4
s = 5
ppia0 = 14
keyboard_row = 15
keyboard_matrix = 16
keyboard_matrix_end = 26
ppia2 = 27

maxWord8 :: TypedIndex Word8
maxWord8 = 0x124 -- for backwards compatibility

kbd :: Int -> Int -> TypedIndex Bool
kbd i j = fromIntegral $ 8+i*6+j

maxBool :: TypedIndex Bool
maxBool = kbd 3 5

maxWord64 :: TypedIndex Word64
maxWord64 = 0

maxInt :: TypedIndex Int
maxInt = 9

pc, pcStep :: TypedIndex Word16
pc = 0
pcStep = 2 -- Do I need this? XXX

maxWord16 :: TypedIndex Word16
maxWord16 = 14

{-# INLINE ld #-}
ld :: MArray IOUArray a IO => IOUArray (TypedIndex a) a -> TypedIndex a -> IO a
ld arr idx = unsafeRead arr (unTyped idx)

{-# INLINE st #-}
st :: MArray IOUArray a IO => IOUArray (TypedIndex a) a -> TypedIndex a -> a -> IO ()
st arr idx = unsafeWrite arr (unTyped idx)

{-# INLINE md #-}
md :: MArray IOUArray a IO => IOUArray (TypedIndex a) a -> TypedIndex a -> (a -> a) -> IO ()
md arr idx f = do { value <- unsafeRead arr (unTyped idx); unsafeWrite arr (unTyped idx) (f value) }

type Segment a = IOUArray (TypedIndex a) a

class Monad m => Reg t m where
    load :: TypedIndex t -> m t
    store :: TypedIndex t -> t -> m ()
    modify :: TypedIndex t -> (t -> t) -> m ()
    modify r f = do { value <- load r; store r (f value) }

(@=) :: Reg t m => TypedIndex t -> t -> m ()
(@=) = store

(@->) :: Reg t m => TypedIndex t -> TypedIndex t -> m ()
(@->) src dst = load src >>= (dst @=)

infixr 2 @= 
infix 2 @->
