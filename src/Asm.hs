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

trigger1, trigger2, delayP0, delayP1, delayBall, oldBall, newBall, pendingHmove, debugColours :: TypedIndex Bool
trigger1 = 0
trigger2 = 1
delayP0 = 2
delayP1 = 3
delayBall = 4
oldBall = 5
newBall = 6
pendingHmove = 7
debugColours = 8

kbd :: Int -> Int -> TypedIndex Bool
kbd i j = fromIntegral $ 8+i*6+j

maxBool :: TypedIndex Bool
maxBool = kbd 3 5

pf :: TypedIndex Word64
pf = 0

maxWord64 :: TypedIndex Word64
maxWord64 = pf

subtimer, interval :: TypedIndex Int
debug :: TypedIndex Int
subtimer = 2
interval = 3
debug = 9

maxInt :: TypedIndex Int
maxInt = debug

pc, bankOffset, pcStep, pcResp0, pcResp1, pcResm0, pcResm1, pcResbl, pcColup0, pcColup1, pcColupf, pcColubk, pcPf0, pcPf1, pcPf2 :: TypedIndex Word16
pc = 0
bankOffset = 1
pcStep = 2
pcResp0 = 3
pcResp1 = 4
pcResm0 = 5
pcResm1 = 6
pcResbl = 7
pcColup0 = 8
pcColup1 = 9
pcColupf = 10
pcColubk = 11
pcPf0 = 12
pcPf1 = 13
pcPf2 = 14

maxWord16 :: TypedIndex Word16
maxWord16 = pcPf2

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
