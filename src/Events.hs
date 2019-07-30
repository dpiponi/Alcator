module Events where

import AcornAtom
import Asm
import qualified Data.Array as A
import Control.Lens hiding (op)
import Control.Monad.Reader
import Data.Bits.Lens
import Data.IORef
import qualified Data.Set as S
import qualified Data.Foldable as F
import Debugger
import Data.Word
import qualified Data.Set.Ordered as O
import Emulation
import Graphics.UI.GLFW
import Stella
import System.Exit

isPressed :: KeyState -> Bool
isPressed KeyState'Pressed = True
isPressed KeyState'Repeating = True -- I don't know!
isPressed KeyState'Released = False

atom_keyboard :: [(Key, ({- Rows -} [Int], {- Column -} Int))]
atom_keyboard = [
    (Key'LeftShift, ([0..9], 7)),
    (Key'RightShift, ([0..9], 7)),
    (Key'LeftControl, ([0..9], 6)),

    (Key'Escape, ([0], 5)),
    (Key'Q, ([0], 4)),
    (Key'G, ([0], 3)),
    (Key'Minus, ([0], 2)),
    (Key'3, ([0], 1)),

    (Key'Z, ([1], 5)),
    (Key'P, ([1], 4)),
    (Key'F, ([1], 3)),
    (Key'Comma, ([1], 2)),
    (Key'2, ([1], 1)),

    (Key'Y, ([2], 5)),
    (Key'O, ([2], 4)),
    (Key'E, ([2], 3)),
    (Key'Semicolon, ([2], 2)),
    (Key'1, ([2], 1)),
    (Key'Up, ([2], 0)),
    (Key'Down, ([2], 0)),

    (Key'X, ([3], 5)),
    (Key'N, ([3], 4)),
    (Key'D, ([3], 3)),
    (Key'Equal, ([3], 2)),
    (Key'0, ([3], 1)),
    (Key'Left, ([3], 0)),
    (Key'Right, ([3], 0)),

    (Key'W, ([4], 5)),
    (Key'M, ([4], 4)),
    (Key'C, ([4], 3)),
    (Key'9, ([4], 2)),
    (Key'Backspace, ([4], 1)),
    (Key'CapsLock, ([4], 0)),

    (Key'V, ([5], 5)),
    (Key'L, ([5], 4)),
    (Key'B, ([5], 3)),
    (Key'8, ([5], 2)),
    (Key'PageDown, ([5], 1)),
    (Key'PageUp, ([5], 0)),

    (Key'U, ([6], 5)),
    (Key'K, ([6], 4)),
    (Key'A, ([6], 3)),
    (Key'7, ([6], 2)),
    (Key'Enter, ([6], 1)),
    (Key'RightBracket, ([6], 0)),

    (Key'T, ([7], 5)),
    (Key'J, ([7], 4)),
    (Key'Apostrophe, ([7], 3)),
    (Key'6, ([7], 2)),
    (Key'Backslash, ([7], 0)),

    (Key'S, ([8], 5)),
    (Key'I, ([8], 4)),
    (Key'Slash, ([8], 3)),
    (Key'5, ([8], 2)),
    (Key'LeftBracket, ([8], 0)),

    (Key'R, ([9], 5)),
    (Key'H, ([9], 4)),
    (Key'Period, ([9], 3)),
    (Key'4, ([9], 2)),
    (Key'Space, ([9], 0))]


-- Row     Data (read)
--     7   6   5   4   3   2   1   0
-- 0           ESC Q   G   - = 3 #  
-- 1           Z   P   F   , < 2 "  
-- 2           Y   O   E   ; + 1 ! 
-- 3           X   N   D   : * 0   
-- 4           W   M   C   9 ) del lock
-- 5           V   L   B   8 ( copy up
-- 6           U   K   A   7 ' ret ]
-- 7           T   J   @   6 &     \
-- 8           S   I   / ? 5 %     [
-- 9           R   H   . > 4 $     sp
shiftBits :: Bool -> A.Array Int Word8 -> A.Array Int Word8
shiftBits x = traverse . bitAt 7 .~ x
controlBits x = traverse . bitAt 6 .~ x
keyBit :: Int -> Int -> Bool -> A.Array Int Word8 -> A.Array Int Word8
keyBit row col x = ix row . bitAt col .~ x

applyKey key (shift, rows) =
    let (newShift, rowsOp) = doKey key shift
    in (newShift, rowsOp rows)

doKey :: Key ->        Bool -> (Bool, A.Array Int Word8 -> A.Array Int Word8)
doKey Key'Escape       shift = (shift, keyBit 0 5 False)                   -- ESC
doKey Key'1            shift = (shift, keyBit 2 1 False)                   -- 1 or !
doKey Key'2            False = (False, keyBit 1 1 False)                   -- 2
doKey Key'2            True  = (True,  keyBit 7 3 False . shiftBits True)  -- @
doKey Key'3            shift = (shift, keyBit 0 1 False)                   -- 3 or #
doKey Key'4            shift = (shift, keyBit 9 2 False)                   -- 4 or $
doKey Key'5            shift = (shift, keyBit 8 2 False)                   -- 5 or %
doKey Key'6            False = (False, keyBit 7 2 False)                   -- 6
doKey Key'6            True  = (True , keyBit 5 0 False . shiftBits True)  -- ^
doKey Key'7            False = (False, keyBit 6 2 False)                   -- 7
doKey Key'7            True  = (True,  keyBit 7 2 False)                   -- &
doKey Key'8            False = (False, keyBit 5 2 False)                   -- 8
doKey Key'8            True  = (True,  keyBit 3 2 False)                   -- 8
doKey Key'9            False = (False, keyBit 4 2 False)                   -- 9
doKey Key'9            True  = (True,  keyBit 5 2 False)                   -- (
doKey Key'0            False = (False, keyBit 3 1 False)                   -- 9
doKey Key'0            True  = (True,  keyBit 4 2 False)                   -- (
doKey Key'Minus        False = (False, keyBit 0 2 False)                   -- -
doKey Key'Minus        True  = (True,  id)                                 -- N/A
doKey Key'Equal        False = (False, keyBit 0 2 False . shiftBits False) -- =
doKey Key'Equal        True  = (True,  keyBit 2 2 False)                   -- +
doKey Key'Backspace    shift = (shift, keyBit 4 1 False)                   -- DEL
doKey Key'Q            shift = (shift, keyBit 0 4 False)                   -- Q
doKey Key'W            shift = (shift, keyBit 4 5 False)                   -- W
doKey Key'E            shift = (shift, keyBit 2 3 False)                   -- E
doKey Key'R            shift = (shift, keyBit 9 5 False)                   -- R
doKey Key'T            shift = (shift, keyBit 7 5 False)                   -- T
doKey Key'Y            shift = (shift, keyBit 2 5 False)                   -- Y
doKey Key'U            shift = (shift, keyBit 6 5 False)                   -- U
doKey Key'I            shift = (shift, keyBit 8 4 False)                   -- I
doKey Key'O            shift = (shift, keyBit 2 4 False)                   -- O
doKey Key'P            shift = (shift, keyBit 1 4 False)                   -- P
doKey Key'LeftBracket  shift = (shift, keyBit 8 0 False)                   -- [
doKey Key'RightBracket shift = (shift, keyBit 6 0 False)                   -- ]
doKey Key'Backslash    shift = (shift, keyBit 7 0 False)                   -- backslash
doKey Key'CapsLock     shift = (shift, keyBit 4 0 False)                   -- LOCK
doKey Key'A            shift = (shift, keyBit 6 3 False)                   -- A
doKey Key'S            shift = (shift, keyBit 8 5 False)                   -- S
doKey Key'D            shift = (shift, keyBit 3 3 False)                   -- D
doKey Key'F            shift = (shift, keyBit 1 3 False)                   -- F
doKey Key'G            shift = (shift, keyBit 0 3 False)                   -- G
doKey Key'H            shift = (shift, keyBit 9 4 False)                   -- H
doKey Key'J            shift = (shift, keyBit 7 4 False)                   -- J
doKey Key'K            shift = (shift, keyBit 6 4 False)                   -- K
doKey Key'L            shift = (shift, keyBit 5 4 False)                   -- L
doKey Key'Semicolon    False = (False, keyBit 2 2 False)                   -- ;
doKey Key'Semicolon    True  = (True,  keyBit 3 2 False . shiftBits True)  -- :
doKey Key'Apostrophe   False = (False, keyBit 6 2 False . shiftBits False) -- '
doKey Key'Apostrophe   True  = (True,  keyBit 1 1 False . shiftBits False) -- '
doKey Key'Enter        shift = (shift, keyBit 6 1 False)                   -- RETURN
doKey Key'LeftShift    _     = (True,  shiftBits False)                    -- SHIFT
doKey Key'Z            shift = (shift, keyBit 1 5 False)                   -- Z
doKey Key'X            shift = (shift, keyBit 3 5 False)                   -- X
doKey Key'C            shift = (shift, keyBit 4 3 False)                   -- C
doKey Key'V            shift = (shift, keyBit 5 5 False)                   -- V
doKey Key'B            shift = (shift, keyBit 5 3 False)                   -- B
doKey Key'N            shift = (shift, keyBit 3 4 False)                   -- N
doKey Key'M            shift = (shift, keyBit 4 4 False)                   -- M
doKey Key'Comma        shift = (shift, keyBit 1 2 False)                   -- , or <
doKey Key'Period       shift = (shift, keyBit 9 3 False)                   -- . or >
doKey Key'Slash        shift = (shift, keyBit 8 3 False)                   -- / or ?
doKey Key'RightShift   _     = (True,  shiftBits False)                    -- SHIFT
doKey Key'LeftControl  shift = (shift, controlBits False)                  -- CTRL
doKey Key'Space        shift = (shift, keyBit 9 0 False)                   -- SPACE
doKey Key'Up           shift = (shift, keyBit 2 0 False . shiftBits True)  -- UP
doKey Key'Down         shift = (shift, keyBit 2 0 False . shiftBits False) -- DOWN
doKey Key'Right        shift = (shift, keyBit 3 0 False . shiftBits True)  -- RIGHT
doKey Key'Left         shift = (shift, keyBit 3 0 False . shiftBits False) -- LEFT
doKey Key'PageDown     shift = (shift, keyBit 5 1 False)                   -- COPY

doKey key              shift = (shift, id)

rowsFromKeys :: O.OSet Key -> MonadAcorn ()
rowsFromKeys keys = do
  let rows = A.array (0, 9) [(i, 0xff) | i <- [0..9]]
--   liftIO $ print (F.toList keys)
  let (_, rows') = foldr applyKey (False, rows) (reverse $ F.toList keys)
--   liftIO $ print rows'
  forM_ [0..9] $ \row -> do
    store (keyboard_matrix + TO row) (rows' A.! row)

isMod Key'LeftShift = True
isMod Key'RightShift = True
isMod Key'LeftControl = True
isMod _ = False

newUpdatePPIA :: IORef (O.OSet Key) -> Key -> Bool -> MonadAcorn Bool
newUpdatePPIA key_set key pressed = do
  if pressed
    then do -- in order
        when (not (isMod key)) $ liftIO $ modifyIORef key_set (O.filter isMod)
        liftIO $ modifyIORef key_set (O.|> key)
    else liftIO $ modifyIORef key_set (O.delete key)
  keys <- liftIO $ readIORef key_set
--   liftIO $ print keys
  rowsFromKeys keys

  return False

updatePPIA :: Key -> Bool -> MonadAcorn Bool
updatePPIA key pressed =
    case lookup key atom_keyboard of
        Nothing -> return False
        Just (rows, column) -> do
            forM_ rows $ \row ->
                modify (keyboard_matrix + TO row) $ bitAt column .~ not pressed
            return True

updateRept :: Bool -> MonadAcorn ()
updateRept pressed = modify ppia2 $ bitAt 6 .~ pressed

-- Handle keys to Alcator rather than keys for emulated machine
handleKey :: KeyState -> Key -> MonadAcorn ()
handleKey motion key = do
    let pressed = isPressed motion
    case key of
      Key'GraveAccent -> liftIO exitSuccess
      Key'LeftAlt     -> when pressed $ do
                              Emulation.dumpState
                              runDebugger
                              resetNextFrame
      _ -> return ()
