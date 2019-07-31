module Keys where

import Graphics.UI.GLFW
import System.Clock
import Data.Char

keyNames :: [(String, Key)]
keyNames = [
        ("Unknown", Key'Unknown),
        ("A", Key'A), ("B", Key'B), ("C", Key'C), ("D", Key'D),
        ("E", Key'E), ("F", Key'F), ("G", Key'G), ("H", Key'H),
        ("I", Key'I), ("J", Key'J), ("K", Key'K), ("L", Key'L),
        ("M", Key'M), ("N", Key'N), ("O", Key'O), ("P", Key'P),
        ("Q", Key'Q), ("R", Key'R), ("S", Key'S), ("T", Key'T),
        ("U", Key'U), ("V", Key'V), ("W", Key'W), ("X", Key'X),
        ("Y", Key'Y), ("Z", Key'Z), ("1", Key'1), ("2", Key'2),
        ("3", Key'3), ("4", Key'4), ("5", Key'5), ("6", Key'6),
        ("7", Key'7), ("8", Key'8), ("9", Key'9), ("0", Key'0),
        ("Return", Key'Enter),
        ("Escape", Key'Escape),
        ("Backspace", Key'Backspace),
        ("Tab", Key'Tab),
        ("Space", Key'Space),
        ("Minus", Key'Minus),
        ("Equals", Key'Equal),
        ("LeftBracket", Key'LeftBracket),
        ("RightBracket", Key'RightBracket),
        ("Backslash", Key'Backslash),
--         ("NonUSHash", Key'NonUSHash),
        ("Semicolon", Key'Semicolon),
        ("Apostrophe", Key'Apostrophe),
        ("Grave", Key'GraveAccent),
        ("Comma", Key'Comma),
        ("Period", Key'Period),
        ("Slash", Key'Slash),
        ("CapsLock", Key'CapsLock),
        ("F1", Key'F1), ("F2", Key'F2), ("F3", Key'F3), ("F4", Key'F4),
        ("F5", Key'F5), ("F6", Key'F6), ("F7", Key'F7), ("F8", Key'F8),
        ("F9", Key'F9), ("F10", Key'F10), ("F11", Key'F11), ("F12", Key'F12),
        ("PrintScreen", Key'PrintScreen),
        ("ScrollLock", Key'ScrollLock),
        ("Pause", Key'Pause),
        ("Insert", Key'Insert),
        ("Home", Key'Home),
        ("PageUp", Key'PageUp),
        ("Delete", Key'Delete),
        ("End", Key'End),
        ("PageDown", Key'PageDown),
        ("Right", Key'Right),
        ("Left", Key'Left),
        ("Down", Key'Down),
        ("Up", Key'Up),
--         ("NumLockClear", Key'NumLockClear),
        ("KPDivide", Key'PadDivide),
        ("KPMultiply", Key'PadMultiply),
        ("KPMinus", Key'PadSubtract),
        ("KPPlus", Key'PadAdd),
        ("KPEnter", Key'PadEnter),
        ("KP1", Key'Pad1), ("KP2", Key'Pad2), ("KP3", Key'Pad3), ("KP4", Key'Pad4),
        ("KP5", Key'Pad5), ("KP6", Key'Pad6), ("KP7", Key'Pad7), ("KP8", Key'Pad8),
        ("KP9", Key'Pad9), ("KP0", Key'Pad0),
--         ("KPPeriod", Key'KPPeriod),
--         ("NonUSBackslash", Key'NonUSBackslash),
--         ("Application", Key'Application),
--         ("Power", Key'Power),
--         ("KPEquals", Key'KPEquals),
        ("F13", Key'F13), ("F14", Key'F14), ("F15", Key'F15), ("F16", Key'F16),
        ("F17", Key'F17), ("F18", Key'F18), ("F19", Key'F19), ("F20", Key'F20),
        ("F21", Key'F21), ("F22", Key'F22), ("F23", Key'F23), ("F24", Key'F24),
        ("KPDecimal", Key'PadDecimal),
--         ("KPHexadecimal", Key'KPHexadecimal),
        ("LCtrl", Key'LeftControl),
        ("LShift", Key'LeftShift),
        ("LAlt", Key'LeftAlt),
--         ("LGUI", Key'LGUI),
        ("RCtrl", Key'RightControl),
        ("RShift", Key'RightShift),
        ("RAlt", Key'RightAlt)
    ]

scancodeFromString :: String -> Maybe Key
scancodeFromString name = lookup name keyNames

data Options = Options {
    screenScale :: (Int, Int),
    topOverscan :: Int,
    bottomOverscan :: Int,
    motionBlurAlpha :: Float,

    joystick1Left :: [String],
    joystick1Right :: [String],
    joystick1Up :: [String],
    joystick1Down :: [String],
    joystick2Left :: [String],
    joystick2Right :: [String],
    joystick2Up :: [String],
    joystick2Down :: [String],
    joystick1Trigger :: [String],
    joystick2Trigger :: [String],
    dumpState :: [String],
    gameQuit :: [String],
    gameSelect :: [String],
    gameReset :: [String],
    tvType :: [String],
    enterDebugger :: [String],
    debugMode :: [String],
    writeRecord :: [String],
    delayLeft :: [String],
    delayRight :: [String],
    delayUp :: [String],
    delayDown :: [String],
    keyboardController00 :: [String],
    keyboardController01 :: [String],
    keyboardController02 :: [String],
    keyboardController03 :: [String],
    keyboardController04 :: [String],
    keyboardController05 :: [String],
    keyboardController10 :: [String],
    keyboardController11 :: [String],
    keyboardController12 :: [String],
    keyboardController13 :: [String],
    keyboardController14 :: [String],
    keyboardController15 :: [String],
    keyboardController20 :: [String],
    keyboardController21 :: [String],
    keyboardController22 :: [String],
    keyboardController23 :: [String],
    keyboardController24 :: [String],
    keyboardController25 :: [String],
    keyboardController30 :: [String],
    keyboardController31 :: [String],
    keyboardController32 :: [String],
    keyboardController33 :: [String],
    keyboardController34 :: [String],
    keyboardController35 :: [String]
} deriving (Show, Read)

defaultOptions :: Options
defaultOptions = Options {
    screenScale = (5, 5),
    topOverscan = 10,
    bottomOverscan = 10,
    motionBlurAlpha = 1.0,

    joystick1Left = ["Left"],
    joystick1Right = ["Right"],
    joystick1Up = ["Up"],
    joystick1Down = ["Down"],
    joystick2Left = ["LeftBracket"],
    joystick2Right = ["RightBracket"],
    joystick2Up = ["Equals"],
    joystick2Down = ["Apostrophe"],
    joystick1Trigger = ["Space"],
    joystick2Trigger = ["Return"],
    dumpState = ["1"],
    gameQuit = ["Q"],
    gameSelect = ["C"],
    gameReset = ["V"],
    tvType = ["X"],
    enterDebugger = ["Escape"],
    debugMode = ["Backslash"],
    writeRecord = ["W"],
    delayLeft = [],
    delayRight = [],
    delayUp = [],
    delayDown = [],
    keyboardController00 = ["7"],
    keyboardController01 = ["6"],
    keyboardController02 = ["5"],
    keyboardController03 = ["0"],
    keyboardController04 = ["9"],
    keyboardController05 = ["8"],
    keyboardController10 = ["U"],
    keyboardController11 = ["Y"],
    keyboardController12 = ["T"],
    keyboardController13 = ["P"],
    keyboardController14 = ["O"],
    keyboardController15 = ["I"],
    keyboardController20 = ["J"],
    keyboardController21 = ["H"],
    keyboardController22 = ["G"],
    keyboardController23 = ["Semicolon"],
    keyboardController24 = ["L"],
    keyboardController25 = ["K"],
    keyboardController30 = ["M"],
    keyboardController31 = ["N"],
    keyboardController32 = ["B"],
    keyboardController33 = ["Slash"],
    keyboardController34 = ["Period"],
    keyboardController35 = ["Comma"]
}

data UIKey = UIKey { uiKey :: Key,
                     uiScancode :: Int,
                     uiState :: KeyState,
                     uiMods :: ModifierKeys,
                     uiTime :: TimeSpec
                   } deriving Show

interpretKey :: Key -> KeyState -> ModifierKeys -> Maybe Char
interpretKey Key'Up KeyState'Pressed _ = Just (chr 11)
interpretKey Key'Down KeyState'Pressed _ = Just (chr 10)
interpretKey Key'Left KeyState'Pressed _ = Just (chr 8)
interpretKey Key'Right KeyState'Pressed _ = Just (chr 9)
-- interpretKey Key'PageDown KeyState'Pressed _ = Just (chr 15)
interpretKey Key'L KeyState'Pressed ModifierKeys {modifierKeysControl=True} = Just (chr 12)

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
