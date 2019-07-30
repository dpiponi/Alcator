module Keys where

import Graphics.UI.GLFW
import System.Clock

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
