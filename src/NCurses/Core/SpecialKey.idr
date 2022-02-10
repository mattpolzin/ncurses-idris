module NCurses.Core.SpecialKey

import NCurses.Core

%default total

%foreign libhelper "keyF0"
prim__keyF0 : PrimIO Char

%foreign libhelper "keyF1"
prim__keyF1 : PrimIO Char

%foreign libncurses "keypad"
prim__keypad : AnyPtr -> Int -> PrimIO ()

%foreign libhelper "keyF2"
prim__keyF2 : PrimIO Char

%foreign libhelper "keyF3"
prim__keyF3 : PrimIO Char

%foreign libhelper "keyF4"
prim__keyF4 : PrimIO Char

%foreign libhelper "keyF5"
prim__keyF5 : PrimIO Char

%foreign libhelper "keyF6"
prim__keyF6 : PrimIO Char

%foreign libhelper "keyF7"
prim__keyF7 : PrimIO Char

%foreign libhelper "keyF8"
prim__keyF8 : PrimIO Char

%foreign libhelper "keyF9"
prim__keyF9 : PrimIO Char

%foreign libhelper "keyF10"
prim__keyF10 : PrimIO Char

%foreign libhelper "keyF11"
prim__keyF11 : PrimIO Char

%foreign libhelper "keyF12"
prim__keyF12 : PrimIO Char

%foreign libhelper "keyUp"
prim__keyUp : PrimIO Char

%foreign libhelper "keyDown"
prim__keyDown : PrimIO Char

%foreign libhelper "keyLeft"
prim__keyLeft : PrimIO Char

%foreign libhelper "keyRight"
prim__keyRight : PrimIO Char

%foreign libhelper "keyBackspace"
prim__keyBackspace : PrimIO Char

||| Keys that can be used when keypad is turned on.
||| See @keypad@ and @keypad'@.
public export
data Key = F0
         | F1
         | F2
         | F3
         | F4
         | F5
         | F6
         | F7
         | F8
         | F9
         | F10
         | F11
         | F12
         | Up
         | Down
         | Left
         | Right
         | Backspace

||| Turn a Key into a Char that can be used to compare against
||| the results of getCh. This only applies if you have enabled
||| keypad for the given window.                                               
||| See @keypad@ and @keypad'@.
export
fnKeyChar : HasIO io => Key -> io Char
fnKeyChar F0        = primIO $ prim__keyF0
fnKeyChar F1        = primIO $ prim__keyF1
fnKeyChar F2        = primIO $ prim__keyF2
fnKeyChar F3        = primIO $ prim__keyF3
fnKeyChar F4        = primIO $ prim__keyF4
fnKeyChar F5        = primIO $ prim__keyF5
fnKeyChar F6        = primIO $ prim__keyF6
fnKeyChar F7        = primIO $ prim__keyF7
fnKeyChar F8        = primIO $ prim__keyF8
fnKeyChar F9        = primIO $ prim__keyF9
fnKeyChar F10       = primIO $ prim__keyF10
fnKeyChar F11       = primIO $ prim__keyF11
fnKeyChar F12       = primIO $ prim__keyF12
fnKeyChar Up        = primIO $ prim__keyUp
fnKeyChar Down      = primIO $ prim__keyDown
fnKeyChar Left      = primIO $ prim__keyLeft
fnKeyChar Right     = primIO $ prim__keyRight
fnKeyChar Backspace = primIO $ prim__keyBackspace

||| Turn keypad mode on or off for the given window.
||| When on, function keys (F0, F1, ...) and arrow keys are
||| transformed into single chars that can be compared against
||| the result of passing a particular key to the fnKeyChar
||| function.
export
keypad' : HasIO io => Window -> (enable : Bool) -> io ()
keypad' (Win win) enable = primIO $ prim__keypad win (boolToInt enable)

||| Turn keypad mode on or off for the std window.
||| When on, function keys (F0, F1, ...) and arrow keys are
||| transformed into single chars that can be compared against
||| the result of passing a particular key to the fnKeyChar
||| function.
export
keypad : HasIO io => (enable : Bool) -> io ()
keypad enable = keypad' !stdWindow enable

