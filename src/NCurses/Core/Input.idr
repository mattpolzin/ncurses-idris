module NCurses.Core.Input

import NCurses.Core

%foreign libncurses "cbreak"
prim__cBreak : PrimIO ()

%foreign libncurses "nocbreak"
prim__noCBreak : PrimIO ()

%foreign libncurses "echo"
prim__echo : PrimIO ()

%foreign libncurses "noecho"
prim__noEcho : PrimIO ()

%foreign libncurses "nodelay"
prim__noDelay : AnyPtr -> Bool -> PrimIO ()

%foreign libncurses "getch"
prim__getCh : PrimIO Char

%foreign libncurses "curs_set"
prim__setCursorVisibility : Int -> PrimIO ()

||| Switch keyboard input to cbreak mode.
|||
||| In cbreak mode, characters are sent to the program
||| immediately instead of waiting for a newline to be
||| entered. This is the opposite of how most terminals
||| accept input by default during normal operation.
export
cBreak : HasIO io => io ()
cBreak = primIO $ prim__cBreak

||| Switch keyboard input out of cbreak mode.
|||
||| In cbreak mode, characters are sent to the program
||| immediately instead of waiting for a newline to be
||| entered. This is the opposite of how most terminals
||| accept input by default during normal operation.
export
noCBreak : HasIO io => io ()
noCBreak = primIO $ prim__noCBreak

||| Switch keyboard input to echo mode.
export
echo : HasIO io => io ()
echo = primIO $ prim__echo

||| Switch keyboard input to noecho mode.
export
noEcho : HasIO io => io ()
noEcho = primIO $ prim__noEcho

||| @noDelay'@ controls whether @getCh@ is blocking or not.
||| When noDelay is False, @getCh@ will wait until the user types. Otherwise, @getCh@
||| returns immediately and returns @Nothing@ if the user has not input anything.
export
noDelay' : HasIO io => Window -> Bool -> io ()
noDelay' (Win win) on = primIO $ prim__noDelay win on

||| @noDelay@ controls whether @getCh@ is blocking or not.
||| When noDelay is False, @getCh@ will wait until the user types. Otherwise, @getCh@
||| returns immediately and returns @Nothing@ if the user has not input anything.
export
noDelay : HasIO io => Bool -> io ()
noDelay on = noDelay' !stdWindow on

||| Get a single character. 
|||
||| If cbreak mode is on, the character is returned
||| immediately. This contrasts with a read from
||| the default shell `stdin` which will wait until a
||| newline to flush its buffer and send input to a
||| program.
export
getCh : HasIO io => io Char
getCh = primIO $ prim__getCh

public export 
data CursorVisibility = CInvisible | CNormal| CHighlyVisible

||| Set the visibility of the cursor.
export
setCursorVisibility : HasIO io => CursorVisibility -> io ()
setCursorVisibility vis = primIO $ prim__setCursorVisibility $
                                     case vis of
                                          CInvisible     => 0
                                          CNormal        => 1
                                          CHighlyVisible => 2

