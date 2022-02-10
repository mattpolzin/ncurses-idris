module NCurses.Core.Input

import NCurses.Core

%foreign libncurses "cbreak"
prim__cBreak : PrimIO ()

%foreign libncurses "noecho"
prim__noEcho : PrimIO ()

%foreign libncurses "getch"
prim__getCh : PrimIO Char

%foreign libncurses "curs_set"
prim__setCursorVisibility : Int -> PrimIO ()

||| Switch keyboard input to cbreak mode.
export
cBreak : HasIO io => io ()
cBreak = primIO $ prim__cBreak

||| Switch keyboard input to noecho mode.
export
noEcho : HasIO io => io ()
noEcho = primIO $ prim__noEcho

||| Get a single character immediately.
|||
||| This contrasts with a read from the default shell
||| `stdin` which will wait until a newline to flush
||| its buffer and send input to a program.
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

