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

%foreign libncurses "wgetch"
prim__getChWindow : AnyPtr -> PrimIO Char

%foreign libncurses "getch"
prim__safeGetCh : PrimIO Int

%foreign libncurses "wgetch"
prim_safeGetChWindow  : AnyPtr -> PrimIO Int

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
||| returns immediately and returns an error value. Use @safeGetCh@ with @noDelay@ on to
||| turn those error values into @Nothing@.
export
noDelay' : HasIO io => Window -> Bool -> io ()
noDelay' (Win win) on = primIO $ prim__noDelay win on

||| @noDelay@ controls whether @getCh@ is blocking or not.
||| When noDelay is False, @getCh@ will wait until the user types. Otherwise, @getCh@
||| returns immediately and returns an error value. Use @safeGetCh@ with @noDelay@ on to
||| turn those error values into @Nothing@.
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

||| Get a single character from the given window. 
|||
||| If cbreak mode is on, the character is returned
||| immediately. This contrasts with a read from
||| the default shell `stdin` which will wait until a
||| newline to flush its buffer and send input to a
||| program.
export
getCh' : HasIO io => Window -> io Char
getCh' (Win win) = primIO $ prim__getChWindow win

||| Get a single character or Nothing on error. 
|||
||| If cbreak mode is on, the character is returned
||| immediately. This contrasts with a read from
||| the default shell `stdin` which will wait until a
||| newline to flush its buffer and send input to a
||| program.
export
safeGetCh : HasIO io => io (Maybe Char)
safeGetCh = do
  err <- primIO $ prim__err
  ch <- primIO $ prim__safeGetCh
  pure $
    if ch == err
       then Nothing
       else Just (cast ch)

||| Get a single character from the given window or Nothing
||| on error. 
|||
||| If cbreak mode is on, the character is returned
||| immediately. This contrasts with a read from
||| the default shell `stdin` which will wait until a
||| newline to flush its buffer and send input to a
||| program.
export
safeGetCh' : HasIO io => Window -> io (Maybe Char)
safeGetCh' (Win win) = do
  err <- primIO $ prim__err
  ch <- primIO $ prim_safeGetChWindow win
  pure $
    if ch == err
       then Nothing
       else Just (cast ch)

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

