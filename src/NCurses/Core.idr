module NCurses.Core

libncurses : String -> String
libncurses fn = "C:" ++ fn ++ ",libncurses"

libhelper : String -> String
libhelper fn = "C:" ++ fn ++ ",libncurses-idris"

%foreign libhelper "ncurses_err"
prim__err : PrimIO Int

%foreign libhelper "std_win"
prim__stdWindow : PrimIO AnyPtr

%foreign libncurses "newwin"
prim__newWindow : Int -> Int -> Int -> Int -> PrimIO AnyPtr

%foreign libncurses "getmaxx"
prim__maxXWindow : AnyPtr -> PrimIO Int

%foreign libncurses "getmaxy"
prim__maxYWindow : AnyPtr -> PrimIO Int

-- Returns ERR/OK
%foreign libncurses "wresize"
prim__resizeWindow : AnyPtr -> Int -> Int -> PrimIO Int

%foreign libncurses "getcury"
prim__getYWindow : AnyPtr -> PrimIO Int

%foreign libncurses "getcurx"
prim__getXWindow : AnyPtr -> PrimIO Int

%foreign libncurses "refresh"
prim__refresh : PrimIO ()

%foreign libncurses "wrefresh"
prim__refreshWindow : AnyPtr -> PrimIO ()

%foreign libncurses "clear"
prim__clear : PrimIO ()

%foreign libncurses "wclear"
prim__clearWindow : AnyPtr -> PrimIO ()

%foreign libncurses "erase"
prim__erase : PrimIO ()

%foreign libncurses "werase"
prim__eraseWindow : AnyPtr -> PrimIO ()

%foreign libncurses "printw"
prim__print : String -> String -> PrimIO ()

%foreign libncurses "wprintw"
prim__printWindow : AnyPtr -> String -> String -> PrimIO ()

%foreign libncurses "addch"
prim__addChar : Char -> PrimIO ()

%foreign libncurses "waddch"
prim__addCharWindow : AnyPtr -> Char -> PrimIO ()

||| move to row and column given and print
||| the given string.
|||
||| IMPORTANT: This takes the y-position before
||| the x-position.
|||
||| The second to last argument is a format string but
||| its best to just always pass strings ("%s").
%foreign libncurses "mvprintw"
prim__mvPrint : Int -> Int -> String -> String -> PrimIO ()

%foreign libncurses "mvwprintw"
prim__mvPrintWindow : AnyPtr -> Int -> Int -> String -> String -> PrimIO ()

%foreign libncurses "vline"
prim__verticalLine : Char -> Int -> PrimIO ()

%foreign libncurses "wvline"
prim__verticalLineWindow : AnyPtr -> Char -> Int -> PrimIO ()

%foreign libncurses "hline"
prim__horizontalLine : Char -> Int -> PrimIO ()

%foreign libncurses "whline"
prim__horizontalLineWindow : AnyPtr -> Char -> Int -> PrimIO ()

%foreign libncurses "move"
prim__move : Int -> Int -> PrimIO ()

%foreign libncurses "wmove"
prim__moveWindow : AnyPtr -> Int -> Int -> PrimIO ()

%foreign libncurses "initscr"
prim__initScr : PrimIO ()

%foreign libncurses "endwin"
prim__endWin : PrimIO ()

boolToInt : Bool -> Int
boolToInt False = 0
boolToInt True  = 1

||| When you make new windows with @newWindow@
||| this data type stores a reference for you
||| to use with any function that operates on
||| a particular window.
|||
||| Most functions operate on a window even if
||| they don't appear to; you get a default window
||| for free with @initNCurses@. You can request the
||| default window with @stdWindow@.
export
data Window = Win AnyPtr

||| Get the default standard ncurses window.
export
stdWindow : HasIO io => io Window
stdWindow = Win <$> (primIO $ prim__stdWindow)

||| Create a new ncurses window.
export
newWindow : HasIO io => (height : Nat) -> (width : Nat) -> (y : Nat) -> (x : Nat) -> io Window
newWindow height width y x = Win <$> (primIO $ prim__newWindow (cast height) (cast width) (cast y) (cast x))

||| As is normal for ncurses, returns (height, width).
export
getMaxSize' : HasIO io => Window -> io (Nat, Nat)
getMaxSize' (Win win) = do y <- (primIO $ prim__maxYWindow win)
                           x <- (primIO $ prim__maxXWindow win)
                           pure (fromInteger (cast y), fromInteger (cast x))

export
getYPos' : HasIO io => Window -> io Nat
getYPos' (Win win) = map cast . primIO $ prim__getYWindow win

export
getYPos : HasIO io => io Nat
getYPos = getYPos' !stdWindow

export
getXPos' : HasIO io => Window -> io Nat
getXPos' (Win win) = map cast . primIO $ prim__getXWindow win

export
getXPos : HasIO io => io Nat
getXPos = getXPos' !stdWindow

export
setWindowSize : HasIO io => Window -> (rows : Nat) -> (cols : Nat) -> io ()
setWindowSize (Win win) rows cols= ignore . primIO $ prim__resizeWindow win (cast rows) (cast cols) 

||| Refresh the standard window.
|||
||| See @refresh'@ to refresh any other window.
export
refresh : HasIO io => io ()
refresh = primIO $ prim__refresh

||| Refresh a particular window.
export
refresh' : HasIO io => Window -> io ()
refresh' (Win win) = primIO $ prim__refreshWindow win

||| Clear the standard window.
|||
||| Clearing causes NCurses to redraw the whole terminal on
||| the next call to refresh. See @erase@ for a less intensive
||| process that allows NCurses to intelligently determine how
||| much of the terminal view needs to be redrawn.
|||
||| See @clear'@ to clear any other window.
export
clear : HasIO io => io ()
clear = primIO $ prim__clear

||| Clear a particular window.
|||
||| Clearing causes NCurses to redraw the whole terminal on
||| the next call to refresh. See @erase@ for a less intensive
||| process that allows NCurses to intelligently determine how
||| much of the terminal view needs to be redrawn.
export
clear' : HasIO io => Window -> io ()
clear' (Win win) = primIO $ prim__clearWindow win

||| Erase the standard window.
|||
||| See @erase'@ to erase any other window.
export
erase : HasIO io => io ()
erase = primIO $ prim__erase

||| Erase the given window.
export
erase' : HasIO io => Window -> io ()
erase' (Win win) = primIO $ prim__eraseWindow win

export
nPutCh : HasIO io => Char -> io ()
nPutCh ch = primIO $ prim__addChar ch

export
nPutCh' : HasIO io => Window -> Char -> io ()
nPutCh' (Win win) ch = primIO $ prim__addCharWindow win ch

export
nPutStr : HasIO io => String -> io ()
nPutStr str = primIO $ prim__print "%s" str

export
nPutStr' : HasIO io => Window -> String -> io ()
nPutStr' (Win win) str = primIO $ prim__printWindow win "%s" str

export
nPutStrLn : HasIO io => String -> io ()
nPutStrLn str = primIO $ prim__print "%s\n" str

export
nPutStrLn' : HasIO io => Window -> String -> io ()
nPutStrLn' (Win win) str = primIO $ prim__printWindow win "%s\n" str

||| Move the cursor and print to the standard window.
|||
||| See @nPutStrLnAt'@ to print to a particular window.
export
nPutStrAt : HasIO io => (row : Nat) -> String -> io ()
nPutStrAt row str = primIO $ prim__mvPrint (cast row) 0 "%s" str

||| Move the cursor and print to the given window.
export
nPutStrAt' : HasIO io => Window -> (row : Nat) -> String -> io ()
nPutStrAt' (Win win) row str = primIO $ prim__mvPrintWindow win (cast row) 0 "%s" str

||| Draw a vertical line comprised of the given character.
export
nVerticalLine : HasIO io => Char -> Nat -> io ()
nVerticalLine ch n = primIO $ prim__verticalLine ch (cast n)

||| Draw a vertical line comprised of the given character in
||| the given window.
export
nVerticalLine' : HasIO io => Window -> Char -> Nat -> io ()
nVerticalLine' (Win win) ch n = primIO $ prim__verticalLineWindow win ch (cast n)

||| Draw a horizontal line comprised of the given character.
export
nHorizontalLine : HasIO io => Char -> Nat -> io ()
nHorizontalLine ch n = primIO $ prim__horizontalLine ch (cast n)

||| Draw a horizontal line comprised of the given character in
||| the given window.
export
nHorizontalLine' : HasIO io => Window -> Char -> Nat -> io ()
nHorizontalLine' (Win win) ch n = primIO $ prim__horizontalLineWindow win ch (cast n)

||| Move the cursor in the standard window.
export
nMoveCursor : HasIO io => (row : Nat) -> (col : Nat) -> io ()
nMoveCursor row col = primIO $ prim__move (cast row) (cast col)

||| Move the cursor in the given window.
export
nMoveCursor' : HasIO io => Window -> (row : Nat) -> (col : Nat) -> io ()
nMoveCursor' (Win win) row col = primIO $ prim__moveWindow win (cast row) (cast col)

||| You must call @initNCurses@ before using the other
||| ncurses functions and you must call @deinitNCurses@
||| after you are done (before exiting your program).
|||
||| You will actually see weird behavior in the shell
||| post-exit of your program if you have not deinited
||| ncurses.
export
initNCurses : HasIO io => io ()
initNCurses = primIO $ prim__initScr

||| Must be called after @initNCurses@ and before your
||| program exits.
export
deinitNCurses : HasIO io => io ()
deinitNCurses = primIO $ prim__endWin

