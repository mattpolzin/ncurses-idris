module NCurses

%default total

libncurses : String -> String
libncurses fn = "C:" ++ fn ++ ",libncurses"

libhelper : String -> String
libhelper fn = "C:" ++ fn ++ ",libncurses-idris"

%foreign libncurses "cbreak"
prim__cBreak : PrimIO ()

%foreign libncurses "noecho"
prim__noEcho : PrimIO ()

%foreign libncurses "curs_set"
prim__setCursorVisibility : Int -> PrimIO ()

%foreign libncurses "getch"
prim__getCh : PrimIO Char

%foreign libncurses "start_color"
prim__startColor : PrimIO ()

%foreign libncurses "initscr"
prim__initScr : PrimIO ()

%foreign libncurses "endwin"
prim__endWin : PrimIO ()

%foreign libhelper "std_win"
prim__stdWindow : PrimIO AnyPtr

%foreign libncurses "newwin"
prim__newWindow : Int -> Int -> Int -> Int -> PrimIO AnyPtr

%foreign libncurses "getmaxx"
prim__maxXWindow : AnyPtr -> PrimIO Int

%foreign libncurses "getmaxy"
prim__maxYWindow : AnyPtr -> PrimIO Int

%foreign libncurses "refresh"
prim__refresh : PrimIO ()

%foreign libncurses "wrefresh"
prim__refreshWindow : AnyPtr -> PrimIO ()

%foreign libncurses "clear"
prim__clear : PrimIO ()

%foreign libncurses "wclear"
prim__clearWindow : AnyPtr -> PrimIO ()

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

%foreign libncurses "move"
prim__move : Int -> Int -> PrimIO ()

%foreign libncurses "mvchgat"
prim__mvChangeAt : Int -> Int -> Int -> Int -> Int -> AnyPtr -> PrimIO ()

%foreign libncurses "mvwchgat"
prim__mvChangeAtWindow : AnyPtr -> Int -> Int -> Int -> Int -> Int -> AnyPtr -> PrimIO ()

%foreign libncurses "attrset"
prim__setAttr : Int -> PrimIO ()

%foreign libncurses "wattrset"
prim__setAttrWindow : AnyPtr -> Int -> PrimIO ()

%foreign libhelper "normal_attr"
prim__normalAttr : PrimIO Int

%foreign libhelper "underline_attr"
prim__underlineAttr : PrimIO Int

%foreign libhelper "standout_attr"
prim__standoutAttr : PrimIO Int

%foreign libhelper "reverse_attr"
prim__reverseAttr : PrimIO Int

%foreign libhelper "blink_attr"
prim__blinkAttr : PrimIO Int

%foreign libhelper "dim_attr"
prim__dimAttr : PrimIO Int

%foreign libhelper "bold_attr"
prim__boldAttr : PrimIO Int

%foreign libhelper "protected_attr"
prim__protectedAttr : PrimIO Int

%foreign libhelper "invisible_attr"
prim__invisibleAttr : PrimIO Int

%foreign libhelper "color_pair_attr"
prim__colorPairAttr : Int -> PrimIO Int

%foreign libhelper "black_color"
prim__blackColor : PrimIO Int

%foreign libhelper "red_color"
prim__redColor : PrimIO Int

%foreign libhelper "green_color"
prim__greenColor : PrimIO Int

%foreign libhelper "yellow_color"
prim__yellowColor : PrimIO Int

%foreign libhelper "blue_color"
prim__blueColor : PrimIO Int

%foreign libhelper "magenta_color"
prim__magentaColor : PrimIO Int

%foreign libhelper "cyan_color"
prim__cyanColor : PrimIO Int

%foreign libhelper "white_color"
prim__whiteColor : PrimIO Int

%foreign libncurses "init_pair"
prim__initColorPair : Int -> Int -> Int -> PrimIO ()

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

||| Begin using color mode.
export
startColor : HasIO io => io ()
startColor = primIO $ prim__startColor

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
getMaxSize : HasIO io => Window -> io (Nat, Nat)
getMaxSize (Win win) = do y <- (primIO $ prim__maxYWindow win)
                          x <- (primIO $ prim__maxXWindow win)
                          pure (fromInteger (cast y), fromInteger (cast x))

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
||| See @clear'@ to clear any other window.
export
clear : HasIO io => io ()
clear = primIO $ prim__clear

||| Clear a particular window.
export
clear' : HasIO io => Window -> io ()
clear' (Win win) = primIO $ prim__clearWindow win

||| Get a single character immediately.
|||
||| This contrasts with a read from the default shell
||| `stdin` which will wait until a newline to flush
||| its buffer and send input to a program.
export
getCh : HasIO io => io Char
getCh = primIO $ prim__getCh

||| Switch keyboard input to cbreak mode.
export
cBreak : HasIO io => io ()
cBreak = primIO $ prim__cBreak

||| Switch keyboard input to noecho mode.
export
noEcho : HasIO io => io ()
noEcho = primIO $ prim__noEcho

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

||| Move the cursor in the standard window.
export
nMoveCursor : HasIO io => (row : Nat) -> (col : Nat) -> io ()
nMoveCursor row col = primIO $ prim__move (cast row) (cast col)

||| Move the cursor and print to the standard window.
|||
||| See @nPutStrLn'@ to print to a particular window.
export
nPutStrLn : HasIO io => (row : Nat) -> String -> io ()
nPutStrLn row str = primIO $ prim__mvPrint (cast row) 0 "%s" str

||| Move the cursor and print to the given window.
export
nPutStrLn' : HasIO io => Window -> (row : Nat) -> String -> io ()
nPutStrLn' (Win win) row str = primIO $ prim__mvPrintWindow win (cast row) 0 "%s" str

||| The default ncurses colors that can be used in constructing
||| color pairs.
public export
data Color = Black
           | Red
           | Green
           | Yellow
           | Blue
           | Magenta
           | Cyan
           | White

export
data ColorPair = MkColorPair Nat

export
defaultColorPair : ColorPair
defaultColorPair = MkColorPair 0

getColor : HasIO io => Color -> io Int
getColor color = case color of
                      Black   => primIO $ prim__blackColor
                      Red     => primIO $ prim__redColor
                      Green   => primIO $ prim__greenColor
                      Yellow  => primIO $ prim__yellowColor
                      Blue    => primIO $ prim__blueColor
                      Magenta => primIO $ prim__magentaColor
                      Cyan    => primIO $ prim__cyanColor
                      White   => primIO $ prim__whiteColor
               
||| Create a new color pair. You must tell it the index to create
||| the color at, which should be a number starting at 0. Some
||| platforms allow you to redefine a color at a given index but this
||| is not universally supported.
|||
||| You might notice that ncurses expects color indices to start at 1 --
||| this function increments the index it is given so that passing 0 to
||| it will use the first available user color pair index of 1.
export
initColorPair : HasIO io => Nat -> (fg : Color) -> (bg : Color) -> io ColorPair
initColorPair idx fg bg = 
  do bgColor <- getColor bg
     fgColor <- getColor fg
     let actualIdx = (S idx)
     primIO $ prim__initColorPair (cast actualIdx) fgColor bgColor 
     pure (MkColorPair actualIdx)

||| Attributes that can be given to text within an ncurses window.
public export
data Attribute = Normal
               | Underline
               | Standout
               | Reverse
               | Blink
               | Dim
               | Bold
               | Protected
               | Invisible
               | CP ColorPair

||| Get the Int representation ncurses cares about for a
||| particular @Attribute@.
getAttribute : HasIO io => Attribute -> io Int
getAttribute attr = case attr of
                         Normal    => primIO $ prim__normalAttr
                         Underline => primIO $ prim__underlineAttr
                         Standout  => primIO $ prim__standoutAttr
                         Reverse   => primIO $ prim__reverseAttr
                         Blink     => primIO $ prim__blinkAttr
                         Dim       => primIO $ prim__dimAttr
                         Bold      => primIO $ prim__boldAttr
                         Protected => primIO $ prim__protectedAttr
                         Invisible => primIO $ prim__invisibleAttr
                         (CP (MkColorPair idx)) => primIO $ prim__colorPairAttr (cast idx)

||| Set an attribute to be applied in the standard window
||| until it is cleared or overwritten.
|||
||| See @nSetAttr'@ for a version that works on
||| any given window.
export
nSetAttr : HasIO io => Attribute -> io ()
nSetAttr attr = do attribute <- getAttribute attr
                   primIO $ prim__setAttr attribute

||| Set an attribute to be applied in the given window
||| until it is cleared or overwritten.
export
nSetAttr' : HasIO io => Window -> Attribute -> io ()
nSetAttr' (Win win) attr = do attribute <- getAttribute attr
                              primIO $ prim__setAttrWindow win attribute

||| Change the attributes at the given position in the standard window.
||| A len of Nothing means "the whole line."
||| A color pair with @defaultColorPair@ offering a sane default.
|||
||| See @nChangeAttr'@ to change attributes in another window.
export
nChangeAttr : HasIO io 
           => (row : Nat) 
           -> (col : Nat) 
           -> (len : Maybe Nat) 
           -> Attribute 
           -> ColorPair
           -> io ()
nChangeAttr row col len attr (MkColorPair colorIdx) = 
  let length = the Int (maybe (-1) cast len)
  in  
      do attribute <- getAttribute attr
         primIO $ 
           prim__mvChangeAt (cast row) (cast col) length attribute (cast colorIdx) prim__getNullAnyPtr

||| Change the attributes at the given position in the given window.
||| A len of Nothing means "the whole line."
||| A color pair with @defaultColorPair@ offering a sane default.
export
nChangeAttr' : HasIO io 
            => Window
            -> (row : Nat) 
            -> (col : Nat) 
            -> (len : Maybe Nat) 
            -> Attribute 
            -> ColorPair
            -> io ()
nChangeAttr' (Win win) row col len attr (MkColorPair colorIdx) = 
  let length = the Int (maybe (-1) cast len)
  in  
      do attribute <- getAttribute attr
         primIO $ 
           prim__mvChangeAtWindow win (cast row) (cast col) length attribute (cast colorIdx) prim__getNullAnyPtr

