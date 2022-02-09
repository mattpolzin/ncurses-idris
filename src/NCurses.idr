module NCurses

import public NCurses.Core as NCurses
import public NCurses.Core.Color as NCurses.Color
import public NCurses.Core.Attribute as NCurses.Attribute
import public NCurses.Core.SpecialKey as NCurses.SpecialKey

%default total

libncurses : String -> String
libncurses fn = "C:" ++ fn ++ ",libncurses"

libhelper : String -> String
libhelper fn = "C:" ++ fn ++ ",libncurses-idris"

%foreign libncurses "initscr"
prim__initScr : PrimIO ()

%foreign libncurses "endwin"
prim__endWin : PrimIO ()

%foreign libncurses "move"
prim__move : Int -> Int -> PrimIO ()

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

||| Move the cursor in the standard window.
export
nMoveCursor : HasIO io => (row : Nat) -> (col : Nat) -> io ()
nMoveCursor row col = primIO $ prim__move (cast row) (cast col)

