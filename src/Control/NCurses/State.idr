module Control.NCurses.State

import NCurses.Core
import NCurses.Core.Color
import NCurses.Core.Attribute
import NCurses.Core.SpecialKey
import NCurses.Core.Input

import public Control.Indexed
import public Data.DPair
import public Data.List.Elem

%default total

public export
record InputState where
  constructor MkInput
  ||| cBreak indicates whether characters typed by the user are delivered to the
  ||| program (via @getCh@) immediately or not. If not, they are delivered when a
  ||| newline is typed (the default behavior for most terminal environments).
  cBreak : Bool
  ||| echo indicates whether characters typed by the user are printed to the screen
  ||| by NCurses as well as delivered to @getCh@.
  echo   : Bool

public export
initInput : InputState
initInput = MkInput { cBreak  = True
                    , echo    = False
                    }

||| Windows allow a terminal screen to be divided up and drawn to separately
||| with their own relative coordinates.
public export
data Window : Type where
  ||| The `keypad` parameter controls whether special keys are supported as single
  ||| inputs.
  ||| Set to true to receive "special keys" as single values rather than composites
  ||| of two or more Chars. For example, arrow keys are represented as two input
  ||| chars, but you will receive @Key@ values for them instead with the @keypad@
  ||| property turned on.
  ||| This is on by default.
  |||
  ||| The `noDelay` parameter controls whether @getCh@ is blocking or not.
  ||| When noDelay is False, @getCh@ will wait until the user types. Otherwise, @getCh@
  ||| returns immediately and returns @Nothing@ if the user has not input anything.
  |||
  ||| If a window has a border, it is drawn inside the window's bounds. This also means the
  ||| effective area to draw within is 1 row/column smaller all of the way around the window.
  ||| Moving to row/column 0 will position the cursor just inside the border.
  MkWindow : (identifier : String)
          -> (keypad : Bool)
          -> (noDelay : Bool) -> Window

public export
identifier : State.Window -> String
identifier (MkWindow i _ _) = i

public export
(.identifier) : State.Window -> String
(.identifier) = identifier

public export
(.keypad) : State.Window -> Bool
(.keypad) (MkWindow _ k _) = k

public export
(.noDelay) : State.Window -> Bool
(.noDelay) (MkWindow _ _ d) = d

public export
setKeypad : (on : Bool) -> State.Window -> State.Window
setKeypad on (MkWindow i _ d) = MkWindow i on d

public export
newKeypadSameIdentity : (on : Bool) -> (w : State.Window) -> (identifier w) === (identifier (setKeypad on w))
newKeypadSameIdentity on (MkWindow identifier keypad noDelay) = Refl

public export
setNoDelay : (on : Bool) -> State.Window -> State.Window
setNoDelay on (MkWindow i k _) = MkWindow i k on

public export
newNoDelaySameIdentity : (on : Bool) -> (w : State.Window) -> (identifier w) === (identifier (setNoDelay on w))
newNoDelaySameIdentity on (MkWindow identifier keypad noDelay) = Refl

public export
initWindow : String -> State.Window
initWindow id = MkWindow { identifier = id
                         , keypad = True
                         , noDelay = False
                         }

||| The state of NCurses.
||| When active:
||| The `input` is the state of input that applies universally.
||| The `windows` are the currently available windows in the session, excluding
|||   the default (or standard) window which is always available.
||| The `currentWindow` is the window to be used for input & output. If Nothing,
|||   the default window is used.
||| The `colors` are the currently available colors in the session.
||| TODO: parameterize on Eq type to use as color id? Eq a => (colors : List a)
public export
data CursesState : Type where
  Inactive : CursesState
  Active : (0 input : InputState)
        -> (0 windows : List State.Window)
        -> (0 currentWindow : (w ** Elem w windows))
        -> (0 colors : List String)
        -> CursesState

public export
(.active) : CursesState -> Bool
(.active) Inactive = False
(.active) (Active _ _ _ _) = True

public export %inline
initState : CursesState
initState = Active initInput [initWindow "default"] (initWindow "default" ** Here) []

public export %inline
0 bumpWindow : DPair State.Window (\w => Elem w ws) -> DPair State.Window (\w => Elem w (y :: ws))
bumpWindow (q ** r) = (q ** There r)

namespace CursesState
  public export
  data IsActive : CursesState -> Type where
    ItIsActive : IsActive (Active _ _ _ _)

  public export
  data IsInactive : CursesState -> Type where
    ItIsInactive : IsInactive Inactive

  public export
  0 currentWindow : (s : CursesState) -> IsActive s => State.Window
  currentWindow Inactive @{ItIsActive} impossible
  currentWindow (Active _ _ (w ** _) _) = w

  public export %inline
  0 addWindow : (s : CursesState) -> IsActive s => (name : String) ->  CursesState
  addWindow Inactive @{ItIsActive} _ impossible
  addWindow (Active i ws w cs) n = Active i (initWindow n :: ws) (bumpWindow w) cs

  public export %inline
  0 addColor : (s : CursesState) -> IsActive s => (n : String) -> CursesState
  addColor Inactive @{ItIsActive} _ impossible
  addColor (Active i ws w cs) n = Active i ws w (n :: cs)

  public export %inline
  0 setEcho : (s : CursesState) -> IsActive s => (on : Bool) -> CursesState
  setEcho Inactive @{ItIsActive} _ impossible
  setEcho (Active input ws w cs) on = Active ({ echo := on } input) ws w cs

  public export %inline
  0 setCBreak : (s : CursesState) -> IsActive s => (on : Bool) -> CursesState
  setCBreak Inactive @{ItIsActive} _ impossible
  setCBreak (Active input ws w cs) on = Active ({ cBreak := on } input) ws w cs

  public export %inline
  0 swapKeypad : (on : Bool) -> (ws : List State.Window) -> Elem y ws -> List State.Window
  swapKeypad on (y :: xs) Here = setKeypad on y :: xs
  swapKeypad on (y :: xs) (There e) = y :: swapKeypad on xs e

  public export %inline
  0 swapKeypadPrf : (on : Bool) -> (ws : List State.Window) -> (e : Elem y ws) -> Elem (setKeypad on y) (swapKeypad on ws e)
  swapKeypadPrf on (y :: xs) Here = Here
  swapKeypadPrf on (y :: xs) (There x) = There $ swapKeypadPrf on xs x

  public export %inline
  0 swapNoDelay : (on : Bool) -> (ws : List State.Window) -> Elem y ws -> List State.Window
  swapNoDelay on (y :: xs) Here = setNoDelay on y :: xs
  swapNoDelay on (y :: xs) (There e) = y :: swapNoDelay on xs e

  public export %inline
  0 swapNoDelayPrf : (on : Bool) -> (ws : List State.Window) -> (e : Elem y ws) -> Elem (setNoDelay on y) (swapNoDelay on ws e)
  swapNoDelayPrf on (y :: xs) Here = Here
  swapNoDelayPrf on (y :: xs) (There x) = There $ swapNoDelayPrf on xs x

  public export %inline
  0 replaceWindow : {w' : State.Window} -> InputState -> List String -> (ws' : _) -> Elem w' ws' -> CursesState
  replaceWindow i cs ws elem = Active i ws (_ ** elem) cs

  public export %inline
  0 setKeypad : (s : CursesState) -> IsActive s => (on : Bool) -> CursesState
  setKeypad Inactive @{ItIsActive} _ impossible
  setKeypad (Active input ws w cs) on = replaceWindow input cs (swapKeypad on ws (snd w)) (swapKeypadPrf on ws (snd w))

  public export %inline
  0 setNoDelay : (s : CursesState) -> IsActive s => (on : Bool) -> CursesState
  setNoDelay Inactive @{ItIsActive} _ impossible
  setNoDelay (Active input ws w cs) on = replaceWindow input cs (swapNoDelay on ws (snd w)) (swapNoDelayPrf on ws (snd w))

  public export
  data YesDelay : CursesState -> Type where
    ItIsDelay : YesDelay (Active _ _ ((MkWindow _ _ False) ** _) _)

  public export
  data NoDelay : CursesState -> Type where
    ItIsNoDelay : NoDelay (Active _ _ ((MkWindow _ _ True) ** _) _)

  public export
  data YesKeypad : CursesState -> Type where
    ItIsKeypad : YesKeypad (Active _ _ ((MkWindow _ True _) ** _) _)

  public export
  data NoKeypad : CursesState -> Type where
    ItIsNoKeypad : NoKeypad (Active _ _ ((MkWindow _ False _) ** _) _)

namespace Attribute
  ||| Proof that the given color exists in the current
  ||| NCurses session.
  public export
  data HasColor : (0 name : String) -> CursesState -> Type where
    ItHasColor : Elem name cs => HasColor name (Active _ ws w cs)

  public export
  data Attribute : CursesState -> Type where
    Normal        : Attribute s
    Underline     : Attribute s
    Standout      : Attribute s
    Reverse       : Attribute s
    Blink         : Attribute s
    Dim           : Attribute s
    Bold          : Attribute s
    Protected     : Attribute s
    Invisible     : Attribute s
    DefaultColors : Attribute s
    Color         : (name : String) -> HasColor name s => Attribute s

  public export
  data AttrCmd : CursesState -> Type where
    SetAttr     : Attribute s -> AttrCmd s
    EnableAttr  : Attribute s -> AttrCmd s
    DisableAttr : Attribute s -> AttrCmd s

||| A Window border.
public export
record Border (color : String) where
  constructor MkBorder
--   {0 state : CursesState}
--   color : Subset String (\c => HasColor c state)
  left, right, top, bottom, topLeft, topRight, bottomLeft, bottomRight : BorderChar

namespace Output
  ||| An NCuress Position; pass the constructor a row and a column.
  ||| Note that this means the _y position_ comes first. Position starts
  ||| at (0,0) in the upper left corner of the window.
  public export
  record Position where
    constructor MkPosition
    row,col : Nat

  ||| An NCurses Size; pass the constructor rows and then columns.
  ||| Note that this means the _height_ comes first.
  public export
  record Size where
    constructor MkSize
    rows,cols : Nat

  public export
  data OutputCmd : CursesState -> Type where
    PutCh  : Char -> OutputCmd s
    PutStr : (newline : Bool) -> String -> OutputCmd s
    VLine  : Char -> Nat -> OutputCmd s
    HLine  : Char -> Nat -> OutputCmd s
    Move   : Position -> OutputCmd s

namespace Window
  public export
  data IdentifiesWindow : (0 name : String) -> (0 windows : List State.Window) -> Type where
    Here : IdentifiesWindow name ((MkWindow name _ _) :: windows)
    There : IdentifiesWindow name windows -> IdentifiesWindow name (w :: windows)

  public export
  data HasWindow : (0 name : String) -> CursesState -> Type where
    ItHasWindow : IdentifiesWindow name ws => HasWindow name (Active _ ws w _)

  public export
  data InWindow : (0 name : String) -> CursesState -> Type where
    IsCurrentWindow : InWindow name (Active _ _ ((MkWindow name _ _) ** _) _)

  public export %inline
  defaultWindow : String
  defaultWindow = "default"

  public export
  0 lookupWindow : (name : String) -> (ws : List State.Window) -> IdentifiesWindow name ws => State.Window
  lookupWindow name (MkWindow name k d :: windows) @{Here} = (MkWindow name k d)
  lookupWindow name (w :: windows) @{(There elem)} =
    lookupWindow name windows

  public export
  0 lookupWindowPrf : (name : String) -> (ws : List State.Window) -> IdentifiesWindow name ws => Elem (lookupWindow name ws) ws
  lookupWindowPrf name (MkWindow name _ _ :: windows) @{Here} = Here
  lookupWindowPrf name ((MkWindow identifier keypad noDelay) :: ws') @{(There e)} =
    There (lookupWindowPrf name ws')

  public export %inline
  0 unsetWindow : (s : CursesState) -> IsActive s => HasWindow Window.defaultWindow s => CursesState
  unsetWindow (Active i ws _ cs) @{_} @{ItHasWindow @{elem}} =
    Active i ws (lookupWindow defaultWindow ws ** lookupWindowPrf defaultWindow ws) cs

  public export %inline
  0 setWindow : (s : CursesState) -> (name : String) -> HasWindow name s => CursesState
  setWindow (Active i ws _ cs) name @{ItHasWindow @{elem}} =
    Active i ws (lookupWindow name ws ** lookupWindowPrf name ws) cs

