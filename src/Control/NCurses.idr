module Control.NCurses

import NCurses.Core
import NCurses.Core.Color
import NCurses.Core.Attribute
import NCurses.Core.SpecialKey
import NCurses.Core.Input
import public Data.List.Elem
import public NCurses.Core.Color as Color
import public Control.TransitionIndexed
import Control.Monad.State
import Data.DPair
import Data.List
import Decidable.Equality

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
  MkWindow : (identifier : String) -> (keypad : Bool) -> (noDelay : Bool) -> Window

public export
identifier : NCurses.Window -> String
identifier (MkWindow i _ _) = i

public export
(.identifier) : NCurses.Window -> String
(.identifier) = identifier

public export
setKeypad : (on : Bool) -> NCurses.Window -> NCurses.Window
setKeypad on (MkWindow i _ d) = MkWindow i on d

public export
newKeypadSameIdentity : (on : Bool) -> (w : NCurses.Window) -> (identifier w) === (identifier (setKeypad on w))
newKeypadSameIdentity on (MkWindow identifier keypad noDelay) = Refl

public export
setNoDelay : (on : Bool) -> NCurses.Window -> NCurses.Window
setNoDelay on (MkWindow i k _) = MkWindow i k on

public export
newNoDelaySameIdentity : (on : Bool) -> (w : NCurses.Window) -> (identifier w) === (identifier (setNoDelay on w))
newNoDelaySameIdentity on (MkWindow identifier keypad noDelay) = Refl

public export
initWindow : String -> NCurses.Window
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
        -> (0 windows : List NCurses.Window)
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
0 bumpWindow : DPair NCurses.Window (\w => Elem w ws) -> DPair NCurses.Window (\w => Elem w (y :: ws))
bumpWindow (q ** r) = (q ** There r)

namespace CursesState
  public export
  data IsActive : CursesState -> Type where
    ItIsActive : IsActive (Active _ _ _ _)

  public export
  data IsInactive : CursesState -> Type where
    ItIsInactive : IsInactive Inactive

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
  0 swapKeypad : (on : Bool) -> (ws : List NCurses.Window) -> Elem y ws -> List NCurses.Window
  swapKeypad on (y :: xs) Here = setKeypad on y :: xs
  swapKeypad on (y :: xs) (There e) = y :: swapKeypad on xs e

  public export %inline
  0 swapKeypadPrf : (on : Bool) -> (ws : List NCurses.Window) -> (e : Elem y ws) -> Elem (setKeypad on y) (swapKeypad on ws e)
  swapKeypadPrf on (y :: xs) Here = Here
  swapKeypadPrf on (y :: xs) (There x) = There $ swapKeypadPrf on xs x

  public export %inline
  0 swapNoDelay : (on : Bool) -> (ws : List NCurses.Window) -> Elem y ws -> List NCurses.Window
  swapNoDelay on (y :: xs) Here = setNoDelay on y :: xs
  swapNoDelay on (y :: xs) (There e) = y :: swapNoDelay on xs e

  public export %inline
  0 swapNoDelayPrf : (on : Bool) -> (ws : List NCurses.Window) -> (e : Elem y ws) -> Elem (setNoDelay on y) (swapNoDelay on ws e)
  swapNoDelayPrf on (y :: xs) Here = Here
  swapNoDelayPrf on (y :: xs) (There x) = There $ swapNoDelayPrf on xs x

  public export %inline
  0 replaceWindow : {w' : NCurses.Window} -> InputState -> List String -> (ws' : _) -> Elem w' ws' -> CursesState
  replaceWindow i cs ws elem = Active i ws (_ ** elem) cs

  public export %inline
  0 setKeypad : (s : CursesState) -> IsActive s => (on : Bool) -> CursesState
  setKeypad Inactive @{ItIsActive} _ impossible
  setKeypad (Active input ws w cs) on = replaceWindow input cs (swapKeypad on ws (snd w)) (swapKeypadPrf on ws (snd w))

  public export %inline
  0 setNoDelay : (s : CursesState) -> IsActive s => (on : Bool) -> CursesState
  setNoDelay Inactive @{ItIsActive} _ impossible
  setNoDelay (Active input ws w cs) on = replaceWindow input cs (swapNoDelay on ws (snd w)) (swapNoDelayPrf on ws (snd w))

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
    DefaultColors : IsActive s => Attribute s
    Color         : (name : String) -> HasColor name s => Attribute s

  public export
  data AttrCmd : CursesState -> Type where
    SetAttr     : Attribute s -> AttrCmd s
    EnableAttr  : Attribute s -> AttrCmd s
    DisableAttr : Attribute s -> AttrCmd s

namespace Output
  public export
  record Position where
    constructor MkPosition
    row,col : Nat

  public export
  record Size where
    constructor MkSize
    rows,cols : Nat

  public export
  data OutputCmd : CursesState -> Type where
    PutCh  : Char -> OutputCmd s
    PutStr : String -> OutputCmd s
    Move   : Position -> OutputCmd s

namespace Window
  public export
  data IdentifiesWindow : (0 name : String) -> (0 windows : List NCurses.Window) -> Type where
    Here : IdentifiesWindow name ((MkWindow name _ _) :: windows)
    There : IdentifiesWindow name windows -> IdentifiesWindow name (w :: windows)

  public export
  data HasWindow : (0 name : String) -> CursesState -> Type where
    ItHasWindow : IdentifiesWindow name ws => HasWindow name (Active _ ws w _)

  public export %inline
  defaultWindow : String
  defaultWindow = "default"

  public export
  0 lookupWindow : (name : String) -> (ws : List NCurses.Window) -> IdentifiesWindow name ws => NCurses.Window
  lookupWindow name (MkWindow name k d :: windows) @{Here} = (MkWindow name k d)
  lookupWindow name (w :: windows) @{(There elem)} =
    lookupWindow name windows

  public export
  0 lookupWindowPrf : (name : String) -> (ws : List NCurses.Window) -> IdentifiesWindow name ws => Elem (lookupWindow name ws) ws
  lookupWindowPrf name (MkWindow name _ _ :: windows) @{Here} = Here
  lookupWindowPrf name ((MkWindow identifier keypad noDelay) :: ws') @{(There e)} = There (lookupWindowPrf name ws')

  public export %inline
  0 unsetWindow : (s : CursesState) -> IsActive s => HasWindow Window.defaultWindow s => CursesState
  unsetWindow (Active i ws _ cs) @{_} @{ItHasWindow @{elem}} = Active i ws (lookupWindow defaultWindow ws ** lookupWindowPrf defaultWindow ws) cs

  public export %inline
  0 setWindow : (s : CursesState) -> (name : String) -> HasWindow name s => CursesState
  setWindow (Active i ws _ cs) name @{ItHasWindow @{elem}} =
    Active i ws (lookupWindow name ws ** lookupWindowPrf name ws) cs

-- public export
-- data InTy = IChar | IKeyOrChar
-- 
-- public export
-- data In = IMaybe InTy | IYes InTy
-- 
-- public export
-- 0 NextIn : (w : NCurses.Window) -> In
-- NextIn (MkWindow identifier keypad noDelay) =
--   ifThenElse noDelay IMaybe IYes $
--     ifThenElse keypad IKeyOrChar IChar

public export
0 NextIn : (w : NCurses.Window) -> Type
NextIn (MkWindow identifier keypad noDelay) =
  ifThenElse noDelay Maybe id $
    ifThenElse keypad (Either Key Char) Char


-- public export
-- 0 GetNext : In -> Type
-- GetNext (IMaybe IChar) = Maybe Char
-- GetNext (IMaybe IKeyOrChar) = Maybe (Either Key Char)
-- GetNext (IYes IChar) = Char
-- GetNext (IYes IKeyOrChar) = Either Key Char

export
data NCurses : (a : Type) -> CursesState -> (a -> CursesState) -> Type where
  Pure : (x : a) -> NCurses a (fs x) fs
  Bind : NCurses a s1 fs2 -> ((x : a) -> NCurses b (fs2 x) fs3) -> NCurses b s1 fs3

  Init        : IsInactive s => NCurses () s (const NCurses.initState)
  DeInit      : IsActive s => NCurses () s (const Inactive)
  AddWindow   : IsActive s => (name : String) -> Position -> Size -> NCurses () s (const (addWindow s name))
  SetWindow   : IsActive s => (name : String) -> HasWindow name s => NCurses () s (const (setWindow s name))
  UnsetWindow : IsActive s => HasWindow Window.defaultWindow s => NCurses () s (const (unsetWindow s))
  AddColor    : IsActive s => (name : String) -> (fg : Color) -> (bg : Color) -> NCurses () s (const (addColor s name))
  ModAttr     : IsActive s => AttrCmd s -> NCurses () s (const s)
  Clear       : IsActive s => NCurses () s (const s)
  Refresh     : IsActive s => NCurses () s (const s)
  Output      : IsActive s => OutputCmd s -> NCurses () s (const s)
  SetEcho     : IsActive s => (on : Bool) -> NCurses () s (const (setEcho s on))
  SetCBreak   : IsActive s => (on : Bool) -> NCurses () s (const (setCBreak s on))
  SetNoDelay  : IsActive s => (on : Bool) -> NCurses () s (const (setNoDelay s on))
  SetCursor   : IsActive s => CursorVisibility -> NCurses () s (const s)
  SetKeypad   : IsActive s => (on : Bool) -> NCurses () s (const (setKeypad s on))
  GetPos      : IsActive s => NCurses Position s (const s)
  GetSize     : IsActive s => NCurses Size s (const s)
  GetCh       : NCurses (NextIn (fst w)) (Active i ws w cs) (const (Active i ws w cs))

  -- TODO: ideally remove this 'escape hatch' and instead specifically allow
  --       types of IO that are not supported by NCurses directly (like File IO).
  NIO : IO a -> NCurses a s (const s)

public export
TransitionIndexedPointed CursesState NCurses where
  pure = Pure

public export
TransitionIndexedMonad CursesState NCurses where
  bind = Bind

public export
map : (a -> b) -> NCurses a s (const s) -> NCurses b s (const s)
map f x = do
  r <- x
  pure (f r)

public export
(<*>) : NCurses (a -> b) s (const s) -> NCurses a s (const s) -> NCurses b s (const s)
(<*>) x y = do
  f <- x
  r <- y
  pure (f r)

public export
(*>) : NCurses a s (const s) -> NCurses b s (const s) -> NCurses b s (const s)
(*>) x y = map (const id) x <*> y

||| Lift an arbitrary IO operation into NCurses.
||| It's ill-advised to use stdout operations
||| like `putStr` from `IO` while NCurses is
||| active -- NCurses offers its own `putStr`
||| and the standard terminal output will
||| behave undesirably for the most part.
public export
liftIO : IO a -> NCurses a s (const s)
liftIO = NIO

||| Initialize an NCurses session. While this session is active,
||| normal terminal output will not behave the way you expect.
public export
init : IsInactive s => NCurses () s (const NCurses.initState)
init = Init

||| End an NCurses session.
public export
deinit : IsActive s => NCurses () s (const Inactive)
deinit = DeInit

--
-- Window Commands
--

||| Add a window to the NCurses session.
||| You get a default window encompassing the whole terminal screen, but
||| adding windows gives finer control over clearing of parts of the screen,
||| printing with coordinates relative to the smaller windows, etc.
public export
addWindow : IsActive s => (name : String) -> Position -> Size -> NCurses () s (const (addWindow s name))
addWindow = AddWindow

public export
setWindow : IsActive s => (name : String) -> HasWindow name s => NCurses () s (const (setWindow s name))
setWindow = SetWindow

public export
unsetWindow : IsActive s => HasWindow Window.defaultWindow s => NCurses () s (const (unsetWindow s))
unsetWindow = UnsetWindow

||| Clear the screen of the current window.
public export
clear : IsActive s => NCurses () s (const s)
clear = Clear

||| Refresh the current window.
public export
refresh : IsActive s => NCurses () s (const s)
refresh = Refresh

||| Get the current cursor position.
public export
getPos : IsActive s => NCurses Position s (const s)
getPos = GetPos

||| Get the size of the current window.
public export
getSize : IsActive s => NCurses Size s (const s)
getSize = GetSize

--
-- Attribute Commands
--

namespace Attribute
  ||| Add a color to the current NCurses session.
  |||
  ||| Once added, colors can be referenced by name
  ||| when constructing Attributes.
  public export
  addColor : IsActive s => (name : String) -> (fg : Color) -> (bg : Color) -> NCurses () s (const (addColor s name))
  addColor = AddColor

  ||| Set the given attribute until it is set again.
  ||| This has no impact on any other attributes that are set.
  |||
  ||| In ncurses terminology, "attron"
  public export
  enableAttr : IsActive s => Attribute s -> NCurses () s (const s)
  enableAttr = ModAttr . EnableAttr

  ||| Unset the given attribute until it is set again.
  ||| This has no impact on any other attributes that are set.
  |||
  ||| In ncurses terminology, "attroff"
  public export
  disableAttr : IsActive s => Attribute s -> NCurses () s (const s)
  disableAttr = ModAttr . DisableAttr

  ||| Set an attribute to be applied until it is cleared or
  ||| overwritten. All other attributes are cleared at the same time.
  ||| See @enabledAttr@ to enable an attribute without clearing other
  ||| attributes.
  |||
  ||| `setAttr Normal` clears all attributes.
  |||
  ||| In ncurses terminology, "attrset"
  public export
  setAttr : IsActive s => Attribute s -> NCurses () s (const s)
  setAttr = ModAttr . SetAttr

  ||| Set all the given attributes, replacing any existing attributes.
  |||
  ||| In ncurses terminology, "attrset"
  public export
  setAttrs : IsActive s => List (Attribute s) -> NCurses () s (const s)
  setAttrs = -- efficiency note: NCurses offers a one-function call to achieve
             -- this by passing a mask of ORed attributes. We could support
             -- that here in the future.
             foldr (\a,nc => nc >> enableAttr a) (setAttr Normal)

--
-- Output Commands
--

namespace Output
  ||| Print a character to the terminal.
  public export
  putCh : IsActive s => Char -> NCurses () s (const s)
  putCh = Output . PutCh

  ||| Print a string to the terminal _without_ a trailing newline.
  public export
  putStr : IsActive s => String -> NCurses () s (const s)
  putStr = Output . PutStr

  ||| Print a string to the terminal _with_ a trailing newline.
  public export
  putStrLn : IsActive s => String -> NCurses () s (const s)
  putStrLn = Output . PutStr . (++ "\n")

  ||| Move the cursor.
  public export
  move : IsActive s => Position -> NCurses () s (const s)
  move = Output . Move

--
-- Input Commands
--

namespace Input
  ||| cBreak indicates whether characters typed by the user are delivered to the
  ||| program (via @getCh@) immediately or not. If not, they are delivered when a
  ||| newline is typed (the default behavior for most terminal environments).
  |||
  ||| This setting affects all windows.
  public export
  setCBreak : IsActive s => (on : Bool) -> NCurses () s (const (setCBreak s on))
  setCBreak = SetCBreak

  ||| echo indicates whether characters typed by the user are printed to the screen
  ||| by NCurses as well as delivered to @getCh@.
  |||
  ||| This setting affects all windows.
  public export
  setEcho : IsActive s => (on : Bool) -> NCurses () s (const (setEcho s on))
  setEcho = SetEcho

  ||| Turn "keypad" on or off.
  |||
  ||| This property is set independently for each window. This method only sets the
  ||| property for the current window.
  |||
  ||| Set to true to receive "special keys" as single values rather than composites
  ||| of two or more Chars. For example, arrow keys are represented as two input
  ||| chars, but you will receive @Key@ values for them instead with the @keypad@
  ||| property turned on.
  |||
  ||| This is on by default.
  public export
  setKeypad : IsActive s => (on : Bool) -> NCurses () s (const (setKeypad s on))
  setKeypad = SetKeypad

  ||| Set the way the cursor is displayed to the user.
  public export
  setCursor : IsActive s => CursorVisibility -> NCurses () s (const s)
  setCursor = SetCursor

--
-- Test Routine
--

testRoutine : NCurses () Inactive (const Inactive)
testRoutine = TransitionIndexed.Do.do
  init
  addColor "alert" White Red
  setAttr Underline
  setAttr (Color "alert")
  clear >> refresh
  putStr "Hello World"
  setAttr DefaultColors
  putStrLn "back to basics."
  addWindow "win1" (MkPosition 10 10) (MkSize 10 20)
  setWindow "win1"
  unsetWindow
  deinit

--
-- Runtime
--

public export
keys : List (key, value) -> List key
keys [] = []
keys ((x, y) :: xs) = x :: keys xs

keysInjective : {0 x : _} -> keys (x :: xs) = (y :: ys) -> (Builtin.fst x = y, NCurses.keys xs = ys)
keysInjective {x = (w, z)} Refl = (Refl, Refl)

record RuntimeWindow where
  constructor MkRuntimeWindow
  props : NCurses.Window
  win : Core.Window

initRuntimeWindow : String -> Core.Window -> RuntimeWindow
initRuntimeWindow name win = MkRuntimeWindow (initWindow name) win

setRWKeypad : (on : Bool) -> RuntimeWindow -> RuntimeWindow
setRWKeypad on (MkRuntimeWindow (MkWindow identifier keypad noDelay) win) =
  MkRuntimeWindow (MkWindow identifier on noDelay) win

setRWNoDelay : (on : Bool) -> RuntimeWindow -> RuntimeWindow
setRWNoDelay on (MkRuntimeWindow (MkWindow identifier keypad noDelay) win) =
  MkRuntimeWindow (MkWindow identifier keypad on) win

data RuntimeWindows : List NCurses.Window -> Type where
  Nil : RuntimeWindows []
  (::) : (rw : RuntimeWindow) -> RuntimeWindows ws -> RuntimeWindows (rw.props :: ws)

data CurrentWindow : (rw : RuntimeWindow) -> Elem w ws -> RuntimeWindows ws -> Type where
  Here  : CurrentWindow rw Here (rw :: rws)
  There : CurrentWindow rw e rws -> CurrentWindow rw (There e) (other :: rws)

-- TODO: all of these erased proofs are probably suitable for Subset
record CursesActive (0 ws : List NCurses.Window) (0 w : (w' ** Elem w' ws)) (0 cs : List String) where
  constructor MkCursesActive
  windows : RuntimeWindows ws
  currentWindow : (rw ** CurrentWindow rw w.snd windows)
  colors : List (String, ColorPair)
  {0 csPrf : (keys colors) = cs}

data RuntimeCurses : CursesState -> Type where
  RInactive : RuntimeCurses Inactive
  RActive   : CursesActive ws w cs -> RuntimeCurses (Active i ws w cs)

getColor : (colors : List (String, ColorPair))
        -> (0 csPrf : NCurses.keys colors = names)
        -> (elemPrf : Elem _ names)
        -> ColorPair
getColor (z :: zs) csPrf Here = snd z
getColor [] csPrf (There elemPrf) impossible
getColor (z :: zs) csPrf (There elemPrf) = getColor zs (snd $ keysInjective csPrf) elemPrf

getWindow : IdentifiesWindow n ws => (rws : RuntimeWindows ws) -> (rw ** CurrentWindow rw (lookupWindowPrf n ws) rws)
getWindow @{Here} (rw@(MkRuntimeWindow (MkWindow n _ _) win) :: rws') = (rw ** Here)
getWindow @{There e} (rw'@(MkRuntimeWindow (MkWindow identifier keypad noDelay) win) :: rws') = bimap id There $ getWindow @{e} rws'

bumpWindowSameWindow : {0 ws : List NCurses.Window}
                    -> {0 w : DPair NCurses.Window (flip Elem ws)}
                    -> {windows : RuntimeWindows ws}
                    -> {rw : RuntimeWindow}
                    -> {wPrf : CurrentWindow rw (w.snd) windows}
                    -> CurrentWindow rw ((NCurses.bumpWindow w).snd) (MkRuntimeWindow (MkWindow n True False) runtimeWin :: windows)
bumpWindowSameWindow {w = (w' ** elem)} = There wPrf


addRuntimeWindow : {0 ws : List NCurses.Window}
                -> {0 w : DPair NCurses.Window (flip Elem ws)}
                -> (name : String)
                -> (runtimeWin : Core.Window)
                -> CursesActive ws w cs
                -> CursesActive (initWindow name :: ws) (bumpWindow w) cs
addRuntimeWindow identifier runtimeWin (MkCursesActive windows (rw ** wPrf) colors {csPrf = csPrf}) =
  MkCursesActive { windows = initRuntimeWindow identifier runtimeWin :: windows
                 , currentWindow = (rw ** bumpWindowSameWindow {wPrf})
                 , colors
                 , csPrf
                 }

addRuntimeColor : (name : String) -> (cp : ColorPair) -> CursesActive ws w cs -> CursesActive ws w (name :: cs)
addRuntimeColor name cp (MkCursesActive windows currentWindow colors {csPrf}) =
  MkCursesActive { windows
                 , currentWindow
                 , colors = (name, cp) :: colors
                 , csPrf  = cong (name ::) csPrf
                 }

setRuntimeWindow : IdentifiesWindow name ws -> CursesActive ws w cs -> CursesActive ws (lookupWindow name ws ** lookupWindowPrf name ws) cs
setRuntimeWindow elem (MkCursesActive windows currentWindow colors {csPrf}) =
  MkCursesActive { windows
                 , currentWindow = getWindow @{elem} windows
                 , colors
                 , csPrf
                 }

getCoreWindow : CursesActive ws w cs -> Core.Window
getCoreWindow (MkCursesActive _ ((MkRuntimeWindow props win) ** _) _) = win

swapKeypad' : {0 origW : NCurses.Window} -> {0 origWs : List NCurses.Window} -> {0 e : Elem origW origWs} -> (on : Bool) -> {rw : _} -> (rws : RuntimeWindows origWs) -> CurrentWindow rw e rws -> (rws' : RuntimeWindows (swapKeypad on origWs e) ** CurrentWindow (setRWKeypad on rw) (swapKeypadPrf on origWs e) rws')
swapKeypad' on {rw = (MkRuntimeWindow (MkWindow identifier keypad noDelay) _)} ((MkRuntimeWindow (MkWindow identifier keypad noDelay) _) :: rws) Here =
  ((MkRuntimeWindow (MkWindow identifier on noDelay) _) :: rws ** Here)
swapKeypad' on {rw = (MkRuntimeWindow (MkWindow y z w) win)} ((MkRuntimeWindow (MkWindow identifier keypad noDelay) winOther) :: rws') (There {other=(MkRuntimeWindow (MkWindow identifier keypad noDelay) winOther)} x) =
    let (rws'' ** rw') = swapKeypad' on {rw=MkRuntimeWindow (MkWindow y z w) win} rws' x
    in  ((MkRuntimeWindow (MkWindow identifier keypad noDelay) winOther) :: rws'' ** There {other=(MkRuntimeWindow (MkWindow identifier keypad noDelay) winOther)} rw')

setRuntimeKeypad : (on : Bool) -> RuntimeCurses (Active i ws w cs) -> RuntimeCurses (setKeypad (Active i ws w cs) on)
setRuntimeKeypad on (RActive (MkCursesActive windows (rw ** wPrf) colors {csPrf})) = 
  let (windows' ** rw') = swapKeypad' on windows wPrf
  in
  RActive $
    MkCursesActive { windows = windows'
                   , currentWindow = (setRWKeypad on rw ** rw')
                   , colors
                   , csPrf
                   }

swapNoDelay' : {0 origW : NCurses.Window} -> {0 origWs : List NCurses.Window} -> {0 e : Elem origW origWs} -> (on : Bool) -> {rw : _} -> (rws : RuntimeWindows origWs) -> CurrentWindow rw e rws -> (rws' : RuntimeWindows (swapNoDelay on origWs e) ** CurrentWindow (setRWNoDelay on rw) (swapNoDelayPrf on origWs e) rws')
swapNoDelay' on {rw = (MkRuntimeWindow (MkWindow identifier keypad noDelay) _)} ((MkRuntimeWindow (MkWindow identifier keypad noDelay) _) :: rws) Here =
  ((MkRuntimeWindow (MkWindow identifier keypad on) _) :: rws ** Here)
swapNoDelay' on {rw = (MkRuntimeWindow (MkWindow y z w) win)} ((MkRuntimeWindow (MkWindow identifier keypad noDelay) winOther) :: rws') (There {other=(MkRuntimeWindow (MkWindow identifier keypad noDelay) winOther)} x) =
    let (rws'' ** rw') = swapNoDelay' on {rw=MkRuntimeWindow (MkWindow y z w) win} rws' x
    in  ((MkRuntimeWindow (MkWindow identifier keypad noDelay) winOther) :: rws'' ** There {other=(MkRuntimeWindow (MkWindow identifier keypad noDelay) winOther)} rw')

setRuntimeNoDelay : (on : Bool) -> RuntimeCurses (Active i ws w cs) -> RuntimeCurses (setNoDelay (Active i ws w cs) on)
setRuntimeNoDelay on (RActive (MkCursesActive windows (rw ** wPrf) colors {csPrf})) =
  let (windows' ** rw') = swapNoDelay' on windows wPrf
  in
  RActive $
    MkCursesActive { windows = windows'
                   , currentWindow = (setRWNoDelay on rw ** rw')
                   , colors
                   , csPrf
                   }

||| Extract a safe attribute in the given state into
||| a core attribute.
coreAttr : RuntimeCurses s -> Attribute s -> Attribute
coreAttr _ Normal    = Normal
coreAttr _ Underline = Underline
coreAttr _ Standout  = Standout
coreAttr _ Reverse   = Reverse
coreAttr _ Blink     = Blink
coreAttr _ Dim       = Dim
coreAttr _ Bold      = Bold
coreAttr _ Protected = Protected
coreAttr _ Invisible = Invisible
coreAttr _ DefaultColors = CP defaultColorPair
coreAttr (RActive (MkCursesActive _ _ colors {csPrf})) (Color name @{ItHasColor @{elem}}) =
  let color = getColor colors csPrf elem
  in  CP color

modNCursesAttr : HasIO io =>
                 IsActive s =>
                 AttrCmd s
              -> RuntimeCurses s
              -> io (RuntimeCurses s)
modNCursesAttr (SetAttr attr) rs@(RActive as)     = nSetAttr'     (getCoreWindow as) (coreAttr rs attr) $> rs
modNCursesAttr (EnableAttr attr) rs@(RActive as)  = nEnableAttr'  (getCoreWindow as) (coreAttr rs attr) $> rs
modNCursesAttr (DisableAttr attr) rs@(RActive as) = nDisableAttr' (getCoreWindow as) (coreAttr rs attr) $> rs

printNCurses : HasIO io =>
               IsActive s =>
               OutputCmd s
            -> RuntimeCurses s
            -> io (RuntimeCurses s)
printNCurses (Move (MkPosition row col)) rs@(RActive as) = nMoveCursor' (getCoreWindow as) row col $> rs
printNCurses (PutStr str) rs@(RActive as)   = nPutStr' (getCoreWindow as) str $> rs
printNCurses (PutCh ch) rs@(RActive as)     = nPutCh'  (getCoreWindow as) ch  $> rs

runNCurses : HasIO io => NCurses a s fs -> RuntimeCurses s -> io (x : a ** RuntimeCurses (fs x))
runNCurses (Pure x) rs = pure (x ** rs)
runNCurses (Bind x f) rs = do
  (x' ** rs') <- runNCurses x rs
  runNCurses (f x') rs'
runNCurses (NIO ops) rs = do
  res <- liftIO ops
  pure (res ** rs)
----
runNCurses Init RInactive = Prelude.do
  initNCurses
  cBreak
  noEcho
  keypad True
  noDelay False
  win <- stdWindow
  pure (() ** RActive $ MkCursesActive [initRuntimeWindow Window.defaultWindow win] (initRuntimeWindow Window.defaultWindow win ** Here) [] {csPrf=Refl})
runNCurses DeInit (RActive _) = do
  deinitNCurses
  pure (() ** RInactive)
----
runNCurses (AddWindow @{isActive} name pos size) (RActive as) = Prelude.do
  runtimeWin <- newWindow size.rows size.cols pos.row pos.col
  keypad' runtimeWin True
  noDelay' runtimeWin False
  let as' = addRuntimeWindow name runtimeWin as
  pure (() ** RActive as')
runNCurses (SetWindow   @{_} name @{ItHasWindow @{elem}}) (RActive as) = pure (() ** RActive $ setRuntimeWindow elem as)
runNCurses (UnsetWindow @{_}      @{ItHasWindow @{elem}}) (RActive as) = pure (() ** RActive $ setRuntimeWindow elem as)
----
runNCurses (AddColor name fg bg) (RActive as) = do
  let nextIdx = length as.colors
  when (nextIdx == 0) startColor
  cp <- initColorPair nextIdx fg bg
  let as' = addRuntimeColor name cp as
  pure (() ** RActive as')
runNCurses (ModAttr cmd) rs = do
  rs' <- modNCursesAttr cmd rs
  pure (() ** rs')
runNCurses Clear   rs@(RActive as) = clear'   (getCoreWindow as) $> (() ** rs)
runNCurses Refresh rs@(RActive as) = refresh' (getCoreWindow as) $> (() ** rs)
runNCurses (Output cmd) rs = do
  rs' <- printNCurses cmd rs
  pure (() ** rs')
runNCurses GetPos rs@(RActive as) = do
  let win = getCoreWindow as
  pure (MkPosition !(getYPos' win) !(getXPos' win) ** rs)
runNCurses GetSize rs@(RActive as) = do
  size <- (uncurry MkSize) <$> getMaxSize' (getCoreWindow as)
  pure (size ** rs)
runNCurses (SetEcho on) (RActive as) = (ifThenElse on echo noEcho) $> (() ** RActive as)
runNCurses (SetCBreak on) (RActive as) = (ifThenElse on cBreak noCBreak) $> (() ** RActive as)
runNCurses (SetKeypad on) (RActive as) = keypad' (getCoreWindow as) on $> (() ** setRuntimeKeypad on $ RActive as)
runNCurses (SetNoDelay on) (RActive as) = noDelay' (getCoreWindow as) on $> (() ** setRuntimeNoDelay on $ RActive as)
runNCurses (SetCursor c) rs = setCursorVisibility c $> (() ** rs)
-- TODO: prove that w.fst = rw.props for below:
runNCurses GetCh (RActive (MkCursesActive windows (rw ** wPrf) colors)) = ?h

||| Run an NCurses program with guarantees
||| that it is initialized at the beginning and
||| deinitialied at the end.
export
withNCurses : HasIO io => NCurses a Inactive (const Inactive) -> io a
withNCurses nc =
  evalStateT RInactive $
    lift $ fst <$> (runNCurses nc RInactive)

testProgram : HasIO io => io ()
testProgram = withNCurses testRoutine

