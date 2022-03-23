module Control.NCurses

import NCurses.Core
import NCurses.Core.Attribute

import Control.Monad.State
import Data.Either
import Data.List
import Data.Maybe
import Data.Nat
import Data.String

import public NCurses.Core.Color as Color
import public NCurses.Core.Input as Input
import public NCurses.Core.SpecialKey as SpecialKey
import public Control.NCurses.State

%default total

public export
0 NextIn : (w : State.Window) -> Type
NextIn w =
  ifThenElse w.noDelay Maybe id $
    ifThenElse w.keypad (Either Char Key) Char

export
data NCurses : (a : Type) -> CursesState -> CursesState -> Type where
  Pure : (x : a) -> NCurses a s s
  Bind : NCurses a s1 s2 -> ((x : a) -> NCurses b s2 s3) -> NCurses b s1 s3

  Init        : IsInactive s => NCurses () s State.initState
  DeInit      : IsActive s => NCurses () s Inactive
  AddWindow   : IsActive s => (name : String) -> Position -> Size -> Maybe (Exists (\c => (Border c, HasColor c s))) -> NCurses () s (addWindow s name)
  SetWindow   : IsActive s => (name : String) -> HasWindow name s => NCurses () s (setWindow s name)
  UnsetWindow : IsActive s => HasWindow DefaultWindow s => NCurses () s (setWindow s DefaultWindow)
  InWindow    : IsActive s => (w : String) -> HasWindow w s => NCurses () (setWindow s w) (setWindow s w) -> NCurses () s s
  AddColor    : IsActive s => (name : String) -> (fg : Color) -> (bg : Color) -> NCurses () s (addColor s name)
  ModAttr     : IsActive s => AttrCmd s -> NCurses () s s
  Clear       : IsActive s => NCurses () s s
  Erase       : IsActive s => NCurses () s s
  Refresh     : IsActive s => NCurses () s s
  RefreshAll  : IsActive s => NCurses () s s
  Output      : IsActive s => OutputCmd s -> NCurses () s s
  SetEcho     : IsActive s => (on : Bool) -> NCurses () s (setEcho s on)
  SetCBreak   : IsActive s => (on : Bool) -> NCurses () s (setCBreak s on)
  SetNoDelay  : IsActive s => (on : Bool) -> NCurses () s (setNoDelay s on)
  SetCursor   : IsActive s => CursorVisibility -> NCurses () s s
  SetKeypad   : IsActive s => (on : Bool) -> NCurses () s (setKeypad s on)
  GetPos      : IsActive s => NCurses Position s s
  SetPos      : IsActive s => Position -> NCurses () s s
  ||| Get the size of the current window. If `internal` is @True@, then
  ||| will subtract the space taken up by any border a window has.
  GetSize     : IsActive s => (internal : Bool) -> NCurses Size s s
  SetSize     : IsActive s => Size -> NCurses () s s
  GetCh       : IsActive s => NCurses (NextIn (currentWindow s)) s s

  -- TODO: ideally remove this 'escape hatch' and instead specifically allow
  --       types of IO that are not supported by NCurses directly (like File IO).
  NIO : IO a -> NCurses a s s

public export
IndexedFunctor CursesState CursesState NCurses where
  map f x = Bind x (\y => Pure (f y))

public export
IndexedApplicative CursesState NCurses where
  pure = Pure

  ap f x = Bind f (\f' => Bind x (\y => Pure (f' y)))

public export
IndexedMonad CursesState NCurses where
  bind = Bind

||| Lift an arbitrary IO operation into NCurses.
||| It's ill-advised to use stdout operations
||| like `putStr` from `IO` while NCurses is
||| active -- NCurses offers its own `putStr`
||| and the standard terminal output will
||| behave undesirably for the most part.
export
liftIO : IO a -> NCurses a s s
liftIO = NIO

||| Initialize an NCurses session. While this session is active,
||| normal terminal output will not behave the way you expect.
export
init : IsInactive s => NCurses () s State.initState
init = Init

||| End an NCurses session.
export
deinit : IsActive s => NCurses () s Inactive
deinit = DeInit

--
-- Window Commands
--

||| Add a window to the NCurses session.
||| You get a default window encompassing the whole terminal screen, but
||| adding windows gives finer control over clearing of parts of the screen,
||| printing with coordinates relative to the smaller windows, etc.
|||
||| See the @border@ function for a convenient initializer of the @Border@ type.
export
addWindow : IsActive s =>
            (name : String)
         -> Position
         -> Size
         -> Maybe (Exists (\c => (Border c, HasColor c s)))
         -> NCurses () s (addWindow s name)
addWindow = AddWindow

||| Create a border for a window given the colors available in the current
||| state.
public export
border : IsActive s => 
         (color : String)
      -> HasColor color s =>
         (left   : BorderChar)
      -> (right  : BorderChar)
      -> (top    : BorderChar)
      -> (bottom : BorderChar)
      -> (topLeft     : BorderChar)
      -> (topRight    : BorderChar)
      -> (bottomLeft  : BorderChar)
      -> (bottomRight : BorderChar)
      -> Maybe (Exists (\c => (Border c, HasColor c s)))
border @{_} color @{hasColor} left right top bottom topLeft topRight bottomLeft bottomRight =
  Just $
    Evidence color $
      ( MkBorder { left
                 , right
                 , top
                 , bottom
                 , topLeft
                 , topRight
                 , bottomLeft
                 , bottomRight
                 }
      , hasColor)

public export
defaultBorder : IsActive s => 
                (color : String)
             -> HasColor color s =>
                Maybe (Exists (\c => (Border c, HasColor c s)))
defaultBorder color =
  border color Default Default Default Default
               Default Default Default Default
               {s}

||| Set the current window. This is the window that future commands
||| apply to (drawing, setting attributes, reading characters from).
export
setWindow : IsActive s => (name : String) -> HasWindow name s => NCurses () s (setWindow s name)
setWindow = SetWindow

||| Unset the current window. This results in the full screen (also known
||| as the default or standard window) being "current."
export
unsetWindow : IsActive s => HasWindow DefaultWindow s => NCurses () s (setWindow s DefaultWindow)
unsetWindow = UnsetWindow

||| Perform some operations within another window, returning to the current window without
||| modifying state afterwards.
export
inWindow : IsActive s => (name : String) -> HasWindow name s => NCurses () (setWindow s name) (setWindow s name) -> NCurses () s s
inWindow = InWindow
-- ^ NOTE: This _should_ be possible to do provably without creating a new @InWindow@ constructor just by
--         using @SetWindow@ twice but I have yet to get the proofs to work.

public export
%hint
hasWindowWithin : InWindow w s => HasWindow w s
hasWindowWithin {s = (Active _ _ ((MkWindow name _ _) ** Here) _)} @{IsCurrentWindow} = ItHasWindow
hasWindowWithin {s = (Active _ _ ((MkWindow name _ _) ** (There x)) _)} @{IsCurrentWindow} = hasWindowWithin

public export
%hint
identifiesCurrentWindow : InWindow w (Active _ ws _ _) => IdentifiesWindow w ws
identifiesCurrentWindow {ws} @{p} with (hasWindowWithin @{p})
  identifiesCurrentWindow {ws} @{p} | (ItHasWindow @{ident}) = ident

||| If a given state has a window, setting a new current window on that state does not
||| change the fact that the state has the original window.
public export
%hint
setWindowHasWindowStill : HasWindow n s => HasWindow w s => HasWindow n (setWindow s w)
setWindowHasWindowStill {s = (Active _ (MkWindow n _ _ :: ws) _ _)} @{ItHasWindow  @{Here}} @{ItHasWindow} = ItHasWindow
setWindowHasWindowStill {s = Active _ [] _ _} @{ItHasWindow  @{Here}} impossible
setWindowHasWindowStill {s = (Active _ (y :: ws) _ _)} @{ItHasWindow  @{There x}} @{ItHasWindow} = ItHasWindow @{There x}
setWindowHasWindowStill {s = Active _ [] _ _} @{ItHasWindow  @{There x}} impossible

public export
%hint
setWindowIsActiveStill : IsActive s => HasWindow w s => IsActive (setWindow s w)
setWindowIsActiveStill @{ItIsActive} @{ItHasWindow} = ItIsActive

public export
%hint
setWindowHasColorStill : HasColor c s => HasWindow w s => HasColor c (setWindow s w)
setWindowHasColorStill {s = (Active _ _ _ (c :: xs))} @{ItHasColor  @{Here}} @{ItHasWindow} = ItHasColor
setWindowHasColorStill {s = (Active _ _ _ (y :: xs))} @{ItHasColor  @{(There x)}} @{ItHasWindow} = ItHasColor

||| If a given state has a window, setting a new current window on that state does not
||| change the fact that the state has the original window.
public export
%hint
addWindowHasWindowStill : IsActive s => HasWindow n s => HasWindow n (addWindow s w)
addWindowHasWindowStill {s = (Active _ (_ :: ws) _ _)} @{ItIsActive} @{ItHasWindow @{Here}} = ItHasWindow
addWindowHasWindowStill {s = (Active _ (_ :: ws) _ _)} @{ItIsActive} @{ItHasWindow @{(There x)}} = ItHasWindow

public export
%hint
addWindowIsActiveStill : IsActive s => IsActive (addWindow s w)
addWindowIsActiveStill @{ItIsActive} = ItIsActive

public export
%hint
addWindowHasColorStill : IsActive s => HasColor c s => HasColor c (addWindow s w)
addWindowHasColorStill {s = (Active _ _ _ (c :: xs))} @{ItIsActive} @{ItHasColor @{Here}} = ItHasColor
addWindowHasColorStill {s = (Active _ _ _ (y :: xs))} @{ItIsActive} @{ItHasColor @{(There x)}} = ItHasColor

public export
identifiedWindowExists : IdentifiesWindow w ws -> Exists (\k => Exists (\d => lookupWindow w ws = MkWindow w k d))
identifiedWindowExists {ws = (MkWindow _ _ _ :: windows)} Here = Evidence _ (Evidence _ Refl)
identifiedWindowExists {ws = ((MkWindow identifier keypad noDelay) :: windows)} (There x) = identifiedWindowExists x

public export
%hint
inWindowNow : HasWindow w s => InWindow w (setWindow s w)
inWindowNow @{ItHasWindow @{p}} with (identifiedWindowExists p)
  inWindowNow @{ItHasWindow  @{p}} | (Evidence k (Evidence d prf)) = rewrite prf in IsCurrentWindow

||| Clear the current window.
|||
||| Clearing tells NCurses to redraw the whole terminal on the next
||| refresh. For a less intensive process that allows NCurses
||| to determine what parts of the terminal need to be drawn,
||| use @erase@.
|||
||| Clearing the standard window will require refreshing all
||| windows after the next call to refresh the standard window,
||| It probably makes sense much of the time to refresh immediately
||| after a call to clear.
export
clear : IsActive s => NCurses () s s
clear = Clear

||| Erase the current window.
export
erase : IsActive s => NCurses () s s
erase = Erase

||| Refresh the current window.
export
refresh : IsActive s => NCurses () s s
refresh = Refresh

||| Refresh all windows.
export
refreshAll : IsActive s => NCurses () s s
refreshAll = RefreshAll

||| Get the cursor position within the current window.
export
getPos : IsActive s => NCurses Position s s
getPos = GetPos

||| Set the position of the current window.
export
setPos : IsActive s => Position -> NCurses () s s
setPos = SetPos

||| Get the size of the current window.
|||
||| @ internal If @True@, the size will not include the space
|||            taken up by any internal border the window has
|||            applied to it. If @False@, the size will always
|||            be the space taken up by the window, not the space
|||            available inside the window.
export
getSize : IsActive s => (internal : Bool) -> NCurses Size s s
getSize = GetSize

||| Set the size of the current window. If the window has a border,
||| this size includes the single row & column taken up by the border
||| on all sides.
export
setSize : IsActive s => Size -> NCurses () s s
setSize = SetSize

--
-- Attribute Commands
--

namespace Attribute
  ||| Add a color to the current NCurses session.
  |||
  ||| Once added, colors can be referenced by name
  ||| when constructing Attributes.
  export
  addColor : IsActive s =>
             (name : String)
          -> (fg : Color)
          -> (bg : Color)
          -> NCurses () s (addColor s name)
  addColor = AddColor

  ||| Set the given attribute until it is set again.
  ||| This has no impact on any other attributes that are set.
  |||
  ||| In ncurses terminology, "attron"
  export
  enableAttr : IsActive s => Attribute s -> NCurses () s s
  enableAttr = ModAttr . EnableAttr

  ||| Unset the given attribute until it is set again.
  ||| This has no impact on any other attributes that are set.
  |||
  ||| In ncurses terminology, "attroff"
  export
  disableAttr : IsActive s => Attribute s -> NCurses () s s
  disableAttr = ModAttr . DisableAttr

  ||| Set an attribute to be applied until it is cleared or
  ||| overwritten. All other attributes are cleared at the same time.
  ||| See @enabledAttr@ to enable an attribute without clearing other
  ||| attributes.
  |||
  ||| `setAttr Normal` clears all attributes.
  |||
  ||| In ncurses terminology, "attrset"
  export
  setAttr : IsActive s => Attribute s -> NCurses () s s
  setAttr = ModAttr . SetAttr

  ||| Set all the given attributes, replacing any existing attributes.
  |||
  ||| In ncurses terminology, "attrset"
  export
  setAttrs : IsActive s => List (Attribute s) -> NCurses () s s
  setAttrs = -- efficiency note: NCurses offers a one-function call to achieve
             -- this by passing a mask of ORed attributes. We could support
             -- that here in the future.
             foldr (\a,nc => nc >> enableAttr a) (setAttr Normal)

  ||| Update the attribute (only supports a single attribute for now) and color
  ||| at the current position for the given length of characters. This allows
  ||| you to change attributes of already printed characters.
  |||
  ||| Specify a length of @Nothing@ to update the whole line.
  export
  updateAttr : IsActive s => Attribute s -> ColorAttr s -> (length : Maybe Nat) -> NCurses () s s
  updateAttr attr color length = ModAttr (UpdateAttr attr color length)

--
-- Output Commands
--

namespace Output
  ||| Print a character to the terminal.
  export
  putCh : IsActive s => Char -> NCurses () s s
  putCh = Output . PutCh

  ||| Print a string to the terminal _without_ a trailing newline.
  export
  putStr : IsActive s => String -> NCurses () s s
  putStr = Output . PutStr False

  ||| Print a string to the terminal _with_ a trailing newline.
  export
  putStrLn : IsActive s => String -> NCurses () s s
  putStrLn = Output . PutStr True

  ||| Draw a vertical line to the current window comprised of the given
  ||| character and having the given length.
  export
  drawVerticalLine : IsActive s => Char -> Nat -> NCurses () s s
  drawVerticalLine = Output .: VLine

  ||| Draw a horizontal line to the current window comprised of the given
  ||| character and having the given length.
  export
  drawHorizontalLine : IsActive s => Char -> Nat -> NCurses () s s
  drawHorizontalLine = Output .: HLine

  ||| Move the cursor to a position relative within the current window.
  |||
  ||| If the window has a border, this position is _within_ the border.
  ||| (0, 0) is the top left (row, column) without a border and with a
  ||| border it is 1 row & column inset from there.
  export
  move : IsActive s => Position -> NCurses () s s
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
  export
  setCBreak : IsActive s => (on : Bool) -> NCurses () s (setCBreak s on)
  setCBreak = SetCBreak

  ||| echo indicates whether characters typed by the user are printed to the screen
  ||| by NCurses as well as delivered to @getCh@.
  |||
  ||| This setting affects all windows.
  export
  setEcho : IsActive s => (on : Bool) -> NCurses () s (setEcho s on)
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
  export
  setKeypad : IsActive s => (on : Bool) -> NCurses () s (setKeypad s on)
  setKeypad = SetKeypad

  ||| Turn "noDelay" on or off.
  |||
  ||| This property is set independently for each window. This method only sets the
  ||| property for the current window.
  |||
  ||| This is off by default (i.e. by default @getInput@ waits for user input).
  export
  setNoDelay : IsActive s => (on : Bool) -> NCurses () s (setNoDelay s on)
  setNoDelay = SetNoDelay

  ||| Set the way the cursor is displayed to the user.
  export
  setCursor : IsActive s => CursorVisibility -> NCurses () s s
  setCursor = SetCursor
  
  ||| Get the next keypress or character input.
  |||
  ||| If `noDelay` is turned on, @getInput@ returns immediately and results in
  ||| @Nothing@ if the user has not typed anything. If `noDelay` is turned off,
  ||| @getInput@ blocks until the user enters text. This will block until the next
  ||| character is pressed by default, but if `cBreak` is turned off, it will block
  ||| until the user inputs a newline (much like the default for most terminal input).
  |||
  ||| If `keypad` is turned on, @getInput@ returns either a @Char@ or a @Key@ depending
  ||| on whether the user has input one of the special-keys (like the arrow keys).
  export
  getInput : IsActive s => NCurses (NextIn (currentWindow s)) s s
  getInput = GetCh

  -- One would ideally not need to write the following (just use the `getInput` above)
  -- but in reality it provides a better user experience for downstream development if
  -- functions can be typed as `IsActive s => IsKeypad s => NoDelay s => NCurses () s s)
  -- which does not provide Idris enough information at compile time to evaluate @NextIn@, but
  -- it does allow for the following functions to be used.
  namespace Char
    export
    getChar : IsActive s =>
              NoKeypad s =>
              YesDelay s =>
              NCurses Char s s
    getChar @{act} @{ItIsNoKeypad} @{ItIsDelay} = GetCh

    namespace NotDelayed
      export
      getChar : IsActive s =>
                NoKeypad s =>
                NoDelay  s =>
                NCurses (Maybe Char) s s
      getChar @{act} @{ItIsNoKeypad} @{ItIsNoDelay} = GetCh

  namespace Keypad
    export
    getKeyOrChar : IsActive  s =>
                   YesKeypad s =>
                   YesDelay  s =>
                   NCurses (Either Char Key) s s
    getKeyOrChar @{act} @{ItIsKeypad} @{ItIsDelay} = GetCh

    namespace NotDelayed
      export
      getKeyOrChar : IsActive  s =>
                     YesKeypad s =>
                     NoDelay   s =>
                     NCurses (Maybe (Either Char Key)) s s
      getKeyOrChar @{act} @{ItIsKeypad} @{ItIsNoDelay} = GetCh

--
-- Test Routine
--

testRoutine : NCurses () Inactive Inactive
testRoutine = Indexed.Do.do
  init
  insideWindow DefaultWindow
  addColor "alert" White Red
  setAttr Underline
  setAttr (Color (Named "alert"))
  clear >> refresh
  putStr "Hello World"
  setAttr (Color DefaultColors)
  putStrLn "back to basics."
  addWindow "win1" (MkPosition 10 10) (MkSize 10 20) Nothing
  setWindow "win1"
  insideWindow "win1"
  inp <- getInput
  putChIfPossible inp
  unsetWindow
  addWindow "win2" (MkPosition 0 0) (MkSize 10 10) (defaultBorder "alert")
  setWindow "win2"
  insideWindow "win2"
  unsetWindow -- back to DefaultWindow
  inWindow "win2" (insideWindowTwice "win1") -- inside inWindow
  insideWindow DefaultWindow
  deinit
    where
      putChIfPossible : IsActive s => Either Char Key -> NCurses () s s
      putChIfPossible (Left x) = putCh x
      putChIfPossible (Right _) = pure ()

      getAndPut : IsActive s => YesDelay s => YesKeypad s => NCurses () s s
      getAndPut = do
        inp <- getKeyOrChar
        putChIfPossible inp

      insideWindow : IsActive s => (w : String) -> InWindow w s => NCurses () s s
      insideWindow _ = do
        erase
        putStr "hello"
        refresh

      insideWindowTwice : IsActive s => InWindow "win2" s => (w : _) -> HasWindow w s => NCurses () s s
      insideWindowTwice w = do
        erase
        inWindow w (insideWindow w @{setWindowIsActiveStill} @{inWindowNow})
        refresh

--
-- Runtime
--

public export
keys : List (key, value) -> List key
keys [] = []
keys ((x, y) :: xs) = x :: keys xs

keysInjective : {0 x : _} -> keys (x :: xs) = (y :: ys) -> (Builtin.fst x = y, NCurses.keys xs = ys)
keysInjective {x = (w, z)} Refl = (Refl, Refl)

RuntimeBorder : Type
RuntimeBorder = (ColorPair, Exists (\c => Border c))

record RuntimeWindow where
  constructor MkRuntimeWindow
  props  : State.Window
  border : Maybe RuntimeBorder
  win    : Core.Window

initRuntimeWindow : String -> Maybe RuntimeBorder -> Core.Window -> RuntimeWindow
initRuntimeWindow name border win = MkRuntimeWindow (initWindow name) border win

setRWKeypad : (on : Bool) -> RuntimeWindow -> RuntimeWindow
setRWKeypad on (MkRuntimeWindow (MkWindow identifier keypad noDelay) border win) =
  MkRuntimeWindow (MkWindow identifier on noDelay) border win

setRWNoDelay : (on : Bool) -> RuntimeWindow -> RuntimeWindow
setRWNoDelay on (MkRuntimeWindow (MkWindow identifier keypad noDelay) border win) =
  MkRuntimeWindow (MkWindow identifier keypad on) border win

data RuntimeWindows : List State.Window -> Type where
  Nil : RuntimeWindows []
  (::) : (rw : RuntimeWindow) -> RuntimeWindows ws -> RuntimeWindows (rw.props :: ws)

data CurrentWindow : (rw : RuntimeWindow) -> Elem w ws -> RuntimeWindows ws -> Type where
  Here  : CurrentWindow rw Here (rw :: rws)
  There : CurrentWindow rw e rws -> CurrentWindow rw (There e) (other :: rws)

0 currentWindowPropsPrf : (e : Elem w ws) -> CurrentWindow rw e rws -> rw.props = w
currentWindowPropsPrf Here Here = Refl
currentWindowPropsPrf (There e) (There x) = currentWindowPropsPrf e x

-- TODO: all of these erased proofs are probably suitable for Subset
record CursesActive (0 ws : List State.Window) (0 w : (w' ** Elem w' ws)) (0 cs : List String) where
  constructor MkCursesActive
  windows : RuntimeWindows ws
  currentWindow : (rw ** CurrentWindow rw w.snd windows)
  colors : List (String, ColorPair)
  {0 csPrf : (keys colors) = cs}
  currentColor : Maybe ColorPair
  keyMap : List (Char, Key)

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

getWindow : IdentifiesWindow n ws =>
            (rws : RuntimeWindows ws)
         -> (rw ** CurrentWindow rw (lookupWindowPrf n ws) rws)
getWindow @{Here} (rw@(MkRuntimeWindow (MkWindow n _ _) b win) :: rws') = (rw ** Here)
getWindow @{There e} (rw'@(MkRuntimeWindow (MkWindow identifier keypad noDelay) border win) :: rws') =
  bimap id There $ getWindow @{e} rws'

bumpWindowSameWindow : {0 ws : List State.Window}
                    -> {0 w : DPair State.Window (flip Elem ws)}
                    -> {windows : RuntimeWindows ws}
                    -> {rw : RuntimeWindow}
                    -> {wPrf : CurrentWindow rw (w.snd) windows}
                    -> CurrentWindow rw ((State.bumpWindow w).snd) (MkRuntimeWindow (MkWindow n True False) b runtimeWin :: windows)
bumpWindowSameWindow {w = (w' ** elem)} = There wPrf

addRuntimeWindow : {0 ws : List State.Window}
                -> {0 w : DPair State.Window (flip Elem ws)}
                -> (name : String)
                -> (border : Maybe RuntimeBorder)
                -> (runtimeWin : Core.Window)
                -> CursesActive ws w cs
                -> CursesActive (initWindow name :: ws) (bumpWindow w) cs
addRuntimeWindow identifier border runtimeWin (MkCursesActive windows (rw ** wPrf) colors currentColor keyMap {csPrf = csPrf}) =
  MkCursesActive { windows = initRuntimeWindow identifier border runtimeWin :: windows
                 , currentWindow = (rw ** bumpWindowSameWindow {wPrf})
                 , colors
                 , csPrf
                 , currentColor
                 , keyMap
                 }

runtimeBorder : RuntimeCurses (Active i ws w cs)
             -> Exists (\c => (Border c, HasColor c (Active i ws w cs)))
             -> RuntimeBorder
runtimeBorder rs@(RActive as) (Evidence color (border, (ItHasColor @{elem}))) =
  let cp = getColor as.colors as.csPrf elem
  in  (cp, Evidence color border)

addRuntimeColor : (name : String)
               -> (cp : ColorPair)
               -> CursesActive ws w cs
               -> CursesActive ws w (name :: cs)
addRuntimeColor name cp (MkCursesActive windows currentWindow colors currentColor {csPrf} keyMap) =
  MkCursesActive { windows
                 , currentWindow
                 , colors = (name, cp) :: colors
                 , csPrf  = cong (name ::) csPrf
                 , currentColor
                 , keyMap
                 }

setRuntimeWindow : IdentifiesWindow name ws
                -> CursesActive ws w cs
                -> CursesActive ws (lookupWindow name ws ** lookupWindowPrf name ws) cs
setRuntimeWindow elem (MkCursesActive windows currentWindow colors {csPrf} currentColor keyMap) =
  MkCursesActive { windows
                 , currentWindow = getWindow @{elem} windows
                 , colors
                 , csPrf
                 , currentColor
                 , keyMap
                 }

getCoreWindow : CursesActive ws w cs -> Core.Window
getCoreWindow (MkCursesActive _ ((MkRuntimeWindow props border win) ** _) _ _ _) = win

getCoreWindow' : IsActive s => RuntimeCurses s -> Core.Window
getCoreWindow' (RActive as) = getCoreWindow as

swapKeypad' : {0 origW : State.Window}
           -> {0 origWs : List State.Window}
           -> {0 e : Elem origW origWs}
           -> (on : Bool)
           -> {rw : _}
           -> (rws : RuntimeWindows origWs)
           -> CurrentWindow rw e rws
           -> (rws' : RuntimeWindows (swapKeypad on origWs e) ** CurrentWindow (setRWKeypad on rw) (swapKeypadPrf on origWs e) rws')
swapKeypad' on {rw = (MkRuntimeWindow (MkWindow identifier keypad noDelay) b _)} ((MkRuntimeWindow (MkWindow identifier keypad noDelay) b _) :: rws) Here =
  ((MkRuntimeWindow (MkWindow identifier on noDelay) b _) :: rws ** Here)
swapKeypad' on {rw = (MkRuntimeWindow (MkWindow y z w) b win)} ((MkRuntimeWindow (MkWindow identifier keypad noDelay) border winOther) :: rws') (There {other=(MkRuntimeWindow (MkWindow identifier keypad noDelay) border winOther)} x) =
    let (rws'' ** rw') = swapKeypad' on {rw=MkRuntimeWindow (MkWindow y z w) b win} rws' x
    in  ((MkRuntimeWindow (MkWindow identifier keypad noDelay) border winOther) :: rws'' ** There {other=(MkRuntimeWindow (MkWindow identifier keypad noDelay) border winOther)} rw')

setRuntimeKeypad : (on : Bool)
                -> RuntimeCurses (Active i ws w cs)
                -> RuntimeCurses (setKeypad (Active i ws w cs) on)
setRuntimeKeypad on (RActive (MkCursesActive windows (rw ** wPrf) colors {csPrf} currentColor keyMap)) = 
  let (windows' ** rw') = swapKeypad' on windows wPrf
  in
  RActive $
    MkCursesActive { windows = windows'
                   , currentWindow = (setRWKeypad on rw ** rw')
                   , colors
                   , csPrf
                   , currentColor
                   , keyMap
                   }

swapNoDelay' : {0 origW : State.Window}
            -> {0 origWs : List State.Window}
            -> {0 e : Elem origW origWs}
            -> (on : Bool)
            -> {rw : _}
            -> (rws : RuntimeWindows origWs)
            -> CurrentWindow rw e rws
            -> (rws' : RuntimeWindows (swapNoDelay on origWs e) ** CurrentWindow (setRWNoDelay on rw) (swapNoDelayPrf on origWs e) rws')
swapNoDelay' on {rw = (MkRuntimeWindow (MkWindow identifier keypad noDelay) b _)} ((MkRuntimeWindow (MkWindow identifier keypad noDelay) b _) :: rws) Here =
  ((MkRuntimeWindow (MkWindow identifier keypad on) b _) :: rws ** Here)
swapNoDelay' on {rw = (MkRuntimeWindow (MkWindow y z w) b win)} ((MkRuntimeWindow (MkWindow identifier keypad noDelay) border winOther) :: rws') (There {other=(MkRuntimeWindow (MkWindow identifier keypad noDelay) border winOther)} x) =
    let (rws'' ** rw') = swapNoDelay' on {rw=MkRuntimeWindow (MkWindow y z w) b win} rws' x
    in  ((MkRuntimeWindow (MkWindow identifier keypad noDelay) border winOther) :: rws'' ** There {other=(MkRuntimeWindow (MkWindow identifier keypad noDelay) border winOther)} rw')

setRuntimeNoDelay : (on : Bool)
                 -> RuntimeCurses (Active i ws w cs)
                 -> RuntimeCurses (setNoDelay (Active i ws w cs) on)
setRuntimeNoDelay on (RActive (MkCursesActive windows (rw ** wPrf) colors {csPrf} currentColor keyMap)) =
  let (windows' ** rw') = swapNoDelay' on windows wPrf
  in
  RActive $
    MkCursesActive { windows = windows'
                   , currentWindow = (setRWNoDelay on rw ** rw')
                   , colors
                   , csPrf
                   , currentColor
                   , keyMap
                   }

coreColor : RuntimeCurses s -> (name : String) -> HasColor name s => ColorPair
coreColor (RActive (MkCursesActive _ _ colors {csPrf} _ _)) name @{ItHasColor @{elem}} =
  getColor colors csPrf elem

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
coreAttr _ (Color DefaultColors) = CP defaultColorPair
coreAttr rs (Color (Named name)) = CP (coreColor rs name)

||| Set the current color IF the atttribute in question is a color attribute.
maybeSetCurrentColor : IsActive s => Attribute s -> RuntimeCurses s -> RuntimeCurses s
maybeSetCurrentColor (Color (Named name @{ItHasColor @{elem}})) (RActive (MkCursesActive windows currentWindow colors {csPrf} _ keyMap)) =
  let color = getColor colors csPrf elem
  in
  RActive (MkCursesActive windows currentWindow colors {csPrf} (Just color) keyMap)
maybeSetCurrentColor _ rs = rs

||| Unset the current color IF the attribute in question is a color attribute.
maybeUnsetCurrentColor : IsActive s => Attribute s -> RuntimeCurses s -> RuntimeCurses s
maybeUnsetCurrentColor (Color _) (RActive (MkCursesActive windows currentWindow colors {csPrf} _ keyMap)) =
  RActive (MkCursesActive windows currentWindow colors {csPrf} Nothing keyMap)
maybeUnsetCurrentColor _ rs = rs

currentWindowHasBorder : CursesActive ws w cs -> Bool
currentWindowHasBorder (MkCursesActive _ ((MkRuntimeWindow _ border _) ** _) _ _ _) = isJust border

currentWindowHasBorder' : IsActive s => RuntimeCurses s -> Bool
currentWindowHasBorder' (RActive as) = currentWindowHasBorder as

modNCursesAttr : HasIO io =>
                 IsActive s =>
                 AttrCmd s
              -> RuntimeCurses s
              -> io (RuntimeCurses s)
modNCursesAttr (SetAttr     attr) rs =
  nSetAttr'     (getCoreWindow' rs) (coreAttr rs attr) $> (maybeSetCurrentColor attr rs)
modNCursesAttr (EnableAttr  attr) rs =
  nEnableAttr'  (getCoreWindow' rs) (coreAttr rs attr) $> (maybeSetCurrentColor attr rs)
modNCursesAttr (DisableAttr attr) rs =
  nDisableAttr' (getCoreWindow' rs) (coreAttr rs attr) $> (maybeUnsetCurrentColor attr rs)
modNCursesAttr (UpdateAttr attr color len) rs =
  let cp = case color of
                DefaultColors => defaultColorPair
                Named name    => coreColor rs name
  in
  nChangeAttr' (getCoreWindow' rs) len (coreAttr rs attr) cp $> (maybeSetCurrentColor (Color color) rs)

printNCurses : HasIO io =>
               IsActive s =>
               OutputCmd s
            -> RuntimeCurses s
            -> io (RuntimeCurses s)
printNCurses (PutCh ch) rs   = nPutCh'          (getCoreWindow' rs) ch   $> rs
printNCurses (VLine ch n) rs = nVerticalLine'   (getCoreWindow' rs) ch n $> rs
printNCurses (HLine ch n) rs = nHorizontalLine' (getCoreWindow' rs) ch n $> rs

printNCurses (Move (MkPosition row col)) rs@(RActive as) =
  nMoveCursor' (getCoreWindow as) (offset row) (offset col) $> rs
  where
    offset : Nat -> Nat
    offset x = if (currentWindowHasBorder as) then (S x) else x

printNCurses (PutStr newline str) rs@(RActive as) = do
    let win = getCoreWindow as
    (_, fullWidth) <- getMaxSize' win
    col <- getXPos' win
    let col' = (if currentWindowHasBorder as then (pred col) else col)
    nPutStr' win (wrapText fullWidth col' str) $> rs
  where
    winWidth : Nat -> (k ** NonZero k)
    winWidth n =
      let w = if currentWindowHasBorder as then (n `minus` 2) else n
      in  case w of 0 => (1 ** %search); (S w') => ((S w') ** %search)

    lineInfix : String
    lineInfix = if currentWindowHasBorder as then "\n " else "\n"

    chunk : (chunkSize : Nat)
         -> NonZero chunkSize =>
            (countdown : Nat)
         -> (rem : List Char)
         -> (acc : SnocList Char)
         -> (acc2 : SnocList String)
         -> List String
    chunk z _     []        [<]       acc2 = cast acc2
    chunk z _     []        (sx :< x) acc2 = cast $ (acc2 :< (pack $ cast (sx :< x)))
    chunk z 0     (x :: xs) acc       acc2 = assert_total $ chunk z z (x :: xs) [<] (acc2 :< (pack $ cast acc))
    chunk z (S k) (x :: xs) acc       acc2 = chunk z k xs (acc :< x) acc2

    wrapText : (fullWidth : Nat) -> (currentColumn : Nat) -> String -> String
    wrapText w c s =
      let (width ** _) = winWidth w
          splitText = chunk width (width `minus` c) (unpack s) [<] [<]
          allTxt = concat (intersperse lineInfix splitText)
          final = if newline then allTxt ++ lineInfix else allTxt
      in  final

drawBorder : HasIO io => Core.Window -> (currentColor : Maybe ColorPair) -> Maybe RuntimeBorder -> io ()
drawBorder _ _ Nothing = pure ()
drawBorder win currentColor (Just (cp, (Evidence _ border))) = do
  nEnableAttr' win (CP cp)
  nWindowBorder win border.left border.right border.top border.bottom
                    border.topLeft border.topRight border.bottomLeft border.bottomRight
  case currentColor of
       Just cp' => nEnableAttr' win (CP cp')
       Nothing  => nDisableAttr' win (CP cp)

drawCurrentBorder : HasIO io => CursesActive ws w cs -> io ()
drawCurrentBorder (MkCursesActive _ ((MkRuntimeWindow props border win) ** _) _ currentColor _) =
  drawBorder win currentColor border

refreshAllRuntime : HasIO io => RuntimeWindows ws -> (currentColor : Maybe ColorPair) -> io ()
refreshAllRuntime [] _ = pure ()
refreshAllRuntime ((MkRuntimeWindow _ border win) :: ws) currentColor =
  drawBorder win currentColor border *> refresh' win *> refreshAllRuntime ws currentColor

runNCurses : HasIO io => NCurses a s1 s2 -> RuntimeCurses s1 -> io (a, RuntimeCurses s2)
runNCurses (Pure x) rs = pure (x, rs)
runNCurses (Bind x f) rs = do
  (x', rs') <- runNCurses x rs
  runNCurses (f x') rs'
runNCurses (NIO ops) rs = do
  res <- liftIO ops
  pure (res, rs)
----
runNCurses Init RInactive = Prelude.do
  initNCurses
  cBreak
  noEcho
  keypad True
  noDelay False
  win <- stdWindow
  keyMap <- SpecialKey.keyMap
  pure ((), RActive $ MkCursesActive [initRuntimeWindow DefaultWindow Nothing win]
                                     (initRuntimeWindow DefaultWindow Nothing win ** Here)
                                     []
                                     {csPrf=Refl}
                                     Nothing
                                     keyMap)
runNCurses DeInit (RActive _) = do
  deinitNCurses
  pure ((), RInactive)
----
runNCurses (AddWindow @{isActive} name pos size border) rs@(RActive as) = Prelude.do
  runtimeWin <- newWindow size.rows size.cols pos.row pos.col
  keypad' runtimeWin True
  noDelay' runtimeWin False
  let b = runtimeBorder rs <$> border
  drawBorder runtimeWin as.currentColor b
  let as' = addRuntimeWindow name b runtimeWin as
  when (isJust b) $ -- offset cursor to just inside the border.
    nMoveCursor' runtimeWin 1 1
  pure ((), RActive as')
runNCurses (SetWindow   @{_} name @{ItHasWindow @{elem}}) (RActive as) = pure ((), RActive $ setRuntimeWindow elem as)
runNCurses (UnsetWindow @{_}      @{ItHasWindow @{elem}}) (RActive as) = pure ((), RActive $ setRuntimeWindow elem as)
----
runNCurses Clear rs@(RActive as) = do
  let win = getCoreWindow as
  clear' win
  when (currentWindowHasBorder as) $ -- offset cursor to just inside the border.
    nMoveCursor' win 1 1
  pure ((), rs)
runNCurses Erase   rs@(RActive as) = do
  let win = getCoreWindow as
  erase' win
  when (currentWindowHasBorder as) $ -- offset cursor to just inside the border.
    nMoveCursor' win 1 1
  pure ((), rs)
runNCurses Refresh rs@(RActive as) =
  let win = getCoreWindow as
  in  drawCurrentBorder as *> refresh' win $> ((), rs)
runNCurses RefreshAll rs@(RActive (MkCursesActive windows _ _ currentColor _)) = 
  -- refresh std window:
  refresh *>
  -- followed by all others:
  refreshAllRuntime windows currentColor $> ((), rs)
----
runNCurses (AddColor name fg bg) (RActive as) = do
  let nextIdx = length as.colors
  when (nextIdx == 0) startColor
  cp <- initColorPair nextIdx fg bg
  let as' = addRuntimeColor name cp as
  pure ((), RActive as')
runNCurses (ModAttr cmd) rs = do
  rs' <- modNCursesAttr cmd rs
  pure ((), rs')
runNCurses (Output cmd) rs = do
  rs' <- printNCurses cmd rs
  pure ((), rs')
runNCurses (SetPos pos) rs = moveWindow (getCoreWindow' rs) pos.row pos.col $> ((), rs)
runNCurses GetPos rs = do
  let win = getCoreWindow' rs
  y <- getYPos' win
  x <- getXPos' win
  let offset : Nat -> Nat = (\coord => if currentWindowHasBorder' rs then (pred coord) else coord)
  pure (MkPosition (offset y) (offset x), rs)
runNCurses (SetSize size) rs = setWindowSize (getCoreWindow' rs) size.cols size.rows $> ((), rs)
runNCurses (GetSize internal) rs = do
  (rows, cols) <- getMaxSize' (getCoreWindow' rs)
  let offset : Nat -> Nat = (\dim => if currentWindowHasBorder' rs && internal then (dim `minus` 2) else dim)
  pure (MkSize (offset rows) (offset cols), rs)
runNCurses (SetEcho   on) (RActive as) = (ifThenElse on echo noEcho)     $> ((), RActive as)
runNCurses (SetCBreak on) (RActive as) = (ifThenElse on cBreak noCBreak) $> ((), RActive as)
runNCurses (SetKeypad on) (RActive as) = keypad' (getCoreWindow as)   on $> ((), setRuntimeKeypad on $ RActive as)
runNCurses (SetNoDelay on) (RActive as) = noDelay' (getCoreWindow as) on $> ((), setRuntimeNoDelay on $ RActive as)
runNCurses (SetCursor c) rs = setCursorVisibility c $> ((), rs)
runNCurses GetCh rs@(RActive as@(MkCursesActive windows {w} ((MkRuntimeWindow (MkWindow _ keypad noDelay) _ _) ** wPrf) colors currentColor keyMap)) with 0 (w)
  runNCurses GetCh rs@(RActive as@(MkCursesActive windows {w} ((MkRuntimeWindow (MkWindow _ keypad noDelay) _ _) ** wPrf) colors currentColor keyMap)) | (w' ** e') =
    rewrite sym $ currentWindowPropsPrf e' wPrf in
      if noDelay
         then if keypad
                 then do Just ch <- safeGetCh' (getCoreWindow as)
                           | Nothing => pure (Nothing, rewrite currentWindowPropsPrf e' wPrf in rs)
                         let keyOrCh = maybeToEither ch (lookup ch keyMap)
                         pure (Just keyOrCh, rewrite currentWindowPropsPrf e' wPrf in rs)
                 else do ch <- safeGetCh' (getCoreWindow as)
                         pure (ch, rewrite currentWindowPropsPrf e' wPrf in rs)
         else if keypad
                then do ch <- getCh' (getCoreWindow as)
                        let keyOrCh = maybeToEither ch (lookup ch keyMap)
                        pure (keyOrCh, rewrite currentWindowPropsPrf e' wPrf in rs)
                else do ch <- getCh' (getCoreWindow as)
                        pure (ch, rewrite currentWindowPropsPrf e' wPrf in rs)
runNCurses (InWindow _ @{_} @{ItHasWindow @{elem}} nc) rs@(RActive as) = do
  r <- runNCurses nc (RActive $ setRuntimeWindow elem as)
  pure ((), rs)

||| Run an NCurses program with guarantees
||| that it is initialized at the beginning and
||| deinitialied at the end.
export
withNCurses : HasIO io => NCurses a Inactive Inactive -> io a
withNCurses nc =
  evalStateT RInactive $
    lift $ fst <$> (runNCurses nc RInactive)

testProgram : HasIO io => io ()
testProgram = withNCurses testRoutine

