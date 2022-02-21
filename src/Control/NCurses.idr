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
initInput = MkInput { cBreak = True
                    , echo = False
                    }

||| Windows allow a terminal screen to be divided up and drawn to separately
||| with their own relative coordinates.
public export
data Window : Type where
  ||| Set to true to receive "special keys" as single values rather than composites
  ||| of two or more Chars. For example, arrow keys are represented as two input
  ||| chars, but you will receive @Key@ values for them instead with the @keypad@
  ||| property turned on.
  |||
  ||| This is off by default.
  MkWindow : (identifier : String) -> (keypad : Bool) -> Window

public export
(.identifier) : NCurses.Window -> String
(.identifier) (MkWindow i _) = i

public export
initWindow : String -> NCurses.Window
initWindow id = MkWindow { identifier = id
                         , keypad = False
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
        -> (0 currentWindow : Maybe (w ** Elem w windows))
        -> (0 colors : List String)
        -> CursesState

public export
(.active) : CursesState -> Bool
(.active) Inactive = False
(.active) (Active _ _ _ _) = True

public export %inline
initState : CursesState
initState = Active initInput [] Nothing []

public export %inline
0 bumpWindow : Maybe (DPair NCurses.Window (\w => Elem w ws)) -> Maybe (DPair NCurses.Window (\w => Elem w (y :: ws)))
bumpWindow Nothing = Nothing
bumpWindow (Just (q ** r)) = Just (q ** There r)

namespace CursesState
  public export
  data IsActive : CursesState -> Type where
    ItIsActive : IsActive (Active _ _ _ _)

  public export
  data IsInactive : CursesState -> Type where
    ItIsInactive : IsInactive Inactive

  public export %inline
  0 addWindow : (s : CursesState) -> IsActive s => (name : String) ->  CursesState
  addWindow Inactive @{ItIsActive} n impossible
  addWindow (Active i ws w cs) n = Active i (initWindow n :: ws) (bumpWindow w) cs

  public export %inline
  0 addColor : (s : CursesState) -> IsActive s => (n : String) -> CursesState
  addColor Inactive @{ItIsActive} n impossible
  addColor (Active i ws w cs) n = Active i ws w (n :: cs)

  public export %inline
  0 setEcho : (s : CursesState) -> IsActive s => (on : Bool) -> CursesState
  setEcho Inactive @{ItIsActive} on impossible
  setEcho (Active input ws w cs) on = Active ({ echo := on } input) ws w cs

  public export %inline
  0 setCBreak : (s : CursesState) -> IsActive s => (on : Bool) -> CursesState
  setCBreak Inactive @{ItIsActive} on impossible
  setCBreak (Active input ws w cs) on = Active ({ cBreak := on } input) ws w cs

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
    Here : IdentifiesWindow name ((MkWindow name _) :: windows)
    There : IdentifiesWindow name windows -> IdentifiesWindow name (w :: windows)

  public export
  data HasWindow : (0 name : String) -> CursesState -> Type where
    ItHasWindow : IdentifiesWindow name ws => HasWindow name (Active _ ws w _)

  public export %inline
  0 unsetWindow : (s : CursesState) -> IsActive s => CursesState
  unsetWindow Inactive @{ItIsActive} impossible
  unsetWindow (Active i ws _ cs) = Active i ws Nothing cs

  export
  0 lookupWindow : (name : String) -> (ws : List NCurses.Window) -> IdentifiesWindow name ws => (w ** Elem w ws)
  lookupWindow name (MkWindow name k :: windows) @{Here} = ((MkWindow name k) ** Here)
  lookupWindow name (w :: windows) @{(There elem)} =
    let (w' ** e) = lookupWindow name windows
    in  (w' ** There e)

  public export %inline
  0 setWindow : (s : CursesState) -> (name : String) -> HasWindow name s => CursesState
  setWindow Inactive name @{ItHasWindow} impossible
  setWindow (Active i ws _ cs) name @{ItHasWindow @{elem}} =
    Active i ws (Just $ lookupWindow name ws) cs

export
data NCurses : (a : Type) -> CursesState -> (a -> CursesState) -> Type where
  Pure : (x : a) -> NCurses a (fs x) fs
  Bind : NCurses a s1 fs2 -> ((x : a) -> NCurses b (fs2 x) fs3) -> NCurses b s1 fs3

  Init        : IsInactive s => NCurses () s (const NCurses.initState)
  DeInit      : IsActive s => NCurses () s (const Inactive)
  AddWindow   : IsActive s => (name : String) -> Position -> Size -> NCurses () s (const (addWindow s name))
  SetWindow   : IsActive s => (name : String) -> HasWindow name s => NCurses () s (const (setWindow s name))
  UnsetWindow : IsActive s => NCurses () s (const (unsetWindow s))
  AddColor    : IsActive s => (name : String) -> (fg : Color) -> (bg : Color) -> NCurses () s (const (addColor s name))
  ModAttr     : IsActive s => AttrCmd s -> NCurses () s (const s)
  Clear       : IsActive s => NCurses () s (const s)
  Refresh     : IsActive s => NCurses () s (const s)
  Output      : IsActive s => OutputCmd s -> NCurses () s (const s)
  SetEcho     : IsActive s => (on : Bool) -> NCurses () s (const (setEcho s on))
  SetCBreak   : IsActive s => (on : Bool) -> NCurses () s (const (setCBreak s on))
  SetCursor   : IsActive s => CursorVisibility -> NCurses () s (const s)
  GetPos      : IsActive s => NCurses Position s (const s)
  GetSize     : IsActive s => NCurses Size s (const s)
--   GetCh    : IsActive s => NCurses Char s (const s)

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
unsetWindow : IsActive s => NCurses () s (const (unsetWindow s))
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

||| cBreak indicates whether characters typed by the user are delivered to the
||| program (via @getCh@) immediately or not. If not, they are delivered when a
||| newline is typed (the default behavior for most terminal environments).
|||
||| This setting affects all windows.
setCBreak : IsActive s => (on : Bool) -> NCurses () s (const (setCBreak s on))
setCBreak = SetCBreak

||| echo indicates whether characters typed by the user are printed to the screen
||| by NCurses as well as delivered to @getCh@.
|||
||| This setting affects all windows.
setEcho : IsActive s => (on : Bool) -> NCurses () s (const (setEcho s on))
setEcho = SetEcho

||| Set the way the cursor is displayed to the user.
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

record CursesActive (0 ws : List NCurses.Window) (0 cs : List String) where
  constructor MkCursesActive
  windows : List (String, Core.Window)
  {0 wsPrf : (keys windows) = ((.identifier) <$> ws)}
  colors : List (String, ColorPair)
  {0 csPrf : (keys colors) = cs}

data RuntimeCurses : CursesState -> Type where
  RInactive : RuntimeCurses Inactive
  RActive   : CursesActive ws cs -> RuntimeCurses (Active i ws w cs)

getColor : (colors : List (String, ColorPair))
        -> (0 csPrf : NCurses.keys colors = names)
        -> (elemPrf : Elem _ names)
        -> ColorPair
getColor (z :: zs) csPrf Here = snd z
getColor [] csPrf (There elemPrf) impossible
getColor (z :: zs) csPrf (There elemPrf) = getColor zs (snd $ keysInjective csPrf) elemPrf

getWindow : (windows : List (String, Core.Window))
         -> (0 wsPrf : NCurses.keys windows = ((.identifier) <$> ws))
         -> (elemPrf : Elem _ ws)
         -> Core.Window

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
coreAttr (RActive (MkCursesActive _ colors {csPrf})) (Color name @{ItHasColor @{elem}}) =
  let color = getColor colors csPrf elem
  in  CP color

modNCursesAttr : HasIO io =>
                 AttrCmd s
              -> RuntimeCurses s
              -> io (RuntimeCurses s)
modNCursesAttr (SetAttr attr) rs     = nSetAttr     (coreAttr rs attr) $> rs
modNCursesAttr (EnableAttr attr) rs  = nEnableAttr  (coreAttr rs attr) $> rs
modNCursesAttr (DisableAttr attr) rs = nDisableAttr (coreAttr rs attr) $> rs

printNCurses : HasIO io =>
               OutputCmd s
            -> RuntimeCurses s
            -> io (RuntimeCurses s)
printNCurses (PutStr str) rs   = nPutStr str $> rs
printNCurses (Move (MkPosition row col)) rs = nMoveCursor row col $> rs
printNCurses (PutCh ch) rs     = nPutCh ch $> rs

addRuntimeWindow : (name : String)
                -> (runtimeWin : Core.Window)
                -> CursesActive ws cs
                -> CursesActive (initWindow name :: ws) cs
addRuntimeWindow identifier runtimeWin (MkCursesActive windows {wsPrf} colors {csPrf}) =
  MkCursesActive { windows = (identifier, runtimeWin) :: windows
                 , wsPrf = cong (identifier ::) wsPrf
                 , colors
                 , csPrf
                 }

addRuntimeColor : (name : String) -> (cp : ColorPair) -> CursesActive ws cs -> CursesActive ws (name :: cs)
addRuntimeColor name cp (MkCursesActive windows {wsPrf} colors {csPrf}) =
  MkCursesActive { windows
                 , wsPrf
                 , colors = (name, cp) :: colors
                 , csPrf  = cong (name ::) csPrf
                 }

runNCurses : HasIO io => NCurses a s fs -> RuntimeCurses s -> io (x : a ** RuntimeCurses (fs x))
runNCurses (Pure x) rs = pure (x ** rs)
runNCurses (Bind x f) rs = do
  (x' ** rs') <- runNCurses x rs
  runNCurses (f x') rs'
----
runNCurses Init RInactive = Prelude.do
  initNCurses
  cBreak
  noEcho
  pure (() ** RActive $ MkCursesActive [] [] {wsPrf=Refl, csPrf=Refl})
runNCurses DeInit (RActive _) = do
  deinitNCurses
  pure (() ** RInactive)
runNCurses (AddWindow @{isActive} name pos size) (RActive as) = do
  runtimeWin <- newWindow size.rows size.cols pos.row pos.col
  let as' = addRuntimeWindow name runtimeWin as
  pure (() ** RActive as')
runNCurses (SetWindow @{_} name @{ItHasWindow @{elem}}) (RActive as) = pure (() ** RActive as)
runNCurses UnsetWindow (RActive as) = pure (() ** RActive as)
runNCurses (AddColor name fg bg) (RActive as) = do
  let nextIdx = length as.colors
  when (nextIdx == 0) startColor
  cp <- initColorPair nextIdx fg bg
  let as' = addRuntimeColor name cp as
  pure (() ** RActive as')
runNCurses (ModAttr cmd) rs = do
  rs' <- modNCursesAttr cmd rs
  pure (() ** rs')
runNCurses Clear   rs = clear $> (() ** rs)
runNCurses Refresh rs = refresh $> (() ** rs)
runNCurses (Output cmd) rs = do
  rs' <- printNCurses cmd rs
  pure (() ** rs')
runNCurses (NIO ops) rs = do
  res <- liftIO ops
  pure (res ** rs)
runNCurses GetPos rs = pure (MkPosition !(getYPos) !(getXPos) ** rs)
runNCurses GetSize rs = do
  size <- (uncurry MkSize) <$> getMaxSize' !stdWindow
  pure (size ** rs)
runNCurses (SetEcho on) (RActive as) = (ifThenElse on echo noEcho) $> (() ** RActive as)
runNCurses (SetCBreak on) (RActive as) = (ifThenElse on cBreak noCBreak) $> (() ** RActive as)
runNCurses (SetCursor c) rs = setCursorVisibility c $> (() ** rs)

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

