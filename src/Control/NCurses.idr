module Control.NCurses

import Data.List.Elem
import NCurses
import Control.TransitionIndexed
import Control.Monad.State

%default total

||| The state of NCurses.
||| TODO: parameterize on Eq type to use as color id? Eq a => (colors : List a)
public export
data CursesState : Type where
  Inactive : CursesState
  Active : (colors : List String) -> CursesState

public export
(.active) : CursesState -> Bool
(.active) Inactive = False
(.active) (Active _) = True

public export
initState : CursesState
initState = Active []

namespace CursesState
  public export
  data IsActive : CursesState -> Type where
    ItIsActive : IsActive (Active _)

  public export
  data IsInactive : CursesState -> Type where
    ItIsInactive : IsInactive Inactive

  public export
  addColor : (s : CursesState) -> IsActive s => (n : String) -> CursesState
  addColor Inactive @{ItIsActive} n impossible
  addColor (Active colors) n = Active (n :: colors)

namespace Attribute
  ||| Proof that the given color exists in the current
  ||| NCurses session.
  public export
  data HasColor : (0 name : String) -> CursesState -> Type where
    ItHasColor : Elem name cs => HasColor name (Active cs)

  public export
  data Attribute : CursesState -> Type where
    Normal : Attribute s
    Underline : Attribute s
    Standout : Attribute s
    Reverse : Attribute s
    Blink : Attribute s
    Dim : Attribute s
    Bold : Attribute s
    Protected : Attribute s
    Invisible : Attribute s
    Color : (name : String) -> HasColor name s => Attribute s

  public export
  data AttrCmd : CursesState -> Type where
    SetAttr : Attribute s -> AttrCmd s

namespace Print
  public export
  data PrintCmd : CursesState -> Type where
    PutStr : String -> PrintCmd s

public export
data NCurses : (0 a : Type) -> CursesState -> (0 _ : a -> CursesState) -> Type where
  Pure : (x : a) -> NCurses a (fs x) fs
  Bind : NCurses a s1 fs2 -> ((x : a) -> NCurses b (fs2 x) fs3) -> NCurses b s1 fs3

  Init     : IsInactive s => NCurses () s (const NCurses.initState)
  DeInit   : IsActive s => NCurses () s (const Inactive)
  AddColor : IsActive s => (name : String) -> (fg : Color) -> (bg : Color) -> NCurses () s (const (addColor s name))
  ModAttr  : IsActive s => AttrCmd s -> NCurses () s (const s)
  Clear    : IsActive s => NCurses () s (const s)
  Refresh  : IsActive s => NCurses () s (const s)
  Print    : IsActive s => PrintCmd s -> NCurses () s (const s)

public export
TransitionIndexedPointed CursesState NCurses where
  pure = Pure

public export
TransitionIndexedMonad CursesState NCurses where
  bind = Bind

public export
init : IsInactive s => NCurses () s (const NCurses.initState)
init = Init

public export
deinit : IsActive s => NCurses () s (const Inactive)
deinit = DeInit

public export
clear : IsActive s => NCurses () s (const s)
clear = Clear

public export
refresh : IsActive s => NCurses () s (const s)
refresh = Refresh

||| Add a color to the current NCurses session.
|||
||| Once added, colors can be referenced by name
||| when constructing Attributes.
public export
addColor : IsActive s => (name : String) -> (fg : Color) -> (bg : Color) -> NCurses () s (const (addColor s name))
addColor = AddColor

||| Set an attribute to be applied in the standard window
||| until it is cleared or overwritten.
|||
||| In ncurses terminology, "attrset"
|||
||| See @nSetAttr'@ for a version that works on
||| any given window.
public export
setAttr : IsActive s => Attribute s -> NCurses () s (const s)
setAttr = ModAttr . SetAttr

public export
putStr : IsActive s => String -> NCurses () s (const s)
putStr = Print . PutStr

testRoutine : NCurses () Inactive (const Inactive)
testRoutine = TransitionIndexed.Do.do
  init
  addColor "alert" White Red
  setAttr Underline
  setAttr (Color "alert")
  clear >> refresh
  putStr "Hello World"
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

record CursesActive (0 cs : List String) where
  constructor MkCursesActive
  colors : List (String, ColorPair)
  {0 csPrf : (keys colors) = cs}

data RuntimeCurses : CursesState -> Type where
  RInactive : RuntimeCurses Inactive
  RActive   : CursesActive cs -> RuntimeCurses (Active cs)

getColor : (colors : List (String, ColorPair)) -> (0 csPrf : NCurses.keys colors = names) -> (elemPrf : Elem _ names) -> ColorPair
getColor (z :: zs) csPrf Here = snd z
getColor [] csPrf (There elemPrf) impossible
getColor (z :: zs) csPrf (There elemPrf) = getColor zs (snd $ keysInjective csPrf) elemPrf

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
coreAttr (RActive (MkCursesActive colors {csPrf})) (Color name @{ItHasColor @{elem}}) =
  let color = getColor colors csPrf elem
  in  CP color

modNCursesAttr : HasIO io =>
                 AttrCmd s
              -> RuntimeCurses s
              -> io (RuntimeCurses s)
modNCursesAttr (SetAttr attr) rs = do
  nSetAttr (coreAttr rs attr)
  pure rs

printNCurses : HasIO io =>
               PrintCmd s
            -> RuntimeCurses s
            -> io (RuntimeCurses s)
printNCurses (PutStr str) rs = do
  nPutStr str
  pure rs

runNCurses : HasIO io => NCurses a s fs -> RuntimeCurses s -> io (x : a ** RuntimeCurses (fs x))
runNCurses (Pure x) rs = pure (x ** rs)
runNCurses (Bind x f) rs = do
  (x' ** rs') <- runNCurses x rs
  runNCurses (f x') rs'
----
runNCurses Init RInactive = do
  initNCurses
  pure (() ** RActive $ MkCursesActive [] {csPrf=Refl})
runNCurses DeInit (RActive _) = do
  deinitNCurses
  pure (() ** RInactive)
runNCurses (AddColor name fg bg) (RActive as) = do
  let nextIdx = length as.colors
  cp <- initColorPair nextIdx fg bg
  let as' = { colors $= ((name, cp) ::)
            , csPrf $= (cong (name ::)) } as
  pure (() ** RActive as')
runNCurses (ModAttr cmd) rs = do
  rs' <- modNCursesAttr cmd rs
  pure (() ** rs')
runNCurses Clear   rs = clear $> (() ** rs)
runNCurses Refresh rs = refresh $> (() ** rs)
runNCurses (Print cmd) rs = do
  rs' <- printNCurses cmd rs
  pure (() ** rs')

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

