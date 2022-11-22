module Main

import System
import System.Signal
import Control.NCurses
import Control.NCurses.Pretty
import Control.Monad.Indexed.State

data Rows : Type -> Type where
  Nil : Rows a
  (::) : {default False highlight : Bool} -> a -> Rows a -> Rows a

down : Rows a -> Rows a
down [] = []
down ((::) x xs {highlight = False}) = x :: down xs
down ((::) x [] {highlight = True}) = (::) x [] {highlight = True}
down ((::) x (y :: xs) {highlight = True}) = x :: ((::) y xs {highlight = True})

up : Rows a -> Rows a
up [] = []
up ((::) x xs {highlight = True}) = ((::) x xs {highlight = True})
up ((::) x [] {highlight = False}) = [x]
up ((::) x ((::) y xs {highlight = False})) = x :: up (y :: xs)
up ((::) x ((::) y xs {highlight = True})) = ((::) x (y :: xs) {highlight = True})

init : Rows a -> Rows a
init [] = []
init (x :: xs) = (::) x xs {highlight = True}

State : Type -> CursesState -> CursesState -> Type
State = IndexedStateT (Rows String) CursesState CursesState NCurses

items : Rows String
items = [ "this"
        , "is"
        , "a"
        , "list"
        , "of"
        , "strings"
        ]

prettyDoc : Rows String -> Doc (Attribute s)
prettyDoc rows = vsep $ go rows
  where
    go : Rows String -> List (Doc (Attribute s))
    go [] = []
    go ((::) x xs {highlight = False}) = pretty x :: go xs
    go ((::) x xs {highlight = True }) = standout (pretty x) :: go xs

mutual
  step : IsActive s => YesKeypad s => YesDelay s => State () s s
  step = do
    rows <- get
    lift $ Indexed.do
      erase
      printDoc (prettyDoc rows)
      refresh
    Right key <- lift $ getKeyOrChar
      | Left _ => loop
    case key of
         Up   => modify up
         Down => modify down
         _    => pure ()
    loop

  loop : IsActive s => YesKeypad s => YesDelay s => State () s s
  loop =
    case !(lift $ liftIO handleNextCollectedSignal) of
         (Just SigINT) => pure ()
         _ => step

run : NCurses () Inactive Inactive
run = Indexed.do
  init
  setCursor CInvisible
  addWindow "options" (MkPosition 1 0) (MkSize 10 10) Nothing
  addColor "heading" White Red
  clear
  setAttr (Color (Named "heading"))
  putStr "Ctrl+C to exit, arrow keys to select a row."
  refresh
  setWindow "options"
  setAttr (Color DefaultColors)
  evalStateT (init items) loop
  deinit

main : IO ()
main = do
  ignore $ collectSignal SigINT
  withNCurses run

