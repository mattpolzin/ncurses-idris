module Main

import Control.NCurses
import Control.NCurses.Pretty
import Data.Nat
import Data.List
import Data.String
import System.Clock

rows : Nat
rows = 200

-- purposefully only using 1 color here for this particular perf test
printRows : IsActive s => HasColor "red" s => Nat -> NCurses () s s
printRows 0 = pure ()
printRows (S k) = do
  enableAttr (Color (Named "red"))
  putStrLn "Hello. This is just extra stuff."
  printRows k

run : NCurses () Inactive Inactive 
run = Indexed.Do.do
  init
  addColor "inverse" Black White
  addColor "alert" White Red
  addColor "red" Red Black
  printRows rows
  erase
  printRows rows
  clear
  printRows rows
  addWindow "secondary" (MkPosition 0 0) (MkSize rows 100) Nothing
  setWindow "secondary"
  printRows rows
  erase
  printRows rows
  clear
  printRows rows
  deinit

show' : Clock t -> String
show' (MkClock seconds nanoseconds) =
  let seconds = show seconds
      nanoseconds = padLeft 5 '0' $ show (cast nanoseconds `div` 10000)
  in  "\{seconds}.\{nanoseconds}"

main : IO ()
main = Prelude.do
  t1 <- clockTime UTC
  withNCurses run
  t2 <- clockTime UTC
  let diff = timeDifference t2 t1
--   putStrLn $ show $ nanoseconds diff
  putStrLn $ show' diff
