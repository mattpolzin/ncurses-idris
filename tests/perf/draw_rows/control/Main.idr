module Main

import Control.NCurses
import Control.NCurses.Pretty
import Data.Nat
import Data.List
import Data.String
import System.Clock


----
---- NOTES
----

-- Control.NCurses perf test spends 18% of its time performing
-- ModAttr commands. short circuiting for unchanged current color
-- does not seem to save much time.

-- Control.NCurses perf test spends 3% of its time performing
-- Move commands.


rows : Nat
rows = 200

-- purposefully only using 1 color here for this particular perf test
prettyDoc : Nat -> Doc (Attribute (Active i ws w ["red", "alert", "inverse"]))
prettyDoc rows =
  vsep $ (replicate rows 
           (color "red" "Hello. This is just extra stuff.")
         )

run : NCurses () Inactive Inactive 
run = Indexed.Do.do
  init
  addColor "inverse" Black White
  addColor "alert" White Red
  addColor "red" Red Black
  printDoc (prettyDoc rows)
  erase
  printDoc (prettyDoc rows)
  clear
  printDoc (prettyDoc rows)
  addWindow "secondary" (MkPosition 0 0) (MkSize rows 100) Nothing
  setWindow "secondary"
  printDoc (prettyDoc rows)
  erase
  printDoc (prettyDoc rows)
  clear
  printDoc (prettyDoc rows)
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
