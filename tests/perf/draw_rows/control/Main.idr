module Main

import Control.NCurses
import Control.NCurses.Pretty
import Data.Nat
import Data.List
import Data.String
import System.Clock

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
  printDoc (prettyDoc 100)
  erase
  printDoc (prettyDoc 100)
  clear
  printDoc (prettyDoc 100)
  addWindow "secondary" (MkPosition 0 0) (MkSize 100 100) Nothing
  setWindow "secondary"
  printDoc (prettyDoc 100)
  erase
  printDoc (prettyDoc 100)
  clear
  printDoc (prettyDoc 100)
  deinit

show' : Clock t -> String
show' (MkClock seconds nanoseconds) =
  let nanosecondDigits = 4
      seconds = show seconds
      quotient : Integer = cast $ 10 `power` minus 9 nanosecondDigits
      nanoseconds = padRight nanosecondDigits '0' $ show (cast nanoseconds `div` quotient)
  in  "\{seconds}.\{nanoseconds}"

main : IO ()
main = do
  t1 <- clockTime UTC
  withNCurses run
  t2 <- clockTime UTC
  let diff = timeDifference t2 t1
  putStrLn $ show' diff
