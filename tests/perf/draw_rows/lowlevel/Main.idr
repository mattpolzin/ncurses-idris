module Main

import Data.Nat
import Data.List
import Data.String
import Data.String.Extra
import NCurses
import System.Clock

-- 200 rows: 0.828 _or_ 82.8% faster at low level

rows : Nat
rows = 200

-- purposefully only using 1 color here for this particular perf test
printRows : Nat -> (window : Window) -> (red : ColorPair) -> (alert : ColorPair) -> (inverse : ColorPair) -> IO ()
printRows rows w red _ _ = do
  nSetAttr' w (CP red)
  for_ [0..rows] $ \row =>
    nPutStrAt' w row "Hello. This is just extra stuff."

run : IO ()
run = do
  initNCurses
  startColor
  inverse <- initColorPair 1 Black White
  alert   <- initColorPair 2 White Red
  red     <- initColorPair 3 Red Black
  stdWin  <- stdWindow
  printRows rows stdWin red alert inverse
  erase' stdWin
  printRows rows stdWin red alert inverse
  clear' stdWin
  printRows rows stdWin red alert inverse
  secondary <- newWindow rows 100 0 0
  printRows rows secondary red alert inverse
  erase' stdWin
  printRows rows secondary red alert inverse
  clear' stdWin
  printRows rows secondary red alert inverse
  deinitNCurses

show' : Clock t -> String
show' (MkClock seconds nanoseconds) =
  let seconds = show seconds
      nanoseconds = padLeft 5 '0' $ show (cast nanoseconds `div` 10000)
  in  "\{seconds}.\{nanoseconds}"

main : IO ()
main = do
  t1 <- clockTime UTC
  run
  t2 <- clockTime UTC
  let diff = timeDifference t2 t1
--   putStrLn $ show $ nanoseconds diff
  putStrLn $ show' diff
