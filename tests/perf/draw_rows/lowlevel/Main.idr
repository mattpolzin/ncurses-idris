module Main

import Data.Nat
import Data.String
import NCurses
import System.Clock

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
  printRows 100 stdWin red alert inverse
  erase' stdWin
  printRows 100 stdWin red alert inverse
  clear' stdWin
  printRows 100 stdWin red alert inverse
  secondary <- newWindow 100 100 0 0
  printRows 100 secondary red alert inverse
  erase' stdWin
  printRows 100 secondary red alert inverse
  clear' stdWin
  printRows 100 secondary red alert inverse
  deinitNCurses

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
  run
  t2 <- clockTime UTC
  let diff = timeDifference t2 t1
  putStrLn $ show' diff
