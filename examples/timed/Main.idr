module Main

import Data.Fuel
import System
import NCurses

loop : HasIO io => Nat -> io ()
loop 0 = pure ()
loop (S fuel) = do
  nPutStrLn "Looping... \{show fuel}"
  refresh
  sleep 1
  loop fuel

main : IO ()
main = do
  initNCurses
  startColor
  cp <- initColorPair 1 Green Black
  nSetAttr (CP cp)
  clear
  loop 5
  deinitNCurses

