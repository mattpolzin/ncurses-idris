module Main

import Data.Fuel
import System
import NCurses

loop : HasIO io => Fuel -> io ()
loop Dry = pure ()
loop (More fuel) = do
  nPutStrLn "Looping..."
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
  loop (limit 5)
  deinitNCurses

