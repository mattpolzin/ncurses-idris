module Main

import System
import System.Signal
import Control.NCurses

loop : NCurses () s (const s)
loop =
  case !(NIO handleNextCollectedSignal) of
       (Just SigINT) => pure ()
       _ => (NIO $ sleep 1) >> loop

run : NCurses () Inactive (const Inactive)
run = TransitionIndexed.Do.do
  init
  addColor "inverse" Black White
  addColor "alert" White Red
  clear
  setAttr Underline
  putStr "Hello World\n\n"
  setAttr (Color "alert")
  putStr "THIS IS NOT A PROBLEM"
  refresh
  loop
  deinit

main : IO ()
main = do
  ignore $ collectSignal SigINT
  withNCurses run

