module Main

import System
import System.Signal
import Control.NCurses
import Control.NCurses.Pretty

loop : NCurses () s s
loop =
  case !(liftIO handleNextCollectedSignal) of
       (Just SigINT) => pure ()
       _ => (liftIO $ sleep 1) >> loop

inWin1 : IsActive s => InWindow "win1" s => HasColor "inverse" s => NCurses () s s
inWin1 = do
  erase
  setAttr (Color "inverse")
  putStrLn "hello from a window"
  setAttr DefaultColors
  refresh

inMainWin : IsActive s => InWindow "main" s => NCurses () s s
inMainWin = do
  putStrLn "Back in main"
  refresh

run : NCurses () Inactive Inactive
run = Indexed.Do.do
  init
  addWindow "main" (MkPosition 0 0) (MkSize 35 45) Nothing
  setWindow "main"
  addColor "inverse" Black White
  addColor "alert" White Red
  clear
  -- direct invocation
  setAttr (Color "inverse")
  putStr "Ctrl+C to exit\n\n"
  setAttr Underline
  putStr "Hello World\n\n"
  setAttrs [(Color "alert"), Bold]
  putStr "THIS IS NOT A PROBLEM"
  setAttr Normal
  putStrLn "\n\nEnd of initial transmission."
  addWindow "win1" (MkPosition 35 55) (MkSize 20 30) Nothing
  inWindow "win1" inWin1
  inMainWin
  loop
  deinit

main : IO ()
main = do
  ignore $ collectSignal SigINT
  withNCurses run

