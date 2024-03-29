module Main

import System
import System.Signal
import Control.NCurses
import NCurses.Core

loop : NCurses () s s
loop =
  case !(liftIO handleNextCollectedSignal) of
       (Just SigINT) => pure ()
       _ => (liftIO $ sleep 1) >> loop

run : NCurses () Inactive Inactive
run = Indexed.do
  init
  clear
  addColor "green" Green Black
  addColor "greenBg" Black Green 
  addColor "red" Black Red
  addWindow "fst" (MkPosition 0 0) (MkSize 9 8) (defaultBorder "green")
  setWindow "fst"
  putStrLn "a"
  erase
  putStrLn "b"
  putStrLn "c"
  move (MkPosition 3 4)
  putStr "hello you"
  putStr " more text."
  addWindow "snd" (MkPosition 10 0) (MkSize 8 4) (border "red" (Custom '{') (Custom '}') (Custom '=') (Custom '-')
                                                               (Custom '*') (Custom '#') (Custom '+') (Custom '/'))
  setWindow "snd"
  putStrLn "a"
  putStrLn "bcd"
  addWindow "thrd" (MkPosition 0 10) (MkSize 5 7) (defaultBorder "green")
  setWindow "thrd"
  setBackground (Named "greenBg")
  unsetWindow
  refreshAll
  loop
  deinit

main : IO ()
main = do
  ignore $ collectSignal SigINT
  withNCurses run

