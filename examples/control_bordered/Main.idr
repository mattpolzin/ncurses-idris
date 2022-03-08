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
run = Indexed.Do.do
  init
  clear
  addColor "green" Green Black
  addColor "red" Black Red
  addWindow "fst" (MkPosition 0 0) (MkSize 8 8) (defaultBorder "green")
  setWindow "fst"
  putStrLn "a"
  putStrLn "b"
  putStrLn "c"
  move (MkPosition 3 4)
  putStr "hello you"
  addWindow "snd" (MkPosition 9 0) (MkSize 8 4) (border "red" (Custom '{') (Custom '}') (Custom '=') (Custom '-') (Custom '*') (Custom '#') (Custom '+') (Custom '/'))
  setWindow "snd"
  putStrLn "a"
  putStrLn "bcd"
  unsetWindow
  refreshAll
  loop
  deinit

main : IO ()
main = do
  ignore $ collectSignal SigINT
  withNCurses run

