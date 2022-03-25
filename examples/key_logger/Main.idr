module Main

import System
import System.Signal
import NCurses
import Text.PrettyPrint.Prettyprinter.Render.NCurses
import Text.PrettyPrint.Prettyprinter.Render.Terminal
import Text.PrettyPrint.Prettyprinter.Doc

loop : HasIO io => io ()
loop = do
  key <- safeGetCh
  case key of
       Nothing => nPutStrLn " ERROR"
       Just c  => nPutStrLn " char: \{show c}, int: \{show (the Int (cast c))}"
  case !handleNextCollectedSignal of
       (Just SigINT) => pure ()
       _ => loop

main : IO ()
main = do
  ignore $ collectSignal SigINT
  initNCurses
  startColor
  keypad True
  clear
  nPutStrLn "Ctrl+C to quit."
  nPutStrLn "F0: \{show !(fnKeyChar F0)}"
  nPutStrLn "F1: \{show !(fnKeyChar F0)}"
  nPutStrLn "F2: \{show !(fnKeyChar F2)}"
  nPutStrLn "F3: \{show !(fnKeyChar F3)}"
  nPutStrLn "F4: \{show !(fnKeyChar F4)}"
  nPutStrLn "F5: \{show !(fnKeyChar F5)}"
  nPutStrLn "F6: \{show !(fnKeyChar F6)}"
  nPutStrLn "F7: \{show !(fnKeyChar F7)}"
  nPutStrLn "F8: \{show !(fnKeyChar F8)}"
  nPutStrLn "F9: \{show !(fnKeyChar F9)}"
  nPutStrLn "F10: \{show !(fnKeyChar F10)}"
  nPutStrLn "F11: \{show !(fnKeyChar F11)}"
  nPutStrLn "F12: \{show !(fnKeyChar F12)}"
  nPutStrLn "UP: \{show !(fnKeyChar Up)}"
  nPutStrLn "DOWN: \{show !(fnKeyChar Down)}"
  nPutStrLn "LEFT: \{show !(fnKeyChar Left)}"
  nPutStrLn "RIGHT: \{show !(fnKeyChar Right)}"
  nPutStrLn "BACKSPACE: \{show !(fnKeyChar Backspace)}"
  refresh
  loop
  deinitNCurses

