module Main

import System
import System.Signal
import Control.NCurses


loop : HasIO io => io ()
loop =
  case !handleNextCollectedSignal of
       (Just SigINT) => pure ()
       _ => sleep 1 *> loop

main : IO ()
main = do
  ignore $ collectSignal SigINT
  loop

