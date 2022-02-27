module Main

import System
import System.Signal
import Control.NCurses
import Control.NCurses.Pretty
import Data.Nat

displayTicker : IsActive s => InWindow "ticker" s => Nat -> NCurses () s (const s)
displayTicker k = putStr (show k)
--   printDoc $ 
--     color "green" (bold $ pretty k)

loop : IsActive s => InWindow "ticker" s => YesDelay s => YesKeypad s => Nat -> NCurses () s (const s)
loop n = do
  keyOrCh <- getKeyOrChar
  tally keyOrCh
    where
      loopOrExit : Nat -> NCurses () s (const s)
      loopOrExit n' = 
        case !(liftIO handleNextCollectedSignal) of
             (Just SigINT) => pure ()
             _ => loop n'

      tally : Either Char Key -> NCurses () s (const s)
      tally x = do
        clear
        let n' = case x of Right Up => (pred n); Right Down => (S n); _ => n
        displayTicker n'
        refresh
        loopOrExit n'

run : NCurses () Inactive (const Inactive)
run = TransitionIndexed.Do.do
  init
  addColor "green" Green Black
  setCursor CInvisible
  addWindow "header" (MkPosition 0 0) (MkSize 1 50)
  addWindow "ticker" (MkPosition 1 0) (MkSize 1 10)
  clear
  setWindow "header"
  putStrLn "Ctrl+C to quit. Up/Down arrows to adjust ticker.\n"
  refresh
  setWindow "ticker"
  let n = 25
  displayTicker n
  loop n
  deinit

main : IO ()
main = do
  ignore $ collectSignal SigINT
  withNCurses run

