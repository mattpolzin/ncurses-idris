module Main

import System
import System.Signal
import Control.NCurses
import Control.NCurses.Pretty
import Data.Nat
import Data.Maybe

debugLine : IsActive s => (color : String) -> HasColor color s => (msg : String) -> NCurses () s (const s)
debugLine c msg = do
  move (MkPosition 0 0)
  printDoc $
    color c $ pretty msg

Line : Type
Line = (Nat, Nat, Nat)

||| List of horizontal lines with given row, col, and height
stalkLines : List Line
stalkLines = [ (0, 18, 4)
             , (1, 20, 4)
             , (2, 20, 4)
             , (3, 20, 6)
             , (4, 18, 10)
             , (5, 18, 10)
             ]

||| List of horizontal lines with given row, col, and height
bodyLines : List Line
bodyLines = [ (3,  8,  8)
            , (3,  28, 8)
            , (4,  6,  12)
            , (4,  28, 10)
            , (5,  4,  14)
            , (5,  20, 2)
            , (5,  24, 2)
            , (5,  28, 12)
            , (6,  2,  40)
            , (7,  2,  40)
            , (8,  0,  44)
            , (9,  0,  44)
            , (10, 0,  44)
            , (11, 0,  44)
            , (12, 0,  44)
            , (13, 0,  44)
            , (14, 2,  40)
            , (15, 2,  40)
            , (16, 4,  36)
            , (17, 6,  30)
            , (18, 10, 10)
            , (18, 24, 10)
            ]

||| List of horizontal lines with given row, col, and height
holeLines : List Line
holeLines = [ (7,  14, 2)
            , (7,  30, 2)
            , (8,  12, 6)
            , (8,  28, 6)
            , (9,  10, 10)
            , (9,  22, 2)
            , (9,  26, 10)
            , (10, 20, 6)
            , (11, 18, 10)
            , (13, 10, 6)
            , (13, 20, 8)
            , (13, 32, 6)
            , (14, 12, 24)
            , (15, 14, 20)
            , (16, 16, 6)
            , (16, 26, 4)
            ]

eyeLines : List Line
eyeLines = [ (8, 14, 2)
           , (8, 30, 2)
           ]

width : Nat
width = 44

height : Nat
height = 18

drawLines : IsActive s => (color : String) -> HasColor color s => List Line -> NCurses () s (const s)
drawLines color xs = setAttr (Color color) >> traverse_ drawLine xs
  where
    drawLine : (Nat, Nat, Nat) -> NCurses () s (const s)
    drawLine (row, col, len) = do
      move $ MkPosition {row, col}
      drawHorizontalLine ' ' len

loop : IsActive s => NoKeypad s => NoDelay s => HasColor "debug" s => HasColor "black" s => HasColor "red" s => NCurses () s (const s)
loop = do
  interactive 0
  where
    eyes : (color : String) -> HasColor color s => NCurses () s (const s)
    eyes color = drawLines color eyeLines

    interactive : Nat -> NCurses () s (const s)
    interactive n = TransitionIndexed.Do.do
      ch <- getChar
      -- turn eyes on or off if any key is currently pressed or not
      if isJust ch then eyes "red" else eyes "black"
      debugLine "debug" (if isJust ch then "\{show n} keeeey" else "\{show n} noooo key")
      refresh
--       liftIO $ sleep 1
      case !(liftIO handleNextCollectedSignal) of
           (Just SigINT) => pure ()
           _ => interactive (S n)


run : NCurses () Inactive (const Inactive)
run = TransitionIndexed.Do.do
  init
  (MkSize rows cols) <- getSize
  let centerX = (cols `div` 2) `minus` (width `div` 2)
  let centerY = (rows `div` 2) `minus` (height `div` 2)
  setCursor CInvisible
  addColor "orange" Black Yellow
  addColor "green"  Green Green
  addColor "black"  Black Black
  addColor "red"    Red Red
  addColor "debug"  Red Black
  addWindow "pumpkin" (MkPosition centerY centerX) (MkSize height width)
  clear
  setWindow "pumpkin"
  setKeypad False
  setNoDelay True
  drawLines "green"  stalkLines 
  drawLines "orange" bodyLines
  drawLines "black" holeLines
  refresh
  loop
  deinit

main : IO ()
main = do
  ignore $ collectSignal SigINT
  withNCurses run
