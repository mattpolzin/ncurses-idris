||| A program that draws a pumpkin.
|||
||| Massive credit to @gallais who originally created the following pumpkin with a fork of
||| the NCurses library in an example that can be found here:
||| https://github.com/gallais/ncurses-idris/blob/main/examples/Pumpkin.idr
module Main

import System
import System.Signal
import Control.NCurses
import Control.NCurses.Pretty
import Data.Nat
import Data.Maybe
import Data.String
import Control.Indexed

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

instructions : String
instructions = indent ((width `minus` (length unpadded)) `div` 2) unpadded 
  where
    unpadded : String
    unpadded = "Ctrl+C to quit. 't' to toggle eyes!"

drawLines : IsActive s => (color : String) -> HasColor color s => List Line -> NCurses () s s
drawLines color xs = setAttr (Color (Named color)) >> traverse_ drawLine xs
  where
    drawLine : (Nat, Nat, Nat) -> NCurses () s s
    drawLine (row, col, len) = do
      move $ MkPosition {row, col}
      drawHorizontalLine ' ' len

loop : IsActive s => NoKeypad s => YesDelay s => HasColor "black" s => HasColor "red" s => NCurses () s s
loop = do
  interactive False
  where
    eyes : (color : String) -> HasColor color s => NCurses () s s
    eyes color = drawLines color eyeLines

    toggle : Char -> Bool -> Bool
    toggle 't' y = not y
    toggle _ y = y

    interactive : Bool -> NCurses () s s
    interactive on = Indexed.Do.do
      ch <- Char.getChar
      -- turn eyes on/off when 't' is pressed
      if (toggle ch on) then eyes "red" else eyes "black"
      refresh
      case !(liftIO handleNextCollectedSignal) of
           (Just SigINT) => pure ()
           _ => interactive (toggle ch on)

run : NCurses () Inactive Inactive
run = Indexed.Do.do
  init
  (MkSize rows cols) <- getSize True
  let centerX = (cols `div` 2) `minus` (width `div` 2)
  let centerY = (rows `div` 2) `minus` (height `div` 2)
  setCursor CInvisible
  addColor "orange" Black Yellow
  addColor "green"  Green Green
  addColor "black"  Black Black
  addColor "red"    Red Red
  addColor "debug"  Red Black
  addWindow "instructions" (MkPosition centerY centerX) (MkSize 1 width) Nothing
  addWindow "pumpkin" (MkPosition (centerY + 2) centerX) (MkSize height width) Nothing
  clear
  setWindow "instructions"
  putStr instructions
  refresh
  setWindow "pumpkin"
  setKeypad False
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

