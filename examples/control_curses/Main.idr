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

prettyDoc : Doc (Attribute (Active _ _ _ ["red", "alert", "inverse"]))
prettyDoc =
  vsep [ color "red" "-----"
       , pretty "Hello"
       , indent 2 . bold $
           vsep [ pretty "emboldened"
                , color "alert" $ hsep [pretty "Still no cause for", color "inverse" "alarm..."]
                , emptyDoc
                , indent 2 $ "->" <+> line <+> (indent 2 "**")
                ]
       , underline "End of final transmission."
       ]

run : NCurses () Inactive Inactive
run = Indexed.Do.do
  init
  addWindow "main" (MkPosition 0 0) (MkSize 35 45)
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
  addWindow "win1" (MkPosition 35 55) (MkSize 20 30)
  setWindow "win1"
  clear
  setAttr (Color "inverse")
  putStrLn "hello from a window"
  setAttr DefaultColors
  refresh
  setWindow "main"
  -- pretty print
  addColor "red" Red Black
  printDoc $ prettyDoc
  refresh
  loop
  deinit

main : IO ()
main = do
  ignore $ collectSignal SigINT
  withNCurses run

