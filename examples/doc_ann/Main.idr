module Main

import System
import System.Signal
import NCurses
import Text.PrettyPrint.Prettyprinter.Render.NCurses
import Text.PrettyPrint.Prettyprinter.Render.Terminal
import Text.PrettyPrint.Prettyprinter.Doc

doc : ColorPair -> Doc Attribute
doc cp = vsep $ [ pretty "First line of doc"
                , indent 2 $ hsep [ pretty "Indented"
                                  , color cp (pretty "and in color")
                                  , pretty "and then not in color again."
                                  ]
                             <+> line
                             <+> (bold $
                                    standout (pretty "very") <++> pretty "important.")
                , underline (color cp $ pretty "underlined")
                ]

printDoc : HasIO io => ColorPair -> io ()
printDoc = renderNCurses . layoutPretty defaultLayoutOptions . doc

printDoc' : ColorPair -> IO ()
printDoc' = renderIO . layoutPretty defaultLayoutOptions . map toANSI . doc

loop : HasIO io => io ()
loop =
  case !handleNextCollectedSignal of
       (Just SigINT) => pure ()
       _ => sleep 1 *> loop

main : IO ()
main = do
  putStrLn "STARTING"

  ignore $ collectSignal SigINT
  initNCurses
  startColor
  clear
  nPutStrLn "Ctrl+C to quit."
  nPutStr "\nin ncurses, before doc\n"
  cp <- initColorPair 1 Green Black
  printDoc cp
  nPutStr "\nin ncurses, after doc\n"
  refresh
  loop
  deinitNCurses

  putStrLn "--------"
  liftIO $ printDoc' cp

  putStrLn "DONE"

