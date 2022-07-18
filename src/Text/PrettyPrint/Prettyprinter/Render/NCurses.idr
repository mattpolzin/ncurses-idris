module Text.PrettyPrint.Prettyprinter.Render.NCurses

import Control.Monad.State
import Data.String
import NCurses.Core
import NCurses.Core.Attribute
import NCurses.Core.Color
import Text.PrettyPrint.Prettyprinter.Doc
import Text.PrettyPrint.Prettyprinter.Render.Terminal

%default total

export
color : ColorPair -> Doc Attribute -> Doc Attribute
color colorPair = annotate (CP colorPair)

export
underline : Doc Attribute -> Doc Attribute
underline = annotate Underline

export
standout : Doc Attribute -> Doc Attribute
standout = annotate Standout

export
reverse : Doc Attribute -> Doc Attribute
reverse = annotate Reverse

export
blink : Doc Attribute -> Doc Attribute
blink = annotate Blink

export
dim : Doc Attribute -> Doc Attribute
dim = annotate Dim

export
bold : Doc Attribute -> Doc Attribute
bold = annotate Bold

export
protected : Doc Attribute -> Doc Attribute
protected = annotate Protected

export
invisible : Doc Attribute -> Doc Attribute
invisible = annotate Invisible

||| Map NCurses attributes to ANSI styles.
||| This allows a Doc to be written with NCurses styles and
||| displayed in a terminal without NCurses as a fallback or
||| alternative.
export
toANSI : Attribute -> AnsiStyle
toANSI Normal = [Reset]
toANSI Underline = [SetStyle SingleUnderline]
toANSI Standout = [SetStyle DoubleUnderline]
toANSI Reverse = []
toANSI Blink = [SetBlink Slow]
toANSI Dim = [SetStyle Faint]
toANSI Bold = [SetStyle Bold]
toANSI Protected = []
toANSI Invisible = []
toANSI (CP colorPair) = [ SetForeground (toSgr colorPair.foreground)
                        , SetBackground (toSgr colorPair.background)
                        ]
  where
    toSgr : Color.Color -> SGR.Color
    toSgr Black   = Black
    toSgr Red     = Red
    toSgr Green   = Green
    toSgr Yellow  = Yellow
    toSgr Blue    = Blue
    toSgr Magenta = Magenta
    toSgr Cyan    = Cyan
    toSgr White   = White

AttrState : Type
AttrState = StateT (List Attribute) IO ()

export
renderNCurses : HasIO io => SimpleDocStream Attribute -> io ()
renderNCurses = liftIO . evalStateT [] . go
  where
    go : SimpleDocStream Attribute -> AttrState
    go SEmpty               = pure ()
    go (SChar ch rest)      = lift (nPutCh ch) *> go rest
    go (SText _ text rest)  = lift (nPutStr text) *> go rest
    go (SLine i rest)       = do
      y <- lift $ getYPos
      lift $ nMoveCursor (S y) (cast i)
      go rest
    go (SAnnPop rest)       = do
      (last :: attrs) <- get
        | [] => go rest
      lift $ nDisableAttr last
      put attrs
      go rest
    go (SAnnPush attr rest) = do
      attrs <- get
      if (head' attrs == Just attr)
         then go rest
         else do
           lift $ nEnableAttr attr
           modify (attr ::)
           go rest

