||| Support for contrib's Prettyprinter rendering
||| in the context of an NCurses session.
module Control.NCurses.Pretty

import Text.PrettyPrint.Prettyprinter.Render.NCurses
import Control.NCurses
import NCurses
import Control.Monad.State
import public Text.PrettyPrint.Prettyprinter.Doc
import Control.Indexed

export
color : (name : String) -> HasColor name s => Doc (Attribute s) -> Doc (Attribute s)
color name = annotate (Color name)

export
underline : Doc (Attribute s) -> Doc (Attribute s)
underline = annotate Underline

export
standout : Doc (Attribute s) -> Doc (Attribute s)
standout = annotate Standout

export
reverse : Doc (Attribute s) -> Doc (Attribute s)
reverse = annotate Reverse

export
blink : Doc (Attribute s) -> Doc (Attribute s)
blink = annotate Blink

export
dim : Doc (Attribute s) -> Doc (Attribute s)
dim = annotate Dim

export
bold : Doc (Attribute s) -> Doc (Attribute s)
bold = annotate Bold

export
protected : Doc (Attribute s) -> Doc (Attribute s)
protected = annotate Protected

export
invisible : Doc (Attribute s) -> Doc (Attribute s)
invisible = annotate Invisible

record Const (s : CursesState) a where
  constructor C
  runConst : NCurses a s (const s)

Functor (Const s) where
  map f = C . map f . runConst

Applicative (Const s) where
  pure = C . pure
  (C x) <*> (C y) = C (x <*> y)

Monad (Const s) where
  (C x) >>= f = C $ do
    x' <- x
    runConst $ f x'

Attrs : CursesState -> Type
Attrs s = List (Attribute s)

AttrState : CursesState -> Type
AttrState s = StateT (Attrs s) (Const s) ()

export
renderDoc : IsActive s => SimpleDocStream (Attribute s) -> NCurses () s (const s)
renderDoc = runConst . evalStateT [] . go
  where
    go : SimpleDocStream (Attribute s) -> AttrState s
    go SEmpty = pure ()
    go (SChar ch rest)      = (lift $ C (putCh ch)) *> go rest
    go (SText _ text rest)  = (lift $ C (putStr text)) *> go rest
    go (SLine i rest)       = do
      (MkPosition row col) <- lift $ C getPos
      lift . C $ move (MkPosition (S row) (cast i))
      go rest
    go (SAnnPop rest)       = do
      (last :: attrs) <- get
        | [] => go rest
      lift . C $ disableAttr last
      go rest
    go (SAnnPush attr rest) = do
      lift . C $ enableAttr attr
      modify (attr ::)
      go rest

export
printDoc : IsActive s => Doc (Attribute s) -> NCurses () s (const s)
printDoc = renderDoc . layoutPretty defaultLayoutOptions

