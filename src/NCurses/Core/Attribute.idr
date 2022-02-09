module NCurses.Core.Attribute

import NCurses.Core
import NCurses.Core.Color

%default total

%foreign libncurses "mvchgat"
prim__mvChangeAt : Int -> Int -> Int -> Int -> Int -> AnyPtr -> PrimIO ()

%foreign libncurses "mvwchgat"
prim__mvChangeAtWindow : AnyPtr -> Int -> Int -> Int -> Int -> Int -> AnyPtr -> PrimIO ()

%foreign libncurses "attrset"
prim__setAttr : Int -> PrimIO ()

%foreign libncurses "wattrset"
prim__setAttrWindow : AnyPtr -> Int -> PrimIO ()

%foreign libhelper "normal_attr"
prim__normalAttr : PrimIO Int

%foreign libhelper "underline_attr"
prim__underlineAttr : PrimIO Int

%foreign libhelper "standout_attr"
prim__standoutAttr : PrimIO Int

%foreign libhelper "reverse_attr"
prim__reverseAttr : PrimIO Int

%foreign libhelper "blink_attr"
prim__blinkAttr : PrimIO Int

%foreign libhelper "dim_attr"
prim__dimAttr : PrimIO Int

%foreign libhelper "bold_attr"
prim__boldAttr : PrimIO Int

%foreign libhelper "protected_attr"
prim__protectedAttr : PrimIO Int

%foreign libhelper "invisible_attr"
prim__invisibleAttr : PrimIO Int

%foreign libhelper "color_pair_attr"
prim__colorPairAttr : Int -> PrimIO Int

||| Attributes that can be given to text within an ncurses window.
public export
data Attribute = Normal
               | Underline
               | Standout
               | Reverse
               | Blink
               | Dim
               | Bold
               | Protected
               | Invisible
               | CP ColorPair

||| Get the Int representation ncurses cares about for a
||| particular @Attribute@.
getAttribute : HasIO io => Attribute -> io Int
getAttribute attr = case attr of
                         Normal    => primIO $ prim__normalAttr
                         Underline => primIO $ prim__underlineAttr
                         Standout  => primIO $ prim__standoutAttr
                         Reverse   => primIO $ prim__reverseAttr
                         Blink     => primIO $ prim__blinkAttr
                         Dim       => primIO $ prim__dimAttr
                         Bold      => primIO $ prim__boldAttr
                         Protected => primIO $ prim__protectedAttr
                         Invisible => primIO $ prim__invisibleAttr
                         (CP cp) => primIO $ prim__colorPairAttr (cast cp.idx)

||| Set an attribute to be applied in the standard window
||| until it is cleared or overwritten.
|||
||| See @nSetAttr'@ for a version that works on
||| any given window.
export
nSetAttr : HasIO io => Attribute -> io ()
nSetAttr attr = do attribute <- getAttribute attr
                   primIO $ prim__setAttr attribute

||| Set an attribute to be applied in the given window
||| until it is cleared or overwritten.
export
nSetAttr' : HasIO io => Window -> Attribute -> io ()
nSetAttr' (Win win) attr = do attribute <- getAttribute attr
                              primIO $ prim__setAttrWindow win attribute

||| Change the attributes at the given position in the standard window.
||| A len of Nothing means "the whole line."
||| A color pair with @defaultColorPair@ offering a sane default.
|||
||| See @nChangeAttr'@ to change attributes in another window.
export
nChangeAttr : HasIO io 
           => (row : Nat) 
           -> (col : Nat) 
           -> (len : Maybe Nat) 
           -> Attribute 
           -> ColorPair
           -> io ()
nChangeAttr row col len attr cp = 
  let length = the Int (maybe (-1) cast len)
  in  
      do attribute <- getAttribute attr
         primIO $ 
           prim__mvChangeAt (cast row) (cast col) length attribute (cast cp.idx) prim__getNullAnyPtr

||| Change the attributes at the given position in the given window.
||| A len of Nothing means "the whole line."
||| A color pair with @defaultColorPair@ offering a sane default.
export
nChangeAttr' : HasIO io 
            => Window
            -> (row : Nat) 
            -> (col : Nat) 
            -> (len : Maybe Nat) 
            -> Attribute 
            -> ColorPair
            -> io ()
nChangeAttr' (Win win) row col len attr cp = 
  let length = the Int (maybe (-1) cast len)
  in  
      do attribute <- getAttribute attr
         primIO $ 
           prim__mvChangeAtWindow win (cast row) (cast col) length attribute (cast cp.idx) prim__getNullAnyPtr

