module NCurses.Core.Attribute

import NCurses.Core
import NCurses.Core.Color

%default total

%foreign libncurses "wchgat"
prim__changeAtWindow : AnyPtr -> Int -> Int -> Int -> AnyPtr -> PrimIO ()

%foreign libncurses "mvchgat"
prim__mvChangeAt : Int -> Int -> Int -> Int -> Int -> AnyPtr -> PrimIO ()

%foreign libncurses "mvwchgat"
prim__mvChangeAtWindow : AnyPtr -> Int -> Int -> Int -> Int -> Int -> AnyPtr -> PrimIO ()

%foreign libncurses "attrset"
prim__setAttr : Int -> PrimIO ()

%foreign libncurses "wattrset"
prim__setAttrWindow : AnyPtr -> Int -> PrimIO ()

%foreign libncurses "attroff"
prim__disableAttr : Int -> PrimIO ()

%foreign libncurses "wattroff"
prim__disableAttrWindow : AnyPtr -> Int -> PrimIO ()

%foreign libncurses "attron"
prim__enableAttr : Int -> PrimIO ()

%foreign libncurses "wattron"
prim__enableAttrWindow : AnyPtr -> Int -> PrimIO ()

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
||| In ncurses terminology, "attrset"
|||
||| See @nSetAttr'@ for a version that works on
||| any given window.
export
nSetAttr : HasIO io => Attribute -> io ()
nSetAttr attr = do attribute <- getAttribute attr
                   primIO $ prim__setAttr attribute

||| Set an attribute to be applied in the given window
||| until it is cleared or overwritten.
|||
||| In ncurses terminology, "wattrset"
export
nSetAttr' : HasIO io => Window -> Attribute -> io ()
nSetAttr' (Win win) attr = do attribute <- getAttribute attr
                              primIO $ prim__setAttrWindow win attribute

||| Set the given attribute in the standard window
||| until it is set again. This has no impact
||| on any other attributes that are set.
|||
||| In ncurses terminology, "attron"
|||
||| See @nEnableAttr'@ for a version that works on
||| any given window.
export
nEnableAttr : HasIO io => Attribute -> io ()
nEnableAttr attr = do attribute <- getAttribute attr
                      primIO $ prim__enableAttr attribute

||| Set the given attribute in the given window
||| until it is set again. This has no impact
||| on any other attributes that are set.
|||
||| In ncurses terminology, "wattron"
export
nEnableAttr' : HasIO io => Window -> Attribute -> io ()
nEnableAttr' (Win win) attr = do attribute <- getAttribute attr
                                 primIO $ prim__enableAttrWindow win attribute

||| Unset the given attribute in the standard window
||| until it is set again. This has no impact
||| on any other attributes that are set.
|||
||| In ncurses terminology, "attroff"
|||
||| See @nDisableAttr'@ for a version that works on
||| any given window.
export
nDisableAttr : HasIO io => Attribute -> io ()
nDisableAttr attr = do attribute <- getAttribute attr
                       primIO $ prim__disableAttr attribute

||| Unset the given attribute in the given window
||| until it is set again. This has no impact
||| on any other attributes that are set.
|||
||| In ncurses terminology, "wattroff"
export
nDisableAttr' : HasIO io => Window -> Attribute -> io ()
nDisableAttr' (Win win) attr = do attribute <- getAttribute attr
                                  primIO $ prim__disableAttrWindow win attribute

||| Change the attributes at the given position in the standard window.
||| A len of Nothing means "the whole line."
||| A color pair of @defaultColorPair@ offers a sane default.
|||
||| See @nChangeAttr'@ to change attributes in another window.
export
nChangeAttrAt : HasIO io 
            => (row : Nat) 
            -> (col : Nat) 
            -> (len : Maybe Nat) 
            -> Attribute 
            -> ColorPair
            -> io ()
nChangeAttrAt row col len attr cp = 
  let length = the Int (maybe (-1) cast len)
  in  
      do attribute <- getAttribute attr
         primIO $ 
           prim__mvChangeAt (cast row) (cast col) length attribute (cast cp.idx) prim__getNullAnyPtr

||| Change the attributes in the given window for the next @len@ characters.
||| A len of Nothing means "the whole line."
||| A color pair of @defaultColorPair@ offers a sane default.
export
nChangeAttr' : HasIO io 
            => Window
            -> (len : Maybe Nat) 
            -> Attribute 
            -> ColorPair
            -> io ()
nChangeAttr' (Win win) len attr cp = 
  let length = the Int (maybe (-1) cast len)
  in  
      do attribute <- getAttribute attr
         primIO $ 
           prim__changeAtWindow win length attribute (cast cp.idx) prim__getNullAnyPtr

||| Change the attributes at the given position in the given window.
||| A len of Nothing means "the whole line."
||| A color pair of @defaultColorPair@ offers a sane default.
export
nChangeAttrAt' : HasIO io 
              => Window
              -> (row : Nat) 
              -> (col : Nat) 
              -> (len : Maybe Nat) 
              -> Attribute 
              -> ColorPair
              -> io ()
nChangeAttrAt' (Win win) row col len attr cp = 
  let length = the Int (maybe (-1) cast len)
  in  
      do attribute <- getAttribute attr
         primIO $ 
           prim__mvChangeAtWindow win (cast row) (cast col) length attribute (cast cp.idx) prim__getNullAnyPtr

