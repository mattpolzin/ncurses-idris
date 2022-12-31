||| Tetris

-- TODO:
-- * `Drop` action
-- * `Game over` detection

module Main

import System
import System.Signal
import System.Random
import Control.NCurses
import Control.NCurses.Pretty
import Data.Nat
import Data.Maybe
import Data.String
import Data.Vect
import Control.Indexed
import Control.Monad.Indexed.State

------------------------------------------------------------------------
-- Library code

compose : Nat -> (a -> a) -> (a -> a)
compose Z f = id
compose (S n) f = f . compose n f

Matrix : Nat -> Nat -> Type -> Type
Matrix m n a = Vect m (Vect n a)

setAt : Fin m -> Fin n -> a -> Matrix m n a -> Matrix m n a
setAt k l v = updateAt k (updateAt l (const v))

forVect_ :
  IndexedApplicative z f =>
  Vect m a ->
  (Fin m -> a -> f b i i) -> f () i i
forVect_ [] f = pure ()
forVect_ (x :: xs) f = f 0 x *>> forVect_ xs (f . FS)

forMatrix_ :
  IndexedApplicative z f =>
  Matrix m n a ->
  (Fin m -> Fin n -> a -> f b i i) -> f () i i
forMatrix_ mat f
  = forVect_ mat $ \ k, row =>
    forVect_ row (f k)

||| Draw a pixel (takes 2 characters wide to actually get squares)
pixel :
  IsActive s =>
  (col : String) -> HasColor col s =>
  Position -> NCurses () s s
pixel color pos = do
  setAttr (Color (Named color))
  let pos = { col $= (2*) } pos
  move pos
  -- defensively programming against escaping the bounds
  MkSize rows cols <- getWindowSize True
  ifThenElse ((cols `minus` col pos) < 2)
    (putCh ' ')
    (putStr "  ")

------------------------------------------------------------------------
-- Game components

||| Height of the game arena
gameHeight : Nat
gameHeight = 20

||| Width of the game arena
gameWidth : Nat
gameWidth = 10

||| Colours of the pieces
data Colour
  = Red | Blue | Green | Orange | White -- actual colours of interest
  | Black -- used to paint out a piece

||| Game board
Board : Type
Board = Matrix gameHeight gameWidth (Maybe Colour)

initBoard : Board
initBoard = replicate gameHeight (replicate gameWidth Nothing)

||| A sprite is a matrix of boolean values describing whether each pixel
||| is filled in.
||| Invariant: at least one `True` cell per line & column (i.e. the sprite is
||| as small as possible)
Sprite : Nat -> Nat -> Type
Sprite m n = Matrix m n Bool

||| An `L`-shaped piece
anL : Sprite 3 2
anL = [ [True, False]
      , [True, False]
      , [True, True]
      ]

||| A `J`-shaped piece (aka mirrored L)
aJ : Sprite 3 2
aJ = [ [False, True]
     , [False, True]
     , [True, True]
     ]

||| An (reversed) 'S'-shaped piece
anS : Sprite 3 2
anS = [ [True, False]
      , [True, True]
      , [False, True]
      ]

||| A 't'-shaped piece
aT : Sprite 3 2
aT = [ [True, False]
     , [True, True]
     , [True, False]
     ]

||| A straight piece
anI : Sprite 4 1
anI = replicate 4 [True]

||| A square piece
anO : Sprite 2 2
anO = replicate 2 (replicate 2 True)

nextColour : Colour -> Colour
nextColour Red = Blue
nextColour Blue = Green
nextColour Green = Orange
nextColour Orange = White
nextColour White = Red
nextColour Black = Red

newColour : IO Colour
newColour = pure $ case !(randomRIO {a = Int32} (1, 5)) of
    1 => Red
    2 => Blue
    3 => Green
    4 => Orange
    _ => White

||| An element is described by:
|||  + its color
|||  + its associated sprite
|||  + the position of its top-left corner
record Element where
  constructor MkElement
  colour : Colour
  -- These width & height are used to check whether the element is in bounds
  -- They are (implicitly) modified when we `rotate` an element so it's crucial
  -- to ensure they do correspond to the sprite size
  {width : Nat}
  {height : Nat}
  sprite : Sprite height width
  topLeft : Position

||| List of shapes (used for testing purposes, cf. `test` function)
shapes : Colour -> Nat -> List Element
shapes c n =
  let col := 5 * S n in
  [ MkElement c anL (MkPosition 6 col)
  , MkElement c anS (MkPosition 11 col)
  , MkElement c aT  (MkPosition 16 col)
  , MkElement c anI (MkPosition 21 col)
  , MkElement c anO (MkPosition 26 col)
  ]

------------------------------------------------------------------------
-- Game logic

||| Check whether a piece is in bounds.
||| Useful to check whether a move, or a rotation is valid
inBounds : Element -> Bool
inBounds (MkElement _ {width, height} _ topLeft)
  = (col topLeft + width <= gameWidth)
  && (row topLeft + height <= gameHeight)

||| Rotate an element (the width & height are implicit swapped here)
rotate : Element -> Element
rotate = { sprite $= map reverse . transpose }

||| Manufacturing new elements
newElement : IO Element
newElement = do
  col <- newColour
  let pos = MkPosition 0 4
  let mk : {h, w : Nat} -> Sprite h w -> Element
      := \ spr => MkElement col spr pos
  let elt = case !(randomRIO {a = Int32} (1, 6)) of
              1 => mk anL
              2 => mk aJ
              3 => mk anS
              4 => mk aT
              5 => mk anI
              _ => mk anO
  let rot : Nat = cast !(randomRIO {a = Int32} (0,3))
  pure (compose rot rotate elt)

||| Possible user inputs
data Operation = MoveLeft | MoveRight | Rotate
data Action = Quit | Transform Operation

||| Non-blocking reading of user inputs
getAction :
  IsActive s => YesKeypad s => NoDelay s =>
  NCurses (Maybe Action) s s
getAction = getKeyOrChar >>= \ input => pure $ case input of
  Just (Left 'q') => Just Quit
  Just (Left 'r') => Just (Transform Rotate)
  Just (Right Left) => Just (Transform MoveLeft)
  Just (Right Right) => Just (Transform MoveRight)
  _ => Nothing

||| Perform the described transformation
transform : Operation -> Element -> Maybe Element
transform ope curr = do
  let next = case ope of
               MoveLeft => { topLeft->col $= pred } curr
               MoveRight => { topLeft->col $= S } curr
               Rotate => rotate curr
  next <$ guard (inBounds next)


-- TODO: better invariants
unsafeCheck : Nat -> Nat -> Sprite h w -> Matrix m n (Maybe a) -> Bool
unsafeCheck Z Z spr bd = go2 spr bd where

  -- go, in 1D
  go1 : forall w, n. Vect w Bool -> Vect n (Maybe a) -> Bool
  go1 [] _ = True
  go1 (v :: spr) (c :: bd)
    -- a sprite line can fit if
    -- either the sprite does not cover this cell, or the cell is empty
    = (not v || isNothing c)
    -- the rest of the lines are also compatible
      && go1 spr bd
  go1 _ _ = False -- weird out of bounds error

  -- go, in 2D
  go2 : forall h, w, m, n. Sprite h w -> Matrix m n (Maybe a) -> Bool
  go2 [] _ = True
  go2 (l1 :: spr) (l2 :: bd) = go1 l1 l2 && go2 spr bd
  go2 _ _ = False -- weird out of bounds error

unsafeCheck Z (S l) spr bd@((_ :: _) :: _) = unsafeCheck Z l spr (map tail bd)
unsafeCheck (S k) l spr (_ :: bd) = unsafeCheck k l spr bd
unsafeCheck _ _ _ _ = False -- weird out of bounds error

record CleanedUp (m, n : Nat) (a : Type) where
  constructor MkCleanedUp
  removed : Nat
  {leftover : Nat}
  sumsTo : removed + leftover === m
  cleanedUp : Matrix leftover n (Maybe a)

cleanUp : {m : Nat} -> Matrix m n (Maybe a) -> CleanedUp m n a
cleanUp [] = MkCleanedUp 0 Refl []
cleanUp (x :: xs)
  = let MkCleanedUp removed eq xs' = cleanUp xs in
    let eq = cong S eq in
    if all isJust x
    then MkCleanedUp (S removed) eq xs'
    else let eq = transitive (symmetric (plusSuccRightSucc _ _)) eq in
         MkCleanedUp removed eq (x :: xs')

bingos : Board -> Maybe (Nat, Board)
bingos bd = case cleanUp bd of
  MkCleanedUp Z _ _ => Nothing
  MkCleanedUp removed eq xs => pure $ (removed,) $
    rewrite sym eq in
    replicate _ (replicate _ Nothing) ++ xs

------------------------------------------------------------------------
-- Game state

record TetrisState where
  constructor MkTetrisState
  element : Element
  board : Board
  score : Nat

initTetrisState : IO TetrisState
initTetrisState
  = pure
  $ MkTetrisState
    !newElement
    initBoard
    0

State : Type -> CursesState -> CursesState -> Type
State = IndexedStateT TetrisState CursesState CursesState NCurses

---------------------------------------------------------------------------
-- Frame rate

-- We call crucial frame the frame on which we step the geometric shape down
-- In between crucial frames, there are additional action frames were we only
-- handle the user inputs

||| Pause between two crucial frames
pauseTime : Int
pauseTime = 250_000

||| We have 5 times as many action frames as we have crucial ones
actionFrames : Nat
actionFrames = 5

||| Decide whether we have just hit a crucial frame,
||| and return the remaining number of action frames before we reach the next one
isCrucialFrame : Nat -> (Bool, Nat)
isCrucialFrame Z = (True, actionFrames)
isCrucialFrame (S n) = (False, n)


---------------------------------------------------------------------------
-- Render an element

parameters
  {auto act : IsActive s}
  {auto org : HasColor "orange" s}
  {auto red : HasColor "red" s}
  {auto grn : HasColor "green" s}
  {auto blu : HasColor "blue" s}
  {auto wht : HasColor "white" s}
  {auto blk : HasColor "black" s}

  toColor : Colour -> (str : String ** HasColor str s)
  toColor Red = ("red" ** %search)
  toColor Blue = ("blue" ** %search)
  toColor Green = ("green" ** %search)
  toColor Orange = ("orange" ** %search)
  toColor White = ("white" ** %search)
  toColor Black = ("black" ** %search)

  drawElement : Element -> State () s s
  drawElement (MkElement colour sprite topLeft)
    = forMatrix_ sprite $ \ k, l, b =>
        when b $
          let (color ** prf) = toColor colour in
          lift $ pixel color $
             { col $= (cast l +)
             , row $= (cast k +)
             } topLeft

  drawBoard : Board -> State () s s
  drawBoard board
    = forMatrix_ board $ \ k, l, mc => case mc of
        Nothing => pure ()
        Just colour =>
          let (color ** prf) = toColor colour in
          lift $ pixel color (MkPosition (cast k) (cast l))

---------------------------------------------------------------------------
-- Game loop

parameters
  {auto ttr : HasWindow "tetris" s}
  {auto scr : HasWindow "score" s}
  {auto act : IsActive s}
  {auto kpd : YesKeypad s}
  {auto dly : NoDelay s}
  {auto gov : HasColor "meta" s}
  {auto org : HasColor "orange" s}
  {auto red : HasColor "red" s}
  {auto blk : HasColor "black" s}
  {auto grn : HasColor "green" s}
  {auto blu : HasColor "blue" s}
  {auto wht : HasColor "white" s}

  step : Nat -> State () s s
  loop : Nat -> State () s s

  start : State () s s
  start = Indexed.do
    put !(lift $ liftIO initTetrisState)
    lift clear
    drawBoard initBoard
    loop actionFrames

  gameOver : State () s s
  gameOver = do
    lift $ Indexed.do
      MkSize rows cols <- getWindowSize True
      let msg = "Game Over"
      setAttrs [Bold, Color (Named "meta")]
      move $ MkPosition 0 0
      putStr msg
      let restart = "r: restart"
      let quit    = "q: quit"
      let col = (cols `minus` max (length restart) (length quit)) `div` 2
      move $ MkPosition { row = pred (rows `div` 2), col }
      putStr restart
      move $ MkPosition { row = rows `div` 2, col }
      putStr quit
      refresh
    wait

    where

    wait : State () s s
    wait = do
      c <- lift $ liftIO getChar
      case c of
        'q' => pure ()
        'r' => start
        _ => wait

  loop n =
    case !(lift $ liftIO handleNextCollectedSignal) of
         (Just SigINT) => pure ()
         _ => step n

  test : State () s s
  test = Indexed.do
    for_ (with Prelude.(::) [(Z,Red), (1, Orange), (2, Green), (3, Blue)]) $ \ (n, col) =>
      for_ (shapes col n) $ drawElement . compose n rotate
    lift refresh
    gameOver

  step n = Indexed.do
    -- get the element
    curr <- gets element

    -- see whether any actions apply
    next <- map (fromMaybe curr) $ applyAction curr

    -- additionally if we're on a crucial frame, we step the element down
    let (b, n) = isCrucialFrame n
    next <- lift $ ifThenElse b stepElementDown pure next

    -- Before committing to this new position,
    -- we check that it is compatible with the board
    b <- isCompatible next
    next <- ifThenElse b (pure next) (lift $ liftIO newElement)
    modify ({ element := next })

    -- if the new element is compatible we paint out the old element,
    -- otherwise we record it as settled in the game board and look for
    -- completed lines
    ifThenElse b
      (drawElement ({ colour := Black } curr))
      $ do recordElement curr
           modify { score $= (10+) }
           bd <- gets board
           let Just (n, bd) = bingos bd
             | Nothing => pure ()
           let n = ifThenElse (n == 4) 5 n -- bonus for 4 lines
           modify { board := bd, score $= ((100*n)+) }
           lift clear
           drawBoard bd

    -- and then we paint in the new one
    drawElement next
    drawScore
    lift refresh

    -- wait until the next action frame
    lift $ liftIO $ usleep (pauseTime `div` cast actionFrames)
    loop n

     where

       drawScore : State () s s
       drawScore = Indexed.do
         scr <- gets score
         lift $ inWindow "score" $ Indexed.do
           setAttrs @{setWindowIsActiveStill} [Color (Named @{setWindowHasColorStill} "meta")]
           move (MkPosition 0 0)
           let str = show scr
           let pref = " score: "
           let pad = String.replicate (18 `minus` (length str + length pref)) ' '
           putStrLn "\{pref}\{pad}\{str}"
           refresh

       recordElement : Element -> State () s s
       recordElement (MkElement colour sprite (MkPosition row col)) = do
         forMatrix_ sprite $ \ k, l, b =>
           when b $ do
             -- horrible, the `unsafeCheck` should probably return the modified board instead
             -- & we could store it in the state.
             let k = fromMaybe 0 (integerToFin (cast (row + cast k)) _)
             let l = fromMaybe 0 (integerToFin (cast (col + cast l)) _)
             modify { board $= setAt k l (Just colour) }

       isCompatible : Element -> State Bool s s
       isCompatible (MkElement _ spr (MkPosition m n)) = Indexed.do
         bd <- gets board
         pure (unsafeCheck m n spr bd)

       stepElementDown : Element -> NCurses Element s s
       stepElementDown next = do pure ({ topLeft->row $= S } next)
         -- let currRow = row (topLeft next)
         -- pure $ do
         --   guard (currRow + height next < gameHeight)
         --   let next = { topLeft->row := S currRow } next
         --   pure next

       applyAction : Element -> State (Maybe Element) s s
       applyAction curr = do
         Just (Transform ope) <- lift getAction
           | Just Quit => lift (liftIO (Nothing <$ raiseSignal SigINT))
           | Nothing => pure (Just curr)
         let Just next = transform ope curr
           | Nothing => pure Nothing
         b <- isCompatible next
         pure $ next <$ guard b


run : NCurses () Inactive Inactive
run = Indexed.do
  init
  MkSize rows cols <- getWindowSize True
  setCursor CInvisible

  addColor "white"  White White
  addColor "orange" Black Yellow
  addColor "red"    Red Red
  addColor "green"  Green Green
  addColor "black"  Black Black
  addColor "blue"   Cyan Cyan
  addColor "meta"   White Black

  addWindow "tetris" (MkPosition 1 1) (MkSize (2 + 2 * gameWidth) (2 + gameHeight)) (defaultBorder "meta")
  addWindow "score"  (MkPosition 1 (4 + 2 * gameWidth)) (MkSize 3 20) (defaultBorder "meta")

  setWindow "tetris"
  setNoDelay True
  setKeypad True
  refresh
  evalStateT !(liftIO initTetrisState) start
  deinit

main : IO ()
main = do
  ignore $ collectSignal SigINT
  withNCurses run
