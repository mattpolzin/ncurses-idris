||| Tetris

-- TODO:
--

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

||| Board buffer zone: pieces don't appear fully formed in the play area,
||| they are slowly lowered from above.
||| This size is the largest height of all sprites.
gameBuffer : Nat
gameBuffer = 4

boardHeight : Nat
boardHeight = gameHeight + gameBuffer

||| Colours of the pieces
data Colour
  = Red | Blue | Green | Orange | White -- actual colours of interest
  | Black -- used to paint out a piece

||| Game board
Board : Type
Board = Matrix boardHeight gameWidth (Maybe Colour)

initBoard : Board
initBoard = replicate _ (replicate _ Nothing)

||| A sprite is a matrix of boolean values describing whether each pixel
||| is filled in.
||| Invariant: at least one `True` cell per line & column (i.e. the sprite is
||| as small as possible)
Sprite : Nat -> Nat -> Type
Sprite m n = Matrix m n Bool

rows : {h : Nat} -> Sprite h w -> Nat
rows {h} _ = h

cols : {w : Nat} -> Sprite h w -> Nat
cols {w} _ = w

||| An `L`-shaped piece
anL : Sprite 3 2
anL = [ [True, False]
      , [True, False]
      , [True, True]
      ]

||| A `J`-shaped piece (aka mirrored L)
aJ : Sprite 3 2
aJ = map reverse anL

||| An 'S'-shaped piece
anS : Sprite 3 2
anS = [ [True, False]
      , [True, True]
      , [False, True]
      ]

||| An (reversed) 'S'-shaped piece
a4 : Sprite 3 2
a4 = map reverse anS


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
  && (row topLeft + height <= boardHeight)

||| Rotate an element (the width & height are implicit swapped here)
rotate : Element -> Element
rotate = { sprite $= map reverse . transpose }

||| Manufacturing new elements
newElement : IO Element
newElement = do
  col <- newColour
  let mk : {h, w : Nat} -> Sprite h w -> Element
      := \ spr => MkElement col spr (MkPosition 0 0)
  let elt = case !(randomRIO {a = Int32} (1, 7)) of
              1 => mk anL
              2 => mk aJ
              3 => mk anS
              4 => mk a4
              5 => mk aT
              6 => mk anI
              _ => mk anO
  let rot : Nat = cast !(randomRIO {a = Int32} (0,3))
  let elt := compose rot rotate elt
  -- we move the starting position based on the final shape
  -- of the element so that it starts just above the game area
  -- and centered
  let spr := sprite elt
  let pos := MkPosition
               (gameBuffer `minus` rows spr)
               (5 `minus` (S (cols spr) `div` 2))
  pure ({ topLeft := pos } elt)

||| Possible user inputs
data Operation = MoveLeft | MoveRight | Rotate
data Action = Quit | Drop | Transform Operation

||| Non-blocking reading of user inputs
getAction :
  IsActive s => YesKeypad s => NoDelay s =>
  NCurses (Maybe Action) s s
getAction = getKeyOrChar >>= \ input => pure $ case input of
  Just (Left ' ') => Just Drop
  Just (Left 'q') => Just Quit
  Just (Left 'r') => Just (Transform Rotate)
  Just (Right Left) => Just (Transform MoveLeft)
  Just (Right Right) => Just (Transform MoveRight)
  Just (Right Down) => Just Drop
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
  lines : Nat
  queued : Element

initTetrisState : IO TetrisState
initTetrisState
  = pure
  $ MkTetrisState
    !newElement
    initBoard
    0 0
    !newElement

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
||| Nothing indicates the user has called the `Drop` command and so all
||| frames are crucial until the element finds its place
isCrucialFrame : Maybe Nat -> (Bool, Maybe Nat)
isCrucialFrame Nothing = (True, Nothing)
isCrucialFrame (Just Z) = (True, Just actionFrames)
isCrucialFrame (Just (S n)) = (False, Just n)


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

  drawElement : Element -> NCurses () s s
  drawElement (MkElement colour sprite topLeft)
    = forMatrix_ sprite $ \ k, l, b =>
        let row = row topLeft + cast k in
        when (b && row >= gameBuffer) $
          let (color ** prf) = toColor colour in
          pixel color $
             { col $= (cast l +)
             , row := row `minus` gameBuffer
             } topLeft

  drawBoard : Board -> NCurses () s s
  drawBoard board
    = forMatrix_ (drop 4 board) $ \ k, l, mc => case mc of
        Nothing => pure ()
        Just colour =>
          let (color ** prf) = toColor colour in
          pixel color (MkPosition (cast k) (cast l))

parameters
  {auto act : IsActive s}
  {auto scr : HasWindow "meta" s}
  {auto org : HasColor "orange" s}
  {auto red : HasColor "red" s}
  {auto grn : HasColor "green" s}
  {auto blu : HasColor "blue" s}
  {auto wht : HasColor "white" s}
  {auto blk : HasColor "black" s}

  -- the meta window has a 'next piece' entry
  drawNextElement : Element -> NCurses () s s
  drawNextElement elt = inWindow "meta" $ Indexed.do
    let center = 4 `minus` (width elt `div` 2)
    drawElement -- ugh
        @{setWindowIsActiveStill}
        @{setWindowHasColorStill}
        @{setWindowHasColorStill}
        @{setWindowHasColorStill}
        @{setWindowHasColorStill}
        @{setWindowHasColorStill}
        @{setWindowHasColorStill}
        ({ topLeft := MkPosition (gameBuffer + 5) center } elt)

---------------------------------------------------------------------------
-- Game loop

parameters
  {auto ttr : HasWindow "tetris" s}
  {auto scr : HasWindow "meta" s}
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

  step : Maybe Nat -> State () s s
  loop : Maybe Nat -> State () s s

  start : State () s s
  start = Indexed.do
    put !(lift $ liftIO initTetrisState)
    lift clear
    lift $ inWindow "meta" clear
    lift $ drawBoard initBoard
    lift $ drawNextElement !(gets queued)
    lift refresh
    loop (Just actionFrames)

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
      for_ (shapes col n) $ lift . drawElement . compose n rotate
    lift refresh
    gameOver

  step n = Indexed.do
    -- get the element
    curr <- gets element
    queued <- gets queued

    -- see whether any actions apply
    (dropping, next) <- map (fromMaybe curr <$>) $ applyAction curr

    let n = guard dropping *> n
    -- additionally if we're on a crucial frame, we step the element down
    let (b, n) = isCrucialFrame n
    next <- lift $ ifThenElse b stepElementDown pure next

    -- Before committing to this new position,
    -- we check that it is compatible with the board
    b <- isCompatible next
    (next, queued) <-
      ifThenElse b
        (pure (next, queued))
        (lift $ Indexed.do
           newqueued <- liftIO newElement
           -- paint out the old 'next element', paint in the new one
           drawNextElement ({ colour := Black } queued)
           drawNextElement newqueued
           pure (queued, newqueued))
    modify ({ element := next, queued := queued })

    -- if the new element is compatible we paint out the old element,
    -- otherwise we record it as settled in the game board and look for
    -- completed lines
    True <- ifThenElse b
              (do lift $ drawElement ({ colour := Black } curr)
                  pure True)
              (do recordElement curr
                  bd <- gets board
                  -- detect gameOver: any cell in the buffer zone is occupied
                  let False = any (any isJust) $ take gameBuffer bd
                    | True => pure False
                  modify { score $= (10+) }
                  -- detect whether we have full lines
                  let Just (l, bd) = bingos bd
                    | Nothing => pure True
                  let n = (l * S l) `div` 2 -- bonus for multilines
                  modify { board := bd, score $= ((100*n)+), lines $= (l+) }
                  -- we've removed some lines so we redraw the whole board
                  lift clear
                  lift $ drawBoard bd
                  pure True)
      | False => gameOver

    -- and then we paint in the new one
    lift $ drawElement next
    drawScore
    lift refresh

    -- wait until the next action frame
    when (isJust n) $
      lift $ liftIO $ usleep @{%search} ((pauseTime - (2500 * cast !(gets lines))) `div` cast actionFrames) @{believe_me Oh}
    loop (ifThenElse b n (n <|> Just actionFrames))

     where


       drawScore : State () s s
       drawScore = Indexed.do
         scr <- gets score
         lns <- gets lines
         qud <- gets queued
         lift $ inWindow "meta" $ Indexed.do
           setAttrs @{setWindowIsActiveStill} [Color (Named @{setWindowHasColorStill} "meta")]
           move (MkPosition 0 0)
           let str1 = show scr
           let str2 = show lns
           let pref1 = " score: "
           let pref2 = " lines: "
           let pad1 = String.replicate (17 `minus` (length str1 + length pref1)) ' '
           let pad2 = String.replicate (17 `minus` (length str2 + length pref2)) ' '
           putStrLn "\{pref1}\{pad1}\{str1} "
           putStrLn "\{pref2}\{pad2}\{str2} "
           move (MkPosition 3 1)
           putStrLn "next piece:"
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

       applyAction : Element -> State (Bool, Maybe Element) s s
       applyAction curr = do
         Just (Transform ope) <- lift getAction
           | Just Quit => lift (liftIO ((True, Nothing) <$ raiseSignal SigINT))
           | Just Drop => pure (False, Nothing)
           | Nothing => pure (True, Just curr)
         let Just next = transform ope curr
           | Nothing => pure (True, Nothing)
         b <- isCompatible next
         pure $ (True, next <$ guard b)


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
  addWindow "meta"   (MkPosition 1 (4 + 2 * gameWidth)) (MkSize 13 20) (defaultBorder "meta")

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
