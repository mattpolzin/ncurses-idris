||| Snake
module Main

import System
import System.Signal
import System.Random
import Control.NCurses
import Control.NCurses.Pretty
import Data.Nat
import Data.Maybe
import Data.String
import Control.Indexed
import Control.Monad.Indexed.State

Eq Position where
  MkPosition x y == MkPosition v w
    = x == v && y == w

dist : Nat -> Nat -> Nat
dist 0 n = n
dist m 0 = m
dist (S m) (S n) = dist m n

distance : Position -> Position -> Nat
distance p q = dist p.col q.col + dist p.row q.row

namespace ISnake

  data ISnake : Nat -> (hd : Position) -> Type

  public export
  NoCollision : Position -> ISnake n hd -> Type

  public export
  data ISnake : Nat -> (hd : Position) -> Type where
    Tail : (hd : Position) -> ISnake 0 hd
    (:<) : (sn : ISnake n pos) -> (hd : Position) ->
           {auto 0 _ : So (distance pos hd == 1)} ->
           {auto 0 _ : NoCollision hd sn} ->
           ISnake (S n) hd

  NoCollision pos (Tail hd) = So (pos /= hd)
  NoCollision pos (sn :< hd) = (So (pos /= hd), NoCollision pos sn)

  dropTailAux :
    (sn : ISnake (S n) hd) ->
    (Position, (sn' : ISnake n hd ** (x : _) -> NoCollision x sn -> NoCollision x sn'))
  dropTailAux ((Tail pos) :< hd) = (pos, (Tail hd ** \ x, (p, _) => p))
  dropTailAux ((sn :< pos) :< hd)
    = let (p, (sn' ** prf)) = dropTailAux (sn :< pos) in
      let 0 noColl2 = prf hd %search in
      (p, ((sn' :< hd) ** \ x, (p, q) => (p, prf x q)))

  export
  dropTail : ISnake (S n) hd -> (Position, ISnake n hd)
  dropTail sn = let (p, (sn ** _)) = dropTailAux sn in (p, sn)

  export
  noCollision : (x : Position) -> (sn : ISnake n hd) -> Maybe (NoCollision x sn)
  noCollision x (Tail hd) = case choose (x /= hd) of
    Left so => Just so
    Right _ => Nothing
  noCollision x (sn :< hd) = do
    let Left so1 = choose (x /= hd)
      | Right _ => Nothing
    so2 <- noCollision x sn
    pure (so1, so2)

record Snake where
  constructor MkSnake
  {0 snakeHead : Position}
  {snakeLength : Nat}
  snakeBody : ISnake snakeLength snakeHead

head : (sn : Snake) -> (head : Position ** snakeHead sn === head)
head (MkSnake (Tail snakeHead)) = (snakeHead ** Refl)
head (MkSnake (_ :< snakeHead)) = (snakeHead ** Refl)

initSnake : Snake
initSnake
  = MkSnake
  $  Tail (MkPosition 4 4)
  :< MkPosition 4 3
  :< MkPosition 3 3
  :< MkPosition 3 4
  :< MkPosition 3 5
  :< MkPosition 4 5
  :< MkPosition 5 5

data Direction = North | South | East | West

Eq Direction where
  North == North = True
  South == South = True
  East == East = True
  West == West = True
  _ == _ = False

flip : Direction -> Direction
flip North = South
flip South = North
flip East = West
flip West = East

getDirection : IsActive s => YesKeypad s => NoDelay s =>
               NCurses (Maybe Direction) s s
getDirection = getKeyOrChar >>= \ input => pure $ case input of
  Just (Left 'w') => Just North
  Just (Left 'a') => Just West
  Just (Left 's') => Just South
  Just (Left 'd') => Just East
  Just (Right Up) => Just North
  Just (Right Left) => Just West
  Just (Right Down) => Just South
  Just (Right Right) => Just East
  _ => Nothing

direction : Direction -> Position -> Position
direction North pos = { row $= pred } pos
direction South pos = { row $= S } pos
direction East pos = { col $= S } pos
direction West pos = { col $= pred } pos

record SnakeState where
  constructor MkSnakeState
  snake : Snake
  direction : Direction
  apple : Position
  score : Nat

initSnakeState : SnakeState
initSnakeState = MkSnakeState
  { snake = initSnake
  , direction = East
  , apple = MkPosition 10 10
  , score = 0
  }

State : Type -> CursesState -> CursesState -> Type
State = IndexedStateT SnakeState CursesState CursesState NCurses

pixel :
  IsActive s =>
  (col : String) -> HasColor col s =>
  Position -> NCurses () s s
pixel color pos = do
  setAttr (Color (Named color))
  let pos = { col $= (2*) } pos
  move pos
  MkSize rows cols <- getWindowSize True
  ifThenElse ((cols `minus` col pos) < 2)
    (putCh ' ')
    (putStr "  ")


newApple : IsActive s => State () s s
newApple = do
  MkSize rows cols <- lift $ getWindowSize True
  row <- lift $ liftIO $ randomRIO {a = Int32} (1, (cast rows - 1) `div` 2)
  col <- lift $ liftIO $ randomRIO {a = Int32} (0, (cast cols - 1) `div` 2)
  modify { apple := MkPosition { row = cast row, col = cast col } }

parameters
  {auto act : IsActive s}
  {auto kpd : YesKeypad s}
  {auto dly : NoDelay s}
  {auto gov : HasColor "meta" s}
  {auto org : HasColor "orange" s}
  {auto red : HasColor "red" s}
  {auto blk : HasColor "black" s}
  {auto grn : HasColor "green" s}
  {auto blu : HasColor "blue" s}

  step : State () s s
  loop : State () s s

  start : State () s s
  start = do
    put initSnakeState
    newApple
    lift clear
    loop

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

  loop =
    case !(lift $ liftIO handleNextCollectedSignal) of
         (Just SigINT) => pure ()
         _ => step

  step = do
    st <- get
    drawSnakeState st
    Just pos <- stepSnake
      | Nothing => loop
    -- draw collision at pos
    drawCollision pos
    -- redraw snake on top
    drawSnakeState st
    gameOver

    where

      drawSnakeState : SnakeState -> State () s s
      drawSnakeState (MkSnakeState sn dir apple score) = lift $ do
        drawScore score
        go True (snakeBody sn)
        pixel "red" apple
        refresh

        where

         drawScore : Nat -> NCurses () s s
         drawScore n = do
           let str = show n
           let len = length str
           MkSize rows cols <- getWindowSize True
           setAttrs [Bold, Color (Named "meta")]
           move (MkPosition { row = 0, col = cols `minus` len })
           putStr str

         drawHead : Bool -> Position -> NCurses () s s
         drawHead b pos =
           ifThenElse b
             (pixel "green" pos)
             (pixel "blue" pos)

         go : Bool -> ISnake n hd -> NCurses () s s
         go b (Tail hd) = drawHead b hd
         go b (sn :< hd) = do
           go (not b) sn
           drawHead b hd

      stepPosition : Direction -> Position -> NCurses Position s s
      stepPosition dir pos = do
        let next = direction dir pos
        MkSize rows cols <- getWindowSize True
        let cols = cols `div` 2
        pure $ ifThenElse (next.col == cols || next.row == rows || next.row == 0)
                 pos
                 next

      stepDirection : Maybe Direction -> State () s s
      stepDirection (Just ndir) = do
        odir <- gets direction
        ifThenElse (ndir == flip odir)
          (pure ()) -- this would be instant death
          (modify { direction := ndir })
      stepDirection Nothing = pure ()

      stepSnake : State (Maybe Position) s s
      stepSnake = do
        stepDirection !(lift getDirection)
        MkSnakeState sn@(MkSnake body) dir apple score <- get
        let (hd ** Refl) = head sn
        next <- lift $ stepPosition dir hd
        let Just noColl = noCollision next body
          | Nothing => pure (Just next)
        let Left so = choose (distance hd next == 1)
          | Right soNot => pure (Just next)
        if (next == apple)
          then do newApple
                  modify { snake := MkSnake (body :< next), score $= (100 +) }
          else do let (pos, body) = dropTail (body :< next)
                  modify { snake := MkSnake body, score $= S }
                  lift $ pixel "black" pos
        lift $ liftIO $ usleep 70000
        pure Nothing

      drawCollision : Position -> State () s s
      drawCollision pos = do
        let offsets : List Int := [-2,-1,0,1,2]
        let zone : List (Int, Int) := [ (x, y) | x <- offsets, y <- offsets ]
        lift $ for_ zone $ \ (offX, offY) => do
          let adjust : Int -> Nat -> Nat := \ i, n => cast (i + cast n)
          let cell = { row $= adjust offY, col $= adjust offX } pos
          MkSize rows cols <- getWindowSize True
          let cols = cols `div` 2
          unless (cell.row <= 0 || cell.row >= rows || cell.col >= cols) $
            ifThenElse ( (abs offX == abs offY)
                      || (offX == 0 && abs offY /= 1)
                      || (offY == 0 && abs offX /= 1))
              (pixel "orange" cell)
              (pixel "red" cell)

run : NCurses () Inactive Inactive
run = Indexed.do
  init
  MkSize rows cols <- getWindowSize True
  setCursor CInvisible
  addWindow "snake" (MkPosition 0 0) (MkSize rows cols) Nothing
  addColor "orange" Black Yellow
  addColor "red"    Red Red
  addColor "green"  Green Green
  addColor "black"  Black Black
  addColor "blue"   Blue Blue
  addColor "meta"   White Black
  setWindow "snake"
  setNoDelay True
  setKeypad True
  refresh
  evalStateT initSnakeState start
  deinit

main : IO ()
main = do
  ignore $ collectSignal SigINT
  withNCurses run
