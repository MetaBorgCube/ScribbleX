import Control.ST
import Control.ST.ImplicitCall
import Data.String

data State : Type where
  Ready : State
  S1 : (x : Int) -> State
  S2 : (x : Int) -> State
  S3 : (x : Int) -> State
  S4 : (x : Int) -> State
  S5 : (x : Int) -> (y : Int) -> State
  S6 : (x : Int) -> (y : Int) -> (z : Int) -> (q : String) -> State
  S7 : (x : Int) -> (y : Int) -> (z : Int) -> (q : String) -> (a : Int) -> State
  S8 : (x : Int) -> (y : Int) -> (z : Int) -> (q : String) -> (a : Int) -> State
  S9 : (x : Int) -> (y : Int) -> (z : Int) -> (q : String) -> (a : Int) -> State
  S10 : (x : Int) -> (y : Int) -> (z : Int) -> (q : String) -> (a : Int) -> State
  S11 : (x : Int) -> (y : Int) -> (z : Int) -> (q : String) -> (a : Int) -> (b : String) -> State
  S12 : (x : Int) -> (y : Int) -> (z : Int) -> (q : String) -> (a : Int) -> (b : String) -> (c : String) -> (d : Int) -> State

interface Test (m : Type -> Type) where
  StateT : State -> Type

  start : STrans m Var [] (\ l => [l ::: StateT Ready])

  a1 : (l : Var) ->
    STrans m Int
           [l ::: StateT Ready]
           (\ x => [l ::: StateT (S1 x)])

  a2 : (l : Var) ->
    STrans m Int
           [l ::: StateT (S1 x)]
           (const [l ::: StateT (S2 x)])

  a3 : (l : Var) ->
    STrans m (x0 : Int ** x0 = x)
           [l ::: StateT (S2 x)]
           (const [l ::: StateT (S3 x)])

  a4 : (l : Var) ->
    STrans m ()
           [l ::: StateT (S3 x)]
           (const [l ::: StateT (S4 x)])

  a5 : (l : Var) ->
    STrans m (Int , (x0 : Int ** x0 = x))
           [l ::: StateT (S4 x)]
           (\ r =>
             [l ::: StateT (case r of
               (y , _) =>
                 (S5 x y))])

  a6 : (l : Var) ->
    STrans m (Int , String)
           [l ::: StateT (S5 x y)]
           (\ r =>
             [l ::: StateT (case r of
               (z , q) =>
                 (S6 x y z q))])

  b1 : (l : Var) -> (a : Int) ->
    STrans m ()
           [l ::: StateT (S6 x y z q)]
           (const [l ::: StateT (S7 x y z q a)])

  b2 : (l : Var) -> Int ->
    STrans m ()
           [l ::: StateT (S7 x y z q a)]
           (const [l ::: StateT (S8 x y z q a)])

  b3 : (l : Var) -> (a : Int) ->
    STrans m ()
           [l ::: StateT (S8 x y z q a)]
           (const [l ::: StateT (S9 x y z q a)])

  b4 : (l : Var) ->
    STrans m ()
           [l ::: StateT (S9 x y z q a)]
           (const [l ::: StateT (S10 x y z q a)])

  b5 : (l : Var) -> (b : String) -> (a : Int) ->
    STrans m ()
           [l ::: StateT (S10 x y z q a)]
           (const [l ::: StateT (S11 x y z q a b)])

  b6 : (l : Var) -> (c : String) -> (d : Int) ->
    STrans m ()
           [l ::: StateT (S11 x y z q a b)]
           (const [l ::: StateT (S12 x y z q a b c d)])

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
