import Control.ST
import Control.ST.ImplicitCall
import Data.String

import Lt

data State : Type where
  Ready : State
  S3 : (x : Int) -> State
  S4 : (x : Int) -> (y : Int) -> State

interface DepTestOK_B (m : Type -> Type) where
  StateT : State -> Type

  start : STrans m Var [] (\ l => [l ::: StateT Ready])

  init : (l : Var) ->
    STrans m Int
           [l ::: StateT Ready]
           (\ x => [l ::: StateT (S3 x)])

  bid : (l : Var) -> (y : Int) -> (Lt x y) ->
    STrans m ()
           [l ::: StateT (S3 x)]
           (const [l ::: StateT (S4 x y)])

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
