%access public export

Proj1 : Pair x y -> x
Proj1 = fst

Proj2 : Pair x y -> y
Proj2 = snd

Lte : Int -> Int -> Type
Lte x y = (x <= y) = True
