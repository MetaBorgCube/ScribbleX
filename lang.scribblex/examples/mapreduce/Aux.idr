%access public export

Append : String -> String -> Type
Append s1 s2 = (s : String ** s = s1 ++ s2)

Eq : String -> String -> Type
Eq = (=)

F : String -> String
F = ?f

G : String -> String -> String
G = ?g

