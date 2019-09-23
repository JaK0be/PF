import Data.Char

type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta::Int->Polinomio->Int
conta _ [] = 0
conta x (h:t) | x == coiso h = 1 + conta x t
              | otherwise = conta x t

coiso::Monomio->Int
coiso (x,y) = y

coiso2::Monomio->Float
coiso2 (x,y) = x

grau::Polinomio->Int
grau [x] = coiso x
grau (h:y:t) | coiso h >= coiso y = grau (h:t)
             | otherwise = grau (y:t)

selgrau::Int->Polinomio->Polinomio --ver com função de ordem superior
selgrau _ [] = []
selgrau x (h:t) | x==coiso h = [h] ++ selgrau x t
                | otherwise = selgrau x t

calcula::Float->Polinomio->Float
calcula _ [] = 0
calcula x (h:t) = (coiso2 h * x)^(coiso h) + calcula x t

simp::Polinomio->Polinomio
simp [] = []
simp (h:t) = if coiso h == 0 then simp t
                             else  [h] ++ simp t

mult::Monomio->Polinomio->Polinomio
mult _ [] = []
mult (c,e) (h:t) = [(c*coiso2 h,e+coiso h)] ++ mult (c,e) t
--------------------------------------------------------------------------------------------------------------------------------------

nzp::[Int]->(Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h<0 = mais (1,0,0) (nzp t)
          | h==0 = mais (0,1,0) (nzp t)
          | otherwise = mais (0,0,1) (nzp t)

mais::(Int,Int,Int)->(Int,Int,Int)->(Int,Int,Int)
mais (x,y,z) (a,b,c) = (x+a,y+b,z+c)

-------------------------------------------------------------------------------------------------------------------------------------

digitAlpha::String->(String,String)
digitAlpha l = soma2 (alpha l) (digit l)

alpha::String->String
alpha [] = []
alpha (h:t) | isAlpha h = [h] ++ alpha t
            | otherwise = alpha t 

digit::String->String
digit [] = []
digit (h:t) | isDigit h = [h] ++ digit t
            | otherwise = digit t

soma2::String->String->(String,String)
soma2 x y = (x,y)

---------------------------------------------------------------------------------------------------------------------------------------

zipwith::(a->b->c)->[a]->[b]->[c]
zipwith _ [] [] = []
zipwith _ [] l = []
zipwith _ l [] = []
zipwith f (x:xs) (y:ys) = [f x y] ++ zipwith f xs ys

takewhile::(a->Bool)->[a]->[a]
takewhile f (h:t) = if f h then [h] ++ takewhile f t
                           else []

dropwhile::(a->Bool)->[a]->[a]
dropwhile _ [] = []
dropwhile f (h:t) = if f h then dropwhile f t
                           else [h] ++ dropwhile f t

spaN::(a->Bool)->[a]->([a],[a])
spaN _ [] = ([],[])
spaN f (h:t) | f h = soma ([h],[]) (spaN f t)
             | otherwise = soma ([],[h]) (spaN f t) 

soma::([a],[a])->([a],[a])->([a],[a])
soma (x,y) (h,t) = (x++h,y++t)