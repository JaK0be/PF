data Frac = F Integer Integer

normaliza :: Frac -> Frac
normaliza (F n 0) = error "denominador nulo"
normaliza (F 0 d) = F 0 1
normaliza (F n d) = F ((signum d) * (n `div` m)) ((abs d) `div` m) where m = mdc (abs n) (abs d)


mdc :: Integer -> Integer -> Integer
mdc a b | a > b = mdc (a - b) b
        | a < b = mdc a (b - a)
        | a == b = a

instance Eq Frac where
  (==) x y = (a1==a2) && (b1==b2)
              where (F a1 b1) = normaliza x 
                    (F a2 b2) = normaliza y

instance Ord Frac where
  compare (F x y) (F a b) = compare (x*b) (y*a)

instance Show Frac where
  show (F a b) = "("++show a++"/"++show b++")"

instance Num Frac where
  (+) (F a b) (F x y) = normaliza (F ((a*y) + (x*b)) (b*y)) 
  (*) (F a b) (F x y) = normaliza (F (a*x) (b*y))
  (-) (F a b) (F x y) = normaliza (F ((a*y) - (x*b)) (b*y))
  negate (F a b) = (F (-x) y)
                    where F x y = normaliza (F a b)
  abs (F a b) = normaliza (f (abs a) (abs b))
  signum (F n d) = signum (F n d) = let (F a b) = normaliza (F n d) in if (a == 0) then 0 else if (a > 0) then 1 else (-1)
  fromInteger x = (F x 1)

mDobro::Frac->[Frac]->[Frac]
mDobro _ [] = []
mDobro x (h:t) = if (2*x) <= h then [h] ++ (mDobro x t) else (mDobro x t)

----------------------------------------------------------------------------------------------------------------------------------------------------------------
data ExpInt = Const Int | Simetrico ExpInt | Mais ExpInt ExpInt | Menos ExpInt ExpInt | Mult ExpInt ExpInt

calcula::ExpInt->Int
calcula (Const x) = x
calcula (Simetrico x) = (-(calcula x))
calcula (Mais x y) = (calcula x) + (calcula y)
calcula (Menos x y) = (calcula x) - (calcula y)
calcula (Mult x y) = (calcula x) * (calcula y)

infixa::ExpInt->String
infixa (Const x) = show x
infixa (Simetrico x) = "(-" ++ (infixa x) ++ ")"
infixa (Mais x y) = "(" ++ (infixa x) ++ "+" ++ (infixa y) ++ ")"
infixa (Menos x y) = "(" ++ (infixa x) ++ "-" ++ (infixa y) ++ ")"
infixa (Mult x y) = "(" ++ (infixa x) ++ "*" ++ (infixa y) ++ ")"

posfixa::ExpInt->String
posfixa (Const x) = show x
posfixa (Simetrico x) = posfixa x ++ " " ++ "-"
posfixa (Mais x y) = posfixa x ++ " " ++ posfixa y ++ " " ++ "+"
posfixa (Menos x y) = posfixa x ++ " " ++ posfixa y ++ " " ++ "-"
posfixa (Mult x y) = posfixa x ++ " " ++ posfixa y ++ " " ++ "*"

instance Show ExpInt where
  show x = infixa x 

instance Eq ExpInt where
  (==) x y = (calcula x) == (calcula y)

instance Num ExpInt where
  (+) x y = (Mais x y)
  (-) x y = (Menos x y) 
  (*) x y = (Mult x y)
  negate x = (Simetrico x)
  abs x = if (calcula x) < 0 then (Simetrico x) else x
  signum x | (calcula x) < 0 = (-1)
           | (calcula x) == 0 = 0
           | (calcula x) > 0 = 1
  fromInteger x = (Const (fromInteger x))

----------------------------------------------------------------------------------------------------------------------------------------------------------
data Movimento = Credito Float | Debito Float
data Data = D Int Int Int -- Dia Mes Ano
data Extracto = Ext Float [(Data, String, Movimento)]

estValor::Extracto->Float->[Movimento]
estValor (Ext _ []) _ = []
estValor (Ext z ((a,b,Debito c):t)) x = if c > z then [Debito c] ++ estValor (Ext z t) x else estValor (Ext z t) x
estValor (Ext z ((a,b,Credito c):t)) x = if c > z then [Credito c] ++ estValor (Ext z t) x else estValor (Ext z t) x

filtro::Extracto->[String]->[(Data,Movimento)]
filtro _ [] = []
filtro (Ext z ((a,b,c):t)) x = if elem b x then [(a,c)] ++ filtro (Ext z t) x else filtro (Ext z t) x

creDeb::Extracto->(Float,Float)
creDeb (Ext _ []) = (0,0)
creDeb (Ext z ((a,b,Debito c):t)) = soma (0,1) (creDeb (Ext z t))
creDeb (Ext z ((a,b,Credito c):t)) = soma (1,0) (creDeb (Ext z t))

soma::(Float,Float)->(Float,Float)->(Float,Float)
soma (x,y) (a,b) = (x+a,y+b)

instance Eq Data where
  (==) (D x y z) (D a b c) = (x==a) && (y==b) && (z==c)

instance Ord Data where
    compare (D x y z) (D a b c) = if (x==a) && (y==b) && (z==c) then EQ else if ((c>z) || ((c==z) && (b>y)) || ((c==z) && (b==y) && (a>x))) then GT else LT
 