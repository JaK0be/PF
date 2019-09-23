import Data.Char


dobros::[Float]->[Float]
dobros [] = []
dobros (h:t) = [2*h] ++ map (2*) t

numOcorre::Char->String->Int
numOcorre x [] = 0
numOcorre x (h:t) | x==h = 1 + (numOcorre x t)
                  | otherwise = (numOcorre x t)

soPos::[Int]->[Int]
soPos [] = []
soPos (h:t) | h<=0 = soPos t
            | otherwise = (h:soPos t)

positivos::[Int]->Bool
positivos [] = True
positivos (h:t) | h<=0 = False
                | otherwise = (positivos t)
somaNeg::[Int]->Int
somaNeg [] = 0
somaNeg (h:t) | h<0 = h + (somaNeg t)
              | otherwise = (somaNeg t)

tresUlt::[a]->[a]
tresUlt [] = []
tresUlt l | length l <= 3 = l
          | otherwise = tresUlt (tail l)

primeiros::[(a,b)] -> [a]
primeiros [] = []
primeiros (h:t) = [fst h] ++ primeiros t

islower::Char->Bool--definição estúpida
islower x = if elem x "abcdefghijklmnopqrstuvwxyz" then True
                                                   else False

isdigit::Char-> Bool
isdigit x = if elem x "0123456789" then True
                                   else False
primMai::String->Bool
primMai [] = False
primMai (h:t) | isLower h = False
              | otherwise = True

segMin::String->Bool
segMin [] = False
segMin (x:y:t) | isLower y = True
               | otherwise = False

soDigitos::String->String
soDigitos [] = []
soDigitos (h:t) | isAlpha h = soDigitos t
                | otherwise = [h] ++ (soDigitos t)

minusculas::String->Int
minusculas [] = 0
minusculas (h:t) | isLower h = 1 + (minusculas t)
                 | otherwise = (minusculas t)

nums::String->[Int]
nums [] = []
nums (h:t) | isAlpha h = (nums t)
           | otherwise = [digitToInt h] ++ (nums t)