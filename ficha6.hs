import Data.Char

type Mat a = [[a]]

toDigits::Int->[Int]
toDigits 0 = []
toDigits x = (mod x 10) : (toDigits (div x 10))

fromDigits1::[Int]->Int
fromDigits1 (h:t) = coiso (h:t) 0

coiso::[Int]->Int->Int
coiso [] _ = 0
coiso (h:t) x = h*(10^x) + coiso t (x+1)

fromDigits2::[Int]->Int
fromDigits2 l = foldr (\x y -> x + (y * 10)) 0 l

intStr::Int->String
intStr x = map intToDigit (reverse (toDigits x))

strInt::String->Int
strInt [] = 0
strInt (h:t) = (digitToInt h)*10^(length (h:t)-1) + strInt t 

agrupa :: String -> [(Char,Int)] --NÃO SEI FAZER
agrupa [] = []
agrupa str = let (a,b) = span ((head str) == ) str in (head a, length a) : agrupa b

subList :: [a] -> [[a]] --NÃO SEI FAZER
subList [] = [[]]
subList (h:ts) = (map (\xs -> h:xs) (subList ts)) ++ (subList ts)

---------------------------------------------------------------------------------------------------------------------------------------------------------

dimOk :: Mat a -> Bool
dimOk [] = False
dimOk (h:ts) = dAux (length h) ts where
  dAux x [] = if (x > 0) then True else False
  dAux x (h:ts) = if (x == (length h)) then dAux x ts else False

dimMat::Mat a->(Int,Int)
dimMat [] = (0,0)
dimMat (h:t) | dimOk (h:t) = (length h,colunas (h:t))
             | otherwise = (0,0)

colunas::Mat a->Int
colunas [] = 0 
colunas (h:t) = 1 + colunas t 

addMat::Num a=>Mat a->Mat a->Mat a
addMat [] [] = []
addMat xs ys = zipWith (zipWith (+)) xs ys