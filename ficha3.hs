type Hora = (Int,Int)
type Etapa = (Hora,Hora)
type Viagem = [Etapa]

segundos::[(a,b)]->[b]
segundos [] = []
segundos (h:t) = [snd h] ++ (segundos t)

nosPrimeiros::(Eq a)=>a->[(a,b)]->Bool
nosPrimeiros _ [] = False
nosPrimeiros x (h:t) | x==(fst h) = True
                     | otherwise = nosPrimeiros x t


minFst::(Ord a) => [(a,b)] -> a
minFst (h:t) = menor (primeiros (h:t))
                where 
                      menor (h:t) |length (h:t) == 1 = h
                                  |otherwise = if h<(head t) then menor (h:(tail t))
                                                             else menor t

sndMinFst::(Ord a)=>[(a,b)]->b
sndMinFst [(q,w)] = w
sndMinFst (h:t) = if fst h < fst (head t) then sndMinFst (h:(tail t))
                                          else sndMinFst t 

--sumTriplos:: (Num a,Num b,Num c) => [(a,b,c)] -> (a,b,c)
--sumTriplos [] = 0
--sumTriplos (h:t) | length (h:t) == 1 = (h:t)
--                 | otherwise = 
coiso::Int->Int->Int
coiso _ 0 = 0
coiso 0 _ = 0
coiso x 1 = x
coiso x y = x + (coiso x (y-1))

power::Int->Int->Int
power _ 0 = 1
power x y = x * (power x (y-1))



------------------------------------------------------------------------------------------------------------------------------
primeiros::[(a,b)] -> [a]
primeiros [] = []
primeiros (h:t) = [fst h] ++ primeiros t

