type Ponto = (Float,Float)
type Rectangulo = (Ponto,Float,Float)
type Triangulo = (Ponto,Ponto,Ponto)
type Poligonal = [Ponto]
type TabTemp = [(Data,Temp,Temp)]
type Data = (Int,Int,Int)
type Temp = Float
type MSet a = [(a,Int)]

distancia::Ponto->Ponto->Float
distancia (a,b) (c,d) = sqrt (((c-40)^2)+((b-d)^2))

comprimento::[Ponto]->Float
comprimento [] = 0
comprimento (h:t) = distancia h (last (h:t))

convTriPol::Triangulo->Poligonal
convTriPol (x,y,z) = [x]++[y]++[z]++[x]

convRetPol::Rectangulo->Poligonal
convRetPol ((x,y),z,d) = [(x,y)]++[(x,y+z)]++[(x-d,y+z)]++[(x-d,y)]++[(x,y)]

fechada::Poligonal->Bool
fechada [] = False
fechada (h:t) = if h == last t then True
                               else False

--triangula
--area seria igual à soma das áreas dos triangulos fornecidos pela triangula

mover::Poligonal->Ponto->Poligonal
mover (h:t) (x,y) = somaPP (h:t) (diferP (x,y) h)

somaP::Ponto->Ponto->Ponto
somaP (x,y) (z,d) = (x+z,y+d)

diferP::Ponto->Ponto->Ponto
diferP (x,y) (z,d) = (x-z,y-d)

somaPP::Poligonal->Ponto->Poligonal
somaPP [] _ = []
somaPP (h:t) (x,y) = [somaP h (x,y)] ++ somaPP t (x,y)

--------------------------------------------------------------------------------------------------------------------------------
medias::TabTemp->[(Data,Temp)]
medias [] = []
medias (h:t) = [coisoTT h] ++ medias t

coisoTT::(Data,Temp,Temp)->(Data,Temp)
coisoTT (x,y,z) = (x,((y+z)/2))

decrescente::TabTemp->Bool
decrescente [] = True
decrescente (h:t) | compara (coisoD h) (coisoD (head t)) = decrescente t
                  | otherwise = False

coisoD::(Data,Temp,Temp)->Data
coisoD (x,y,z) = x

coisoDD::TabTemp->[Data]
coisoDD [] = []
coisoDD (h:t) = [coisoD h] ++ coisoDD t

compara::Data->Data->Bool
compara (x,y,z) (a,b,c) = if (x>=a) && (y>=b) && (z>=c) then True
                                                        else False

conta::[Data]->TabTemp->Int
conta [] _ = 0
conta (x:xs) d | elem x (coisoDD d) = 1 + conta xs d
               | otherwise = conta xs d

-----------------------------------------------------------------------------------------------------------------------
union::Eq a=> MSet a -> MSet a -> MSet a
union [] [] = []
union (x:xs) [] = (x:xs)
union [] (x:xs) = (x:xs)
union (x:xs) (y:ys) | fst x == fst y = [(fst x,(snd x + snd y))] ++ union (x:xs) ys
                    | otherwise = union (xs) (y:ys)
