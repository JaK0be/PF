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
 
-----------------------------------------------------------------------------------------------------------------------------------------------------------
data RTree a = R a [RTree a]

soma::(Num a)=>(RTree a)->a
soma (R a []) = a
soma (R a x) = a + (sum (map soma x))

altura::(RTree a)->Int
altura (R _ x) = 1 + (foldl max 0 (map altura x))

prune::Int->(RTree a)->(RTree a)
prune 1 (R a []) = (R a [])
prune x (R a l) = R a (map (prune (x-1)) l)

mirror::(RTree a)->(RTree a)
mirror (R a l) = R a (map mirror (reverse l))

postorder::(RTree a)->[a]
postorder (R a l) = foldr (++) [a] (map postorder l)

------------------------------------------------------------------------------------------------------------------------------------------------------------
data LTree a = Tip a | Fork (LTree a) (LTree a)

ltSum::(Num a)=> (LTree a)->a
ltSum Tip a = a
ltSum Fork x y = (ltSum x) + (ltSum y)

listaLT::(LTree a)->[a]
listaLT Tip a = [a]
listaLT (Fork x y) = (listaLT x) ++ (listaLT y)

ltHeight::(LTree a)->[a]
ltHeight Tip a = 1
ltHeight (Fork x y) = 1 + max ((ltHeight x) (ltHeight y))

------------------------------------------------------------------------------------------------------------------------------------------------------------
data FTree a b = Leaf b | No a (FTree a b) (FTree a b)
data BTree a = Empty | Node a (BTree a) (BTree a)

splitFTree::(FTree a b)->(BTree a,LTree a)
splitFTree Leaf x = (Empty,Tip x)
splitFTree (No a x y) = let (l1,l2) = splitFTree x
                            (z1,z2) = splitFTree y
                        in (Node a l1 z1,Fork l2 z2)

joinTrees::(BTree a)->(LTree b)->Maybe (FTree a b)
joinTrees (Empty) (Tip x) = Just (Leaf x)
joinTrees (Node a e d) (Fork x y) = case joinTrees e x of 
    Nothing->Nothing
    Just z -> case joinTrees d y of
      Nothing->Nothing
      Just s -> Just (No a z s)
joinTrees _ _ = Nothing