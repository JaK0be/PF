data BTree a = Empty | Node a (BTree a) (BTree a)
  deriving Show

altura::(BTree a)->Int
altura Empty = 0
altura Node _ x y = 1 + max (altura x) (altura y)

contaNodos::(BTree a)->Int
contaNodos Empty = 0
contaNodos Node _ x y = 1 + (contaNodos x) + (contaNodos y)

folhas::(BTree a)->Int
folhas Empty = 0
folhas Node _ Empty Empty = 1
folhas Node _ x d = (folhas x) (folhas d)

prune::Int->(BTree a)->BTree a
prune _ Empty = Empty
prune 0 x = x
prune x Node a s d = Node a (prune (x-1) s) (prune (x-1) d)

mirror::(BTree a)-> BTree a
mirror Empty = Empty
mirror Node a x c = Node (-a) (mirror x) (mirror c)

zipWithBT::(a->b->c)->(BTree a)->(BTree b)->(BTree c)
zipWithBT f (Node a b c) (Node x y z) = Node (f a x) (zipWithBT f b y) (zipWithBT f c z)
zipWithBT _ _ _ = Empty

unzipBT::(BTree (a,b,c))->(BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (x,y,z) e d) = (Node x x1 w1,Node y x2 w2,Node z x3 w3)
                         where (x1,x2,x3) = unzipBT e
                               (w1,w2,w3) = unzipBT d

------------------------------------------------------------------------------------------------------------------------------------------------
minimo::(Ord a) => BTree a->a
minimo (Node x Empty _) = x
minimo (Node x e d) = minimo e

semMinimo::(Ord a)=>BTree a->BTree a
semMinimo (Node x Empty d) = d
semMinimo (Node x e d) = (Node x (semMinimo e) d)

minSmin::(Ord a) => BTree a-> (a,BTree a)
minSmin (Node x Empty d) = (x,d)
minSmin (Node x e d) = let (a,b) = minSmin e in (a,Node x b d)

-----------------------------------------------------------------------------------------------------------------------------------------------
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL 
  deriving Show
data Classificacao = Aprov Int | Rep | Faltou
  deriving Show
type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)

inscNum::Numero->Turma->Bool
inscNum _ Empty = False
inscNum x (Node (a,b,c,d) e d) = if x == a then True
                                           else if x<a then inscNum e else inscNum d

inscNome::Nome->Turma->Bool
inscNome _ Empty = False
inscNome x (Node (_,a,_,_) e d) = if x == a then True
                                            else inscNome x e || inscNome x d

trabEst::Turma->[(Numero,Nome)]
trabEst Empty = []
trabEst (Node (a,b,TE,_) e d) = [(a,b)] ++ trabEst e ++ trabEst d
trabEst (Node (a,b,_,_) e d) = trabEst e ++ trabEst d 

nota::Numero->Turma->Maybe Classificacao
nota _ Empty = Nothing
nota x (Node (a,_,_,b) e d) = if x == a then Just b 
                                        else if x<a then nota x e else nota x d

