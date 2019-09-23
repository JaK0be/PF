data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String
   deriving Show

type Nome = String
type Agenda =[(Nome,[Contacto])]

acrescEmail::Nome->String->Agenda->Agenda
acrescEmail x y [] = [(x,[Email y])]
acrescEmail x y ((nome,lista):t) = if x == nome then ((nome,Email y : lista) : t)
                                                else ((nome,lista) : (acrescEmail x y t))

verEmails::Nome->Agenda->Maybe [String]
verEmails _ [] = Nothing
verEmails x ((nome,lista):t) = if x==nome then Just (procura' lista) else verEmails x t 
                                                     where procura' []=[]
                                                           procura' ((Email x):t) = x : procura' t
                                                           procura' (_:t) = procura' t

consTelefs::[Contacto]->[Integer]
consTelefs [] = []
consTelefs ((Casa x):t) = x : consTelefs t
consTelefs ((Trab x):t) = x : consTelefs t
consTelefs ((Tlm x):t) = x : consTelefs t
consTelefs (_:t) = consTelefs t

casa::Nome->Agenda->Maybe Integer
casa _ [] = Nothing
casa x ((xx,y):t) = if x == xx then pumba y else casa x t
                                      where pumba [] = Nothing
                                            pumba ((Casa x):t) = Just x
                                            pumba (_:t) = pumba t

-------------------------------------------------------------------------------------------------------------------------------------------------
type Dia = Int
type Mes = Int
type Ano = Int

data Data = D Dia Mes Ano
  deriving Show

type TabDN = [(Nome,Data)]

procura::Nome->TabDN->Maybe Data
procura x [] = Nothing
procura x ((xx,y):t) = if x == xx then Just y else procura x t 

idade::Data->Nome->TabDN->Maybe Int
idade _ _ [] = Nothing
idade y x ((xx,yy):t) = if x == xx then Just (idadeAux y yy) else idade y x t

idadeAux::Data->Data->Int
idadeAux (D x y z) (D a b c) = (z-c) + (div (y-b) 12) + (div (z-c) 365) 

anterior::Data->Data->Bool
anterior (D x y z) (D a b c) | z < c = True
                             | z <=c && y<=b && z<c = True
                             | otherwise = False

---------------------------------------------------------------------------------------------------------------------------------------------------
data Movimento = Credito Float | Debito Float
  deriving Show
data Extracto = Ext Float [(Data, String, Movimento)]
  deriving Show


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