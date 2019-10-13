data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving Show

t :: BTree Int
t = Node 5 ( Node 3 ( Node 2 Empty Empty)
                    ( Node 7 Empty Empty))
           ( Node 10 ( Node 4 Empty Empty)
                     Empty)
tree = Node 7 (Node 5 (Node 3 Empty Empty)
                      (Node 6 Empty Empty))
              (Node 15 (Node 8 Empty Empty)
                      Empty)

altura :: BTree a -> Int
altura Empty = 0
altura (Node a te td) = 1 + max (altura te) (altura td)


contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node a te td) = 1 + contaNodos te + contaNodos td


folhas :: BTree a -> Int
folhas Empty = 1
folhas (Node a Empty Empty) = 1
folhas (Node a te td) = folhas te + folhas td


prune :: Int -> BTree a -> BTree a
prune 0 arv = Empty
prune n Empty = Empty
prune n (Node a te td) = Node a (prune (n-1) te) (prune (n-1) td)


path :: [Bool] -> BTree a  -> [a]
path [] arv = []
path bool Empty = []
path (True:t) (Node a te td) = a : path t td
path (False:t) (Node a te td) = a : path t te


mirrorL :: BTree a -> BTree a
mirrorL Empty = Empty
mirrorL (Node a Empty Empty) = Node a Empty Empty
mirrorL (Node a te td) = Node a te te


mirrorR :: BTree a -> BTree a
mirrorR Empty = Empty
mirrorR (Node a Empty Empty) = Node a Empty Empty
mirrorR (Node a te td) = Node a td td


zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f _ Empty = Empty
zipWithBT f Empty _ = Empty
zipWithBT f (Node a te td) (Node b tl tr)
  = Node (f a b) (zipWithBT f te tl) (zipWithBT f td tr)


{-
unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT (Node (a,b,c) te td) = (Node a (unzipBT tea) (unzipBT tda)
                               ,Node b (unzipBT teb) (unzipBT tdb)
                               ,Node c (unzipBT tec) (unzipBT tdc))
-}

minimo :: Ord a => BTree a -> a
minimo (Node a Empty Empty) = a
minimo (Node a te td) = minimo te


semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node a Empty td) = td
semMinimo (Node a te td) = Node a (semMinimo te) td


-- Não cumpre os parâmetros ??
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node a Empty Empty) = (a,Empty)
minSmin bt = (minimo bt, semMinimo bt)


-- Não cumpre os parâmetros
remove :: Ord a => a -> BTree a -> BTree a
remove x (Node a te td) | x == a = Empty
                        | x < a = Node a (remove x te) td
                        | x > a = Node a te (remove x td)


type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL  deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
    deriving Show
type Turma = BTree Aluno -- árvore binária de procura (ordenada por Numero)

a1 = (10, "ana", ORD, Aprov 16)
a2 = (11, "ze", TE, Aprov 16)
a3 = (12, "rute", TE, Rep)
a4 = (13, "joana", ORD, Faltou)
a5 = (14, "toze", TE, Aprov 20)

turma :: Turma
turma = Node a4 (Node a1 Empty
                         (Node a3 (Node a2 Empty Empty)
                                  Empty))
                (Node a5 Empty Empty)


inscNum :: Numero -> Turma -> Bool
inscNum num Empty = False
inscNum num (Node (nr,nome,reg,nota) te td) | num == nr = True
                                            | num < nr = inscNum num te
                                            | otherwise = inscNum num td


inscNome :: Nome -> Turma -> Bool
inscNome name Empty = False
inscNome name (Node (nr,nome,reg,nota) te td) | name == nome = True
                                              | otherwise = inscNome name te
                                                          || inscNome name td


trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (nr,nome,TE,nota) te td) = trabEst te ++ [(nr,nome)] ++ trabEst td
trabEst (Node (nr,nome,_,nota) te td) = trabEst te ++ trabEst td


nota :: Numero -> Turma -> Maybe Classificacao
nota num Empty = Nothing
nota num (Node (nr,nome,reg,classif) te td) | num == nr = Just classif
                                            | num > nr = nota num td
                                            | num < nr = nota num te


nrAlunos :: Turma -> Int
nrAlunos Empty = 0
nrAlunos (Node a te td) = 1 + nrAlunos te + nrAlunos td

nrFaltas :: Turma -> Int
nrFaltas Empty = 0
nrFaltas (Node (nr,nome,reg,Faltou) te td) = 1 + nrFaltas te + nrFaltas td
nrFaltas (Node (nr,nome,reg,_) te td) = nrFaltas te + nrFaltas td

percFaltas :: Turma -> Float
percFaltas t = (fromIntegral (nrFaltas t) / fromIntegral (nrAlunos t)) * 100


nrAprov :: Turma -> Int
nrAprov Empty = 0
nrAprov (Node (nr,nome,reg,Aprov n) te td) = 1 + nrAprov te + nrAprov td
nrAprov (Node (nr,nome,reg,_) te td) = nrAprov te + nrAprov td

sumNotas :: Turma -> Int
sumNotas Empty = 0
sumNotas (Node (nr,nome,reg,Aprov n) te td) = n + sumNotas te + sumNotas td
sumNotas (Node (nr,nome,reg,_) te td) = sumNotas te + sumNotas td

mediaAprov :: Turma -> Float
mediaAprov t = fromIntegral (sumNotas t) / fromIntegral (nrAprov t)


avaliados :: Turma -> Int
avaliados Empty = 0
avaliados (Node (nr,nome,reg,Faltou) te td) = avaliados te + avaliados td
avaliados (Node (nr,nome,reg,_) te td) = 1 + avaliados te + avaliados td

-- Não cumpre os parâmetros
aprovAv :: Turma -> Float
aprovAv bt = fromIntegral (nrAprov bt) / fromIntegral (avaliados bt)



-- juntar funcs e produzir par 
aprov :: Turma -> Float
aprov turma = aux 0 0 turma
   where
     aux a b Empty = a/b
     aux a b (Node (na,no,r,Aprov x) e d) = aux (a+1) (b+1) e + aux (a+1) (b+1) d
     aux a b (Node _ e d) = aux a (b+1) e + aux a (b+1) d
