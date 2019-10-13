-- EXERCICIO 1

type MSet a = [(a,Int)]

-- (a)

cardMSet :: MSet a -> Int
cardMSet [m] = snd m
cardMSet l = sum (map snd l)

-- (b)

-- Retorna a lista do nr de vezes de cada elemento
counts :: MSet a -> [Int]
counts [(a,n)] = [n]
counts ((a,n):t) = n : counts t


moda :: MSet a -> [a]
moda [] = []
moda m = aux m (maximum(map snd m)) (counts m)
  where
    aux [] _ _ = []
    aux m c counts
      | length m > 1 && head counts == c
        && head counts == head (tail counts)
        = fst(head m) : fst(head(tail m))
          : aux (drop 2 m) c (drop 2 counts)
      | length m > 1 && head counts == c
        = fst(head m)
          : aux (drop 1 m) c (drop 1 counts)
      | head counts == c
        = [fst(head m)]
      | otherwise
        = aux (drop 1 m) c (drop 1 counts)

-- (c)

converteMSet :: MSet a -> [a]
converteMSet [(a,n)] = replicate n a
converteMSet ((a,n):t) = replicate n a ++ converteMSet t

-- (d)

inMSet :: a -> MSet a -> Boo
inMSet e [] = False
inMSet e ((a,b):t) | e == a = True
                   | otherwise = inMSet e t

addMSet :: MSet a -> a -> Int -> MSet a
addMSet ((a,b):t) e c | a == e = ((a,b+c):t)
                      | otherwise = (a,b) : addMSet t e c

insere :: MSet a -> a -> Int -> MSet a
insere mset e c | 

addNCopies :: Eq a => MSet a -> a -> Int -> MSet a
addNCopies mset e c | inMSet e mset = addMSet mset e c
                    | otherwise = insere mset e c


-- EXERCICIO 2

data SReais = AA Double Double | FF Double Double
            | AF Double Double | FA Double Double
            | Uniao SReais SReais

instance Show SReais where
  show (AA a b) = "]" ++ show a ++ "," ++ show b ++ "["
  show (FF a b) = "[" ++ show a ++ "," ++ show b ++ "]"
  show (AF a b) = "]" ++ show a ++ "," ++ show b ++ "]"
  show (FA a b) = "[" ++ show a ++ "," ++ show b ++ "["
  show (Uniao s1 s2) = "(" ++ show s1 ++ " U " ++ show s2 ++ ")"


pertence  :: Double -> SReais -> Bool
pertence d (AA a b) = d > a && d < b
pertence d (FF a b) = d >= a && d <= b
pertence d (AF a b) = d > a && d <= b
pertence d (FA a b) = d >= a && d < b
pertence d (Uniao s1 s2) = pertence d s1 || pertence d s2


tira :: Double -> SReais -> SReais
tira d (AA a b) | not (d > a && d < b) = AA a b
                | otherwise = Uniao (AA a d) (AA d b)

tira d (FF a b) | not (d >= a && d <= b) = FF a b
                | d == a = AF a b
                | d == b = FA a b
                | otherwise = Uniao (FA a d) (AF d b)

tira d (AF a b) | not (d > a && d <= b) = AF a b
                | d == b = AA a b
                | otherwise = Uniao (AA a d) (AF d b)

tira d (FA a b) | not (d >= a && d < b) = FA a b
                | d == a = AA a b
                | otherwise = Uniao (FA a d) (AA d b)

tira d (Uniao s1 s2) = Uniao (tira d s1) (tira d s2)



data RTree a = R a [RTree a]

rt :: RTree Int
rt = R 1 [ R 2 [ R 7 []
               , R 2 []
               ]
         , R 3 [ R 1 [ R 4 []
                     , R 2 []]
               , R 2 []
               ]
         ]

{-percorre :: [Int] -> RTree a -> Maybe [a]
percorre (i:is) (R a l) = Just (aux (i:is) (R a l))
  where
    aux [] r = []
    aux (i:is) (R a l) = a : aux is (l!!i)

caminho :: a -> [a] -> [Int]
caminho

procura :: Eq a => a -> RTree a -> Maybe [Int]
procura el (R a l) | el == a = Just [0]
                   | elem el l = Just (caminho el l)
                   | otherwise = Nothing
-}
