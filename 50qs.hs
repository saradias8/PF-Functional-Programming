enumFromto :: Int -> Int -> [Int]
enumFromto a b | a == b = [a]
               | otherwise = a : enumFromto (a+1) b


enumFromThento :: Int -> Int -> Int -> [Int]
enumFromThento a x b | b >= a = a : enumFromThento (a+x-1) x b
                     | otherwise = []


(+++) :: [a] -> [a] -> [a]
(+++) [] list = list
(+++) list [] = list
(+++) (x:xs) (y:ys) = x : (++) xs (y:ys)


(!!!) :: [a] -> Int -> a
(!!!) (h:t) 0 = h
(!!!) (h:t) i = (!!!) t (i-1)


reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (h:t) = reverse1 t ++ [h]


takke :: Int -> [a] -> [a]
takke n [] = []
takke 0 list = []
takke n (h:t) = h : takke (n-1) t


dropp :: Int -> [a] -> [a]
dropp n [] = []
dropp 0 list = list
dropp n (h:t) = dropp (n-1) t


zipp :: [a] -> [b] -> [(a,b)]
zipp (x:xs) (y:ys) = (x,y) : zipp xs ys
zipp _ _ = []


elemm :: Eq a => a -> [a] -> Bool
elemm x [] = False
elemm x (h:t) | x == h = True
              | otherwise = elemm x t


replicate1 :: Int -> a -> [a]
replicate1 0 x = []
replicate1 n x = x : replicate1 (n-1) x


interspers :: a -> [a] -> [a]
interspers a [x] = [x]
interspers a (h:t) = h : a : interspers a t


group :: Eq a => [a] -> [[a]]
group [] = []
group (h:t) = (replicate (conta h t 1) h ) : group (drop (conta h t 1) (h:t))
    where
      conta a [] x = x
      conta a (h:t) x | a == h = conta a t (x+1)
                      | otherwise = x


concatt :: [[a]] -> [a]
concatt [[]]= []
concatt ([]:t) = concatt t
concatt (h:t) = head h : concatt (tail h : t)


initss :: [a] -> [[a]]
initss [] = []
initss list = aux list 0
    where
      aux [] n = []
      aux (h:t) n | n <= length (h:t) = take n (h:t) : aux (h:t) (n+1)
                  | otherwise = []

tails :: [a] -> [[a]]
tails [] = []
tails list = aux list 0
     where aux [] n = []
           aux (h:t) n | n <= length (h:t) = drop n (h:t) : aux (h:t) (n+1)
                       | otherwise = []


prefix :: Eq a => [a] -> [a] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) | x == y = prefix xs ys
                     | otherwise = False


suffix :: Eq a => [a] -> [a] -> Bool
suffix [] _ = True
suffix _ [] = False
suffix l1 l2 | last l1 == last l2 = suffix (init l1) (init l2)
             | otherwise = False


subsequence :: Eq a => [a] -> [a] -> Bool
subsequence [] _ = True
subsequence _ [] = False
subsequence (x:xs) (y:ys) | x == y = subsequence xs ys
                          | otherwise = subsequence (x:xs) ys


elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices a [] = []
elemIndices a list = aux list a 0
     where aux [] a i = []
           aux (h:t) a i | a == h = i : aux t a (i+1)
                         | otherwise = aux t a (i+1)


nub :: Eq a => [a] -> [a]
nub [] = []
nub list = create [] list
    where
      create new [] = new
      create new (y:ys) | elem y new = create new ys
                        | otherwise = create (new++[y]) ys


delete :: Eq a => a -> [a] -> [a]
delete a [] = []
delete a (h:t) | elem a (h:t) == False = (h:t)
               | h == a = t
               | otherwise = h : delete a t


(\\\) :: Eq a => [a] -> [a] -> [a]
(\\\) [] list = list
(\\\) list [] = list
(\\\) (x:xs) (y:ys) | x == y = (\\\) xs ys
                    | otherwise = x : (\\\) xs (y:ys)


union :: Eq a => [a] -> [a] -> [a]
union [] list = list
union list [] = []
union (x:xs) (y:ys) | x == y = union (x:xs) ys
                    | otherwise = x : union xs (y:ys)


intersect :: Eq a => [a] -> [a] -> [a]
intersect (x:xs) (y:ys) | elem x (y:ys) = x : intersect xs (y:ys)
                        | otherwise = intersect xs (y:ys)
intersect _ _ = []


insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (h:t) | x <= h = x : h : t
               | otherwise = h : insert x t


unwordss :: [String] -> String
unwordss [x] = x
unwordss (h:t) = h ++ " " ++ unwordss t


unliness :: [String] -> String
unliness [] = []
unliness (h:t) = h ++ "\n" ++ unliness t


pMaior :: Ord a => [a] -> Int
pMaior list = aux list 0 (head list) 0
    where aux [] a m i = i
          aux (h:t) indAtual maior indMaior
              | h > maior = aux t (indAtual+1) h indAtual
              | otherwise = aux t (indAtual+1) maior indMaior


temRepetidos :: Eq a => [a] -> Bool
temRepetidos [x] = False
temRepetidos (h:i:t) | h == i = True
                     | otherwise = temRepetidos (h:t) || temRepetidos (i:t)


algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t) | h >= '0' && h <= '9' = h : algarismos t
                 | otherwise = algarismos t


posImpares :: [a] -> [a]
posImpares [] = []
posImpares list = aux list 0
       where aux [] a = []
             aux (h:t) a | mod a 2 == 0 = aux t (a+1)
                         | otherwise = h : aux t (a+1)


posPares :: [a] -> [a]
posPares [] = []
posPares list = aux list 0
     where aux [] a = []
           aux (h:t) a | mod a 2 == 0 = h : aux t (a+1)
                       | otherwise = aux t (a+1)


isSorted :: Ord a => [a] -> Bool
isSorted [x] = True
isSorted (h:i:t) | i >= h = isSorted (i:t)
                 | otherwise = False


iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)


menor :: String -> String -> Bool
menor [] _ = True
menor _ [] = True
menor (x:xs) (y:ys) | y >= x = menor xs ys
                    | otherwise = False


elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a (h:t) | a == fst h = True
                 | otherwise = elemMSet a t


lengthMSet ::  [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet (h:t) = snd h + lengthMSet t


converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet (h:t) = replicate (snd h) (fst h) ++ converteMSet t


insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((x,y):t) | x == a = (x,y+1) : t
                       | otherwise = (x,y) : insereMSet a t


removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = []
removeMSet a ((x,y):t) | x == a = if y > 1 then (x,y-1) : t else t
                       | otherwise = (x,y) : removeMSet a t


constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = (h, conta h t 1) : constroiMSet (drop (conta h t 1) (h:t))
        where conta a [] x = x
              conta a (h:t) x | a == h = conta a t (x+1)
                              | otherwise = x


partitionEithers :: [Either a b ] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers (Left a : t) = let (as,bs) = partitionEithers t
                                in (a:as,bs)
partitionEithers (Right b : t) = let (as,bs) = partitionEithers t
                                 in (as,b:bs)


catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just a :t) = a : catMaybes t
catMaybes (Nothing : t) = catMaybes t


data Movimento = Norte | Sul | Este | Oeste
                deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte:t) = posicao (x,y+1) t
posicao (x,y) (Sul:t) = posicao (x,y-1) t
posicao (x,y) (Este:t) = posicao (x+1,y) t
posicao (x,y) (Oeste:t) = posicao (x-1,y) t


caminho :: (Int,Int) ->  (Int,Int) -> [Movimento]
caminho (x,y) (a,b) | (x,y) == (a,b) = []
                    | y < b = Norte : caminho (x,y+1) (a,b)
                    | y > b = Sul   : caminho (x,y-1) (a,b)
                    | x < a = Este  : caminho (x+1,y) (a,b)
                    | x > a = Oeste : caminho (x-1,y) (a,b)


vertical :: [Movimento] -> Bool
vertical [] = True
vertical (Norte:t) = vertical t
vertical (Sul:t) = vertical t
vertical (_:t) = False


data Posicao = Pos Int Int
              deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [p] = p
maisCentral ((Pos x1 y1) : (Pos x2 y2) : t)
   | abs (x1) < abs (x2) || abs (y1) < abs (y2)
     = maisCentral ((Pos x1 y1) :t)
   | otherwise = maisCentral ((Pos x2 y2) :t)


vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos (Pos x1 y1) ((Pos x2 y2):t)
   | y1 == y2 && x1 == x2     = (Pos x1 y1) : vizinhos (Pos x1 y1) t
   | y1 == y2 && x2 == (x1+1) = (Pos x2 y2) : vizinhos (Pos x1 y1) t
   | y1 == y2 && x2 == (x1-1) = (Pos x2 y2) : vizinhos (Pos x1 y1) t
   | x1 == x2 && y2 == (y1+1) = (Pos x2 y2) : vizinhos (Pos x1 y1) t
   | x1 == x2 && y2 == (y1-1) = (Pos x2 y2) : vizinhos (Pos x1 y1) t
   | otherwise = vizinhos (Pos x1 y1) t


mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [p] = True
mesmaOrdenada ((Pos x1 y1):(Pos x2 y2):t)
   | y1 == y2 = mesmaOrdenada ((Pos x2 y2):t)
   | otherwise = False


data Semaforo = Verde | Vermelho | Amarelo
                deriving Show

intersecaoOK :: [Semaforo] -> Bool
intersecaoOK list = aux list 0 <= 1
    where aux [] n = n
          aux (Vermelho:t) n = aux t n
          aux (_:t) n = aux t (n+1)
