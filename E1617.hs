import Data.List

unliness :: [String] -> String
unliness [s] = s
unliness (h:t) = h ++ "\n" ++ unliness t


{- remove [1,2,2,3,2,1,4,1] [2,1,2]
 = aux [] [1,2,2,3,2,1,4,1] [2,1,2]
 = aux [1] [2,2,3,2,1,4,1] [2,1,2]
 = aux [] [1,2,3,2,1,4,1] [1,2]
 = aux [] [2,3,2,1,4,1] [2]
 = aux [] [3,2,1,4,1] []
 = [3,2,1,4,1]
-}
remove :: Eq a => [a] -> [a] -> [a]
remove (h:t) (x:xs) = aux [] (h:t) (x:xs)
  where
    aux _ l [] = l
    aux [] (h:t) (x:xs) | h == x = aux [] t xs
                        | otherwise = aux [h] t (x:xs)
    aux (i:is) (h:t) (x:xs) | h == x = aux [] ((i:is) ++ t) xs
                            | otherwise = aux (h:i:is) t (x:xs)


data Seq a = Nil | Inicio a (Seq a) | Fim (Seq a) a
            deriving Show
s :: Seq Int
s = Fim (Inicio 1 (Fim (Fim (Nil) 2) 3)) 1

primeiro :: Seq a -> a
primeiro (Inicio a s) = a
primeiro (Fim Nil a) = a
primeiro (Fim s a) = primeiro s

semUltimo :: Seq a -> Seq a
semUltimo (Inicio a Nil) = Nil
semUltimo (Inicio a s) = Inicio a (semUltimo s)
semUltimo (Fim s a) = s


data BTree a = Empty | Node a (BTree a) (BTree a)
     deriving Show

tree :: BTree Int
tree = Node 7 (Node 5 (Node 3 Empty Empty)
                      (Node 6 Empty Empty))
              (Node 15 (Node 8 Empty Empty)
                      Empty)

prune :: Int -> BTree a -> BTree a
prune x Empty = Empty
prune 0 (Node a e d) = Empty
prune x (Node a e d) = Node a (prune (x-1) e) (prune (x-1) d)


semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node a Empty d) = Empty
semMinimo (Node a e d) = Node a (semMinimo e) d


type Tabuleiro = [String]

exemplo :: Tabuleiro
exemplo = ["..R.",
           "R...",
           "...R",
           ".R.."]

pos :: Tabuleiro -> (Int,Int) -> [(Int,Int)]
pos [] _ = []
pos (h:t) (a,b) = aux h (a,b) : pos t (0,b+1)
    where
      aux (s:ss) (a,b) | s == 'R' = (a,b)
                       | otherwise = aux ss (a+1,b)

posicoes :: Tabuleiro -> [(Int,Int)]
posicoes [] = []
posicoes t = pos t (0,0)


somaP :: [(Int,Int)] -> [Int]
somaP [] = []
somaP ((a,b):p) = a+b : somaP p

checkEq :: [Int] -> Bool
checkEq [i] = True
checkEq (i:j:is) | i == j = False
                 | otherwise = checkEq (j:is)

line :: [(Int,Int)] -> [Int]
line [] = []
line ((a,b):p) = b : line p

column :: [(Int,Int)] -> [Int]
column [] = []
column ((a,b):p) = a : column p

valido :: Tabuleiro -> Bool
valido tab = (checkEq $ sort $ somaP $ posicoes tab) -- diagonal
             && (checkEq $ line $ posicoes tab) -- linha
             && (checkEq $ column $ posicoes tab) -- coluna

bemFormado :: Int -> Tabuleiro -> Bool
bemFormado n t = length t == n
                 && length (head t) == n
                 && length (filter (=="R") t) == n
                 && length (filter (==".") t) == n^2 -n
