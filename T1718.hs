import Data.List
import System.Random

insertt :: Ord a => a -> [a] -> [a]
insertt a (h:t) | h > a = a:h:t
                | otherwise = h:insertt a t

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just a:t) = a : catMaybes t
catMaybes (_:t) = catMaybes t

data Exp a = Const a
           | Var String
           | Mais (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)


instance Show a => Show (Exp a) where
  show (Const a) = show a
  show (Var a) = a
  show (Mais e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Mult e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"


sorton :: Ord b => (a -> b) -> [a] -> [a]
sorton f [] = []
sorton f (h:t) = inser h (sorton f t)
   where inser h [] = [h]
         inser h (x:xs) | f h < f x = h : inser x xs
                        | otherwise = x : inser h xs


amplitude :: [Int] -> Int
amplitude [] = 0
amplitude list = aux 0 0 list
  where
    aux a b [] = a-b
    aux 0 0 (h:t) = aux h h t
    aux a b (h:t) = aux (max a h) (min b h) t


aux :: [Int] -> ([Int],[Int])
aux (h:t) | maximum t - h > h - minimum t = (h:fst (aux t),snd (aux t))
          | otherwise = (fst(aux t),h:snd (aux t))

parte :: [Int] -> ([Int],[Int])
parte [] = ([],[])
parte list = (a,b)
   where
     (a,b) = aux (sort list)


data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]
      deriving Show

ex :: Imagem
ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5),
                          Quadrado 4,
                          Mover (4,3) (Quadrado 2)])

conta :: Imagem -> Int
conta (Quadrado i) = 1
conta (Mover d i) = conta i
conta (Juntar []) = 0
conta (Juntar (h:t)) = conta h + conta (Juntar t)


erase :: Int -> Imagem -> Imagem
erase x (Quadrado i) | x == 0 = Juntar []
                     | otherwise = Quadrado i
erase x (Mover d i) | x == 0 = Mover d (erase x i)
                    | otherwise = erase (x-1) i
erase x (Juntar i) = Juntar (take x i ++ drop (x+1) i)

apaga :: Imagem -> IO Imagem
apaga img = do x <- randomRIO (0,(conta img)-1)
               return (erase x img)
