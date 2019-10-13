-- | f x y == x 'f' y

-- | foldr' :: (a -> b -> b) -> b -> [a] -> b
-- Reduz uma lista a um valor

sum'  = foldr (+) 0  --  => substitui ':' por '+' e '[]' por '0' => 1:3:5:[] = 1 + 3 + 5 + 0

product' = foldr (*) 1  --  => substitui ':' por '*' e '[]' por '1'=> 1:3:5:[] = 1 * 3 * 5 * 1

length' = foldl f 0
  where f x r = x + 1

length'' = foldr f 0
  where f x r = r + 1


anyy :: (a -> Bool) -> [a] -> Bool
anyy p [] = False
anyy p (x:xs) | p x = True
              | otherwise = anyy p xs

anyy' p [] = False
anyy' p (x:xs) = p x || anyy' p xs

--anyy'' p xs = foldr ((||) . p) False xs


zipwith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith f (x:xs) (y:ys) = f x y : zipwith f xs ys
zipwith f _ _ = []


takewhile :: (a -> Bool) -> [a] -> [a]
takewhile f [] = []
takewhile f (x:xs) | f x = x : takewhile f xs
                   | otherwise = []

dropwhile :: (a -> Bool) -> [a] -> [a]
dropwhile f [] = []
dropwhile f (x:xs) | f x = dropwhile f xs
                   | otherwise = x:xs


spann :: (a -> Bool) -> [a] -> ([a],[a])
spann f [] = ([],[])
spann f (x:xs) | f x = (x: listT, listD)
               | otherwise = ([],x:xs)

     where (listT,listD) = spann f xs


deleteby :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteby f a [] = []
deleteby f a (x:xs) | f a x = xs
                    | otherwise = x : deleteby f a xs


sorton ::  Ord b => (a -> b) -> [a] -> [a]
sorton f [] = []
--sorton f (x:xs)


type Polinomio = [Monomio]
type Monomio = (Float,Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n = filter f       -- filter :: (a -> Bool) -> [a] -> [a]
    where f (c,g) = g == n


conta :: Int -> Polinomio -> Int
conta n pol = length ( filter func pol )
    where func x = snd x == n

conta' n = foldr f 0
  where f (c,g) r | g == n = r+1
                  | otherwise = r

conta'' n = foldl f 0
  where f r (c,g) | g == n = r+1
                  | otherwise = r


grau :: Polinomio -> Int
grau [x] = snd x
grau (x:y:xs) | snd x >= snd y = grau (x:xs)
              | otherwise = grau (y:xs)

grau' :: Polinomio -> Int
grau' = foldl f 0
  where f r (c,g) | g > r = g
                  | otherwise = r

grau'' pol = let f r (c,g) = if g>r then g else r
             in foldl f 0 pol

grau''' = foldr (\ (c,g) r -> max g r) 0

deriv :: Polinomio -> Polinomio
deriv = map derivMonomio

derivMonomio :: Monomio -> Monomio
derivMonomio (c,g) = (c * fromIntegral g, g-1)


calcula :: Float -> Polinomio -> Float
calcula x pol = sum (map calc pol)
  where calc (c,g) = (x ^ g) * c

calcula' x = foldr f 0
  where f (c,g) r = c*x^g + r

calcula'' x = foldl f 0
  where f r (c,g) = r + c*x^g

calcula''' x pol = let f (c,g) r = c*x^g + r
                   in foldr f 0 pol

simp :: Polinomio -> Polinomio
simp = filter f
    where f (c,g) = c > 0


mult :: Monomio -> Polinomio -> Polinomio
mult (a,b) (x:xs) = map multiplica (x:xs)
   where multiplica (c,g) = (c*a,b+g)
