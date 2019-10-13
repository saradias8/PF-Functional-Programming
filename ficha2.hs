import Data.Char


funA :: [Double] -> Double   -- funA [2,3,5,1]
funA [] = 0                  -- = funA (2: 3: 5: 1 :[])
funA (x:y) = x^2 + funA y    -- = 2^2 + funA (3:5:1:[])
                             -- = 4 + 3^2 + funA (5:1:[])
                             -- = 4 + 9 + 5^2 + funA (1:[])
                             -- = 4 + 9 + 25 + 1^2 + funA []
                             -- = 4 + 9 + 25 + 1 + 0 = 39


funB :: [Int] -> [Int]                         -- funB [8,5,12]
funB [] = []                                   -- = funB (8:5:12:[])
funB (h:t) = if (mod h 2 == 0) then h:(funB t) -- = (8:funB(5:12:[]))
                               else (funB t)   -- = (8:funB (12:[]))
                                               -- = (8:12:funB [])
                                               -- = (8:12:[]) = [8,12]


funC (x:y:t) = funC t   -- funC [1,2,3,4,5]
funC [x] = []           -- = funC (1:2:3:4:5:[])
funC [] = []            -- = funC (3:4:5:[])
                        -- = funC (5:[])
                        -- = []


funD l = g [] l       -- funD "otrec"
g l [] = l            -- = funD ['o', 't', 'r', 'e', 'c']
g l (h:t) = g (h:l) t -- =



dobros :: [Float] -> [Float]  -- dobros [1,2,3,4]
dobros [] = []                -- = dobros (1:2:3:4:[])
dobros (x:y) = 2*x : dobros y -- = (2*1 : dobros (2:3:4:[])
                              -- = (2*1 : 2*2 : dobros (3:4:[]))
                              -- = (2*1 : 2*2 : 3*2 : dobros (4:[]))
                              -- = (2*1 : 2*2 : 3*2 : 4*2 : dobros [])
                              -- = (2:4:6:8:[]) = [2,4,6,8]


numOcorre :: Char -> String -> Int
numOcorre char [] = 0
numOcorre char (h:t) =
  if char == h then 1 + numOcorre char t
  else numOcorre char t



positivos :: [Int] -> Bool
positivos [x] = x>0
positivos (x:y) = x == abs x && positivos y



soPos :: [Int] -> [Int]                      -- soPos [1,-2,3]
soPos [] = []
soPos (x:y) = if abs x == x then x: soPos y  -- = soPos (1: -2: 3: [])
              else soPos y                   -- = (1: soPos (-2:3:[]))
                                             -- = (1: soPos (3:[]))
                                             -- = (1: 3: soPos [])
                                             -- = (1:3:[]) = [1,3]

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (x:y) = if x<0 then x + somaNeg y
                else somaNeg y


tresUlt :: [a] -> [a]                                               -- tresUlt [1,2,3,4]
tresUlt (h:t) = if length (h:t) <= 3 then (h:t)                     -- = tresUlt (1:2:3:4:[])
                else tresUlt t                                      -- = tresUlt (2:3:4:[])
                                                                    -- = (2:3:4)


segundos :: [(a,b)] -> [b]
segundos [] = []
segundos (h:t) = (snd h) :segundos t


nosPrimeiros :: Eq a => a -> [(a,b)] -> Bool                         -- nosPrimeiros [(1,2),(2,3)] 2
nosPrimeiros el [] = False                                           --
nosPrimeiros el ((x,y):t) | el == x = True
                          | otherwise = nosPrimeiros el t


sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((x,y,z):t) = (x+p,y+q,z+r)
        where (p,q,r) = sumTriplos t


soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) | h >= '0' && h <= '9' = h: soDigitos t
                | otherwise = soDigitos t


minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) | h >= 'a' && h <= 'z' = 1 + minusculas t
                 | otherwise = minusculas t


nums :: String -> [Int]
nums [] = []
nums (h:t) | h >= '0' && h <= '9' = ( ord h - 48 ): nums t
           | otherwise = nums t



type Polinomio = [Monomio]
type Monomio = (Float,Int)


conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n (h:t) | snd h == n = 1 + conta n t
              | otherwise = conta n t


grau :: Polinomio -> Int
grau (h:t) = maiorG (h:t) 0
     where maiorG [] n = n
           maiorG (h:t) n | snd h >= n = maiorG t (snd h)
                          | otherwise = maiorG t n


selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n (h:t) | snd h == n = h : selgrau n t
                | otherwise = selgrau n t


deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((x,y):t) = (x * (fromIntegral y) , y -1 ) : deriv t


{-calcula :: Float -> Polinomio -> Float
calcula x [] = 0
calcula 0 (h:t) | snd h == 0 = (toInteger fst h) + calcula 0 t
                | otherwise = calcula 0 t

calcula x (h:t) = (toInteger fst h) * (x ** snd h) + calcula x t
-}

simp :: Polinomio -> Polinomio
simp [] = []
simp (h:t) | fst h == 0 = simp t
           | otherwise = h : simp t


normaliza :: Polinomio -> Polinomio
normaliza [x] = [x]
normaliza (h:i:t) | snd h == snd i = ((fst h + fst i),snd h) : normaliza t
                  | otherwise = h : normaliza (i:t)


soma :: Polinomio -> Polinomio -> Polinomio
soma [] pol = pol
soma pol [] = pol
soma (h:t) (i:j) | snd h == snd i = ((fst h + fst i), snd h) : soma t j
                 | otherwise = h : soma t (i:j)


ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (h:t) = insere h (ordena t)
     where insere a [] = [a]
           insere (x,y) (h:t) | y <= snd h = (x,y) : insere h t
                              | otherwise = h : insere (x,y) t


{-equiv :: Polinomio -> Polinomio -> Bool
equiv [] [] = True
equiv [] _ = False
equiv _ [] = False
equiv (x:xs) (y:ys) | fst x == fst y && snd x == snd y = equiv xs ys
                    | otherwise =
-}
