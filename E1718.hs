ind :: [a] -> Int -> a
ind (h:t) 0 = h
ind (h:t) i = ind t (i-1)

data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (a,b) (Norte:t) = posicao (a,b+1) t
posicao (a,b) (Sul:t) = posicao (a,b-1) t
posicao (a,b) (Este:t) = posicao (a+1,b) t
posicao (a,b) (Oeste:t) = posicao (a-1,b) t

anny :: (a -> Bool) -> [a] -> Bool
anny f [] = False
anny f (h:t) | f h = True
             | otherwise = anny f t

type Mat a = [[a]]

zero :: (Num a,Eq a) => [a] -> Bool
zero [] = True
zero (h:t) | h == 0 = zero t
           | otherwise = False

el :: (Num a,Eq a) => Mat a -> Int -> Bool
el [] v = True
el ((h:t):m) 0 = el m 1
el ((h:t):m) v | zero (take v (h:t)) = el m (v+1)
               | otherwise = False

triSup :: (Num a,Eq a) => Mat a -> Bool
triSup matriz = el matriz 0

movimenta :: IO (Int,Int)
movimenta = moveFrom (0,0)

moveFrom :: (Int,Int) -> IO (Int,Int)
moveFrom (x,y) = do
  dir <- getChar
  case dir of 'n' -> moveFrom (x,y+1)
              's' -> moveFrom (x,y-1)
              'e' -> moveFrom (x+1,y)
              'o' -> moveFrom (x-1,y)
              'w' -> moveFrom (x-1,y)
              otherwise -> return (x,y)

teclas :: String -> (Int,Int) -> (Int,Int)
teclas [] (a,b) = (a,b)
teclas ('n':t) (a,b) = teclas t (a,b+1)
teclas ('s':t) (a,b) = teclas t (a,b-1)
teclas ('e':t) (a,b) = teclas t (a+1,b)
teclas ('o':t) (a,b) = teclas t (a-1,b)
teclas _ (a,b) = (a,b)
{-
main :: IO (Int,Int)
main = do input <- getChar
          print (teclas input (0,0))
-}

ex :: Imagem
ex = Mover (5,5)
      (Juntar [Mover (0,1) (Quadrado 5),
              Quadrado 4,
              Mover (4,3) (Quadrado 2)])

data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]

vazia :: Imagem -> Bool
vazia (Quadrado d) = False
vazia (Mover p i) = vazia i
vazia (Juntar []) = True
vazia (Juntar (h:t)) = vazia h && vazia (Juntar t)

quadrados :: Imagem -> [Int]
quadrados (Quadrado d) = [d]
quadrados (Mover p i) = quadrados i
quadrados (Juntar []) = []
quadrados (Juntar (h:t)) = quadrados h ++ quadrados (Juntar t)

maior :: Imagem -> Maybe Int
maior img = Just (maximum $ quadrados img)


ordena :: [Int] -> [Int]
ordena [] = []
ordena (h:t) = insert h (ordena t)
   where
     insert a [] = [a]
     insert a (h:t) | a < h = a:h:t
                        | otherwise = h:insert a t

equal :: Imagem -> Imagem -> Bool
equal img1 img2 = ordena(quadrados img1) == ordena(quadrados img2)

instance Eq  Imagem where
  (==) = equal
