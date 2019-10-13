import Data.Char


--Exercicio1

-- calcula perimetro circunf dado o raio
perimetro raio = 2 * pi * raio

-- calcula distancia entre dois pontos
dist :: (Double, Double) -> (Double, Double) -> Double
dist (a,b) (c,d) = sqrt ((a-c)^2 + (b-d)^2)

-- aprensenta o prim e ult elementos de uma lista
primUlt lista = (head lista, last lista)

-- testa se m é divisivel por n
multiplo :: Integer -> Integer -> Bool
multiplo m n = mod m n == 0

-- dada uma lista aprensenta a propria se o len for par ou a tail se len for impar
truncaImpar lista = if (mod (length lista) 2) == 0 then lista else tail lista

-- calcula maior de dois nrs
max2 :: Integer -> Integer -> Integer
max2 x y = if x>y then x else y

-- calcula maior de 3 nrs inteiros
max3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = if max2 x y > z then max2 x y else z

--Exercicio2

-- apresenta o nr de raizes do polinomio com os coeficientes apresentados
nRaizes a b c = if b^2 - 4*a*c == 0 then 1 else if b^2 - 4*a*c >= 1 then 2 else 0

-- calcula lista das raizes do polinomio
raizes a b c = if nRaizes a b c == 1 then [ ( (-b) / 2*a ) ] else if nRaizes a b c >= 1 then [(-b + sqrt (b^2 - 4*a*c))/2*a , (-b - sqrt (b^2 - 4*a*c))/2*a] else []

-- Exercicio3

type Hora = (Int, Int)

-- hora do dia valida?
horaValida :: Hora -> Bool

horaValida (a,b) = a >= 0 && a <= 23 && b >= 0 && b <= 59

horaValida' a = fst a>=0 && fst a<= 23 && snd a >= 0 && snd a <= 59

-- comparaçao hora
horaComp :: Hora -> Hora -> Bool
horaComp (a,b) (c,d) = if a > c then True else if a == c && b>d then True else if (a,b) == (c,d) then False else False

horaComp' (a,b) (c,d) = a>c ||
                        (a == c && b>d)

horaComp'' h1 h2 = let bh1 = horaMin h1
                       bh2 = horaMin h2
                   in bh1>bh2

-- horas para minutos
horaMin :: Hora -> Int
horaMin (a,b) = 60*a + b


-- minutos para horas
minHora :: Int -> Hora
minHora x = (div x 60, mod x 60 )

-- diferença horas
difHoras :: Hora -> Hora -> Int
difHoras (a,b) (c,d) = abs (horaMin (a,b) - horaMin (c,d))

-- adicionar minutos a horas
addMin :: Int -> Hora -> Hora
addMin x (a,b) = minHora ( horaMin (a,b) + x )


-- Exercicio4

data Hour = H Int Int
           deriving (Show, Eq)

h1 :: Hour
h1 = H 0 15

hourValida :: Hour -> Bool
hourValida (H h m) = h >= 0 && h <= 23 && m >= 0 && m <= 59

hourComp :: Hour -> Hour -> Bool
hourComp (H h1 m1) (H h2 m2) = if h1 > h2 then True else if h1 == h2 && m1>m2 then True else if (h1,m1) == (h2,m2) then False else False


-- Exercicio5

data Semaforo = Verde | Amarelo | Vermelho
      deriving (Show, Eq)

-- proximo estado semaforo
next :: Semaforo -> Semaforo
next x | x == Verde = Amarelo
       | x == Amarelo = Vermelho
       | x == Vermelho = Verde

-- determina obrigatorio parar em semaforo
stop :: Semaforo -> Bool
stop x = if x == Vermelho then True else False

-- estado de dois semaforos em cruzamento, seguro?
safe :: Semaforo -> Semaforo -> Bool
safe x y = x == Vermelho || y == Vermelho

-- Exercicio6

data Ponto = Cartesiano Double Double | Polar Double Double
             deriving (Show, Eq)

-- distancia ponto eixo vertical
posx :: Ponto -> Double
posx (Cartesiano x y) = x

-- distancia ponto eixo horaizontal
posy :: Ponto -> Double
posy (Cartesiano x y) = y

-- distancia ponto origem
raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x^2 + y^2)

-- angulo entre vetor (origem-ponto) e Ox
angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan (x/y)

-- distancia entre dois pontos
dista :: Ponto -> Ponto -> Double
dista (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt ( (x2-x1)^2 + (y2-y1)^2 )



-- Exercicio7

data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving (Show,Eq)

-- testa se figura é poligono
poligono :: Figura -> Bool
poligono (Circulo _ _) = False
poligono (Retangulo _ _) = True
poligono (Triangulo _ _ _) = True


-- calcula lista vertices figura
vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Triangulo p1 p2 p3) = [p1,p2,p3] --



-- Exercicio8


-- testa se char é min
isLower' :: Char -> Bool
isLower' a | a <= 'z' && a >= 'a' = True
           | otherwise = False


-- testa se char é digit
isDigit' :: Char -> Bool
isDigit' a | a >= '0' && a <= '9' = True
           | otherwise = False


-- testa se char é letra
isAlpha' :: Char -> Bool
isAlpha' a | a <= 'z' && a >= 'a' || a <= 'Z' && a >= 'A'= True
           | otherwise = False


-- converte letra para maiuscula
toUpper' :: Char -> Char
toUpper' a | a <= 'Z' && a >= 'A' = a
           | otherwise = chr (ord a - 32)


-- converte nr para digito
intToDigit' :: Int -> Char
intToDigit' x = chr (x + 48)


-- converte digito para nr
digitToInt' :: Char -> Int
digitToInt' a = ord a - 48




criaMatriz :: Int -> [[Int]]
criaMatriz 10 = []
criaMatriz x = aux x : criaMatriz (x+1)
 where
   aux n = if (n+1) <= 10 then n : aux (n+1) else []
