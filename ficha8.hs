import Data.List

data Cor = Vermelho Int
         | Verde Int
         | Azul Int


e :: Cor
e = Vermelho 20
v = Verde 80
a = Azul 50

lc :: [Cor]
lc = [e,v,a]

coresEq :: Cor -> Cor -> Bool
coresEq (Verde _) (Verde _)       = True
coresEq (Vermelho _) (Vermelho _) = True
coresEq (Azul _) (Azul _)         = True
coresEq _ _                       = False


instance Eq Cor where
  (==) = coresEq


intensidade :: Cor -> Int
intensidade (Verde i) = i
intensidade (Vermelho i) = i
intensidade (Azul i) = i

compareCor :: Cor -> Cor -> Bool
compareCor c1 c2 = intensidade c1 > intensidade c2

instance Ord Cor where
  (<=) a b  = compareCor b a


showCor :: Cor -> String
showCor (Vermelho i) | i <= 20 = "Vermelho Claro"
                     | i > 75 = "Vermelho Escuro"
                     | otherwise = "Vermelho"
showCor (Verde i) | i <= 20 = "Verde Claro"
                  | i > 75 = "Verde Escuro"
                  | otherwise = "Verde"
showCor (Azul i) | i <= 20 = "Azul Claro"
                 | i > 75 = "Azul Escuro"
                 | otherwise = "Azul"

instance Show Cor where
  show = showCor



data (Exp a) = Const a
            | Simetrico (Exp a)
            | Mais (Exp a) (Exp a)
            | Menos (Exp a) (Exp a)
            | Mult (Exp a) (Exp a)

calcula :: Num a => Exp a -> a
calcula (Const x)              = x
calcula (Simetrico expInt)     = -(calcula expInt)
calcula (Mais expInt expInt2)  = calcula expInt + calcula expInt2
calcula (Menos expInt expInt2) = calcula expInt - calcula expInt2
calcula (Mult expInt expInt2)  = calcula expInt * calcula expInt2


infixa :: Show a => Exp a -> String
infixa (Const x)              = show x
infixa (Simetrico expInt)     = "-(" ++ (infixa expInt) ++ ")"
infixa (Mais expInt expInt2)  = "(" ++ (infixa expInt) ++ "+" ++ (infixa expInt2) ++ ")"
infixa (Menos expInt expInt2) = "(" ++ (infixa expInt) ++ "+" ++ (infixa expInt2) ++ ")"
infixa (Mult expInt expInt2)  = "(" ++ (infixa expInt) ++ "*" ++ (infixa expInt2) ++ ")"

instance Show a => Show (Exp a) where
  show = infixa

expEq :: (Num a,Eq a) => Exp a -> Exp a -> Bool
expEq e1 e2 = calcula e1 == calcula e2

instance (Num a, Eq a) => Eq (Exp a) where
  (==) e1 e2 = expEq e1 e2


-- Num -> +, -, *, abs, signum, fromInteger

somaExp :: Num a => Exp a -> Exp a -> Exp a
somaExp e1 e2 = Const ((calcula e1) + (calcula e2))

subExp :: Num a => Exp a -> Exp a -> Exp a
subExp e1 e2 = Const ((calcula e1) - (calcula e2))

multExp :: Num a => Exp a -> Exp a -> Exp a
multExp e1 e2 = Const ((calcula e1) * (calcula e2))

absExp :: Num a => Exp a -> Exp a
absExp e1 = Const (abs (calcula e1))

sigExp :: Num a => Exp a -> Exp a
sigExp e1 = Const (signum (calcula e1))

fIntExp :: Num a => Integer -> Exp a
fIntExp i = Const (fromInteger i)


instance Num a => Num (Exp a) where
  (+) = somaExp
  (-) = subExp
  (*) = multExp
  abs = absExp
  signum = sigExp
  fromInteger = fIntExp
