
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt


calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico expInt) = -(calcula expInt)
calcula (Mais expInt expInt2) = calcula expInt + calcula expInt2
calcula (Menos expInt expInt2) = calcula expInt - calcula expInt2
calcula (Mult expInt expInt2) = calcula expInt * calcula expInt2


infixa :: ExpInt -> String
infixa (Const x) = show x
infixa (Simetrico expInt) = "-(" ++ infixa expInt ++ ")"
infixa (Mais expInt expInt2) = "(" ++ (infixa expInt) ++ "+" ++ (infixa expInt2) ++ ")"
infixa (Menos expInt expInt2) = "(" ++ (infixa expInt) ++ "+" ++ (infixa expInt2) ++ ")"
infixa (Mult expInt expInt2) = "(" ++ (infixa expInt) ++ "*" ++ (infixa expInt2) ++ ")"


posfixa :: ExpInt -> String
posfixa (Const x) = show x
posfixa (Simetrico expInt) = "-(" ++ posfixa expInt ++ ")"
posfixa (Mais expInt expInt2) = posfixa expInt ++ " " ++ posfixa expInt2 ++ " " ++ "+"
posfixa (Menos expInt expInt2) = posfixa expInt ++ " " ++ posfixa expInt2 ++ " " ++ "-"
posfixa (Mult expInt expInt2) = posfixa expInt ++ " " ++ posfixa expInt2 ++ " " ++ "*"



data RTree a = R a [RTree a]
             deriving Show

rtree :: RTree Int
rtree = R 2 [R 1 [R 1 []],R 6 [],R 1 []]

rt :: RTree Int
rt = R 5 [ R 7 []
         , R 2 [R 4 []
              , R 5 [] ]
         , R 4 [ R 7 [] ]
         ]

soma :: Num a => RTree a -> a
soma (R a []) = a
soma (R a list) = a + sum (map soma list)


altura :: RTree a -> Int
altura (R a []) = 1
altura (R v l) = 1 + maximum (map altura l )


prune :: Int -> RTree a -> RTree a
prune 1 (R v l) = R v []
prune n (R v l) = R v (map (prune (n-1)) l)


mirror :: RTree a  -> RTree a
mirror (R v []) = R v []
mirror (R v l) = R v (reverse (map mirror l))


postorder :: RTree a -> [a]
postorder (R v []) = [v]
postorder (R v l) = foldr (++) [v] (map postorder l)
