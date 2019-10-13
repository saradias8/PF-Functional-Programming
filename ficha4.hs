import Data.Char

digitAlpha :: String -> (String,String)
digitAlpha [] = ("","")
digitAlpha (h:t) | isDigit h = (h:d,a)
                 | isAlpha h = (d,h:a)
                 | otherwise = (d,a)
    where
      (d,a) = digitAlpha t

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h < 0 = (n+1,z,p)
          | h > 0 = (n,z,p+1)
          | h == 0 = (n,z+1,p)
   where
     (n,z,p) = nzp t
