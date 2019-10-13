import System.Random

bingo :: IO ()
bingo = do list <- acumulaNr [] -- where list == lista de nr saidos
           print list

acumulaNr :: [Int] -> IO [Int]
acumulaNr list | length list == 90 = return list
               | otherwise = do  a <- randomRIO (1,90)
                                 print a
                                 getChar
                                 let newL = if elem a list
                                               then list
                                               else a:list in acumulaNr newL


mastermind :: IO ()
mastermind = do sk <- secretKey
                digits <- digitosIn
                fim <- finish sk digits
                print fim


secretKey :: IO (Int,Int,Int,Int)   -- gera 4 digitos aleatoriamente
secretKey = do a <- randomRIO (0,9)
               b <- randomRIO (0,9)
               c <- randomRIO (0,9)
               d <- randomRIO (0,9)
               return (a,b,c,d)


digitosIn :: IO (Int,Int,Int,Int)  -- recebe a tentativa (digitos) do jogador
digitosIn = do a <- getChar
               b <- getChar
               c <- getChar
               d <- getChar
               return (read [a],read [b],read [c],read [d])

corretos :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> Int  -- diz quantos elementos estão corretos (nas posições corretas)
corretos (a,b,c,d) (x,y,z,w) = length (filter (== True) [a==x,b==y,c==z,d==w])


finish :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> IO (Int,Int,Int,Int)
finish key digits | corretos key digits == 4 = return key
                  | otherwise = do new <- digitosIn
                                   finish key new
