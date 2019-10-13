
data Hora = H Int Int
            deriving Show

type Etapa = (Hora,Hora)

type Viagem = [Etapa]

v :: Viagem
v = [(H 9 30, H 10 25), (H 11 20, H 12 45), (H 13 20, H 14 45)]


horaValida :: Hora -> Bool
horaValida (H h m) = h >= 0 && h <= 23 && m >= 0 && m <= 59

testarEtapa :: Etapa -> Bool
testarEtapa (H h1 m1 , H h2 m2) = horaValida (H h1 m1) && horaValida (H h2 m2) && (h2 > h1 || (h1 == h2 && m2 > m1))


testarViagem :: Viagem -> Bool
testarViagem [e] = testarEtapa e
testarViagem (e1:e2:es) = testarEtapa e1 && testarEtapa (snd e1, fst e2) && testarViagem (e2:es)


horaPartArriv :: Viagem -> (Hora,Hora)
horaPartArriv viagem = (fst (head viagem), snd (last viagem))


horaMin :: Hora -> Int
horaMin (H h m) = 60*h + m

tempoViagem :: Viagem -> Int
tempoViagem [] = 0
tempoViagem (e1:es) = horaMin (fst e1) + horaMin (snd e1) + tempoViagem es

tempoEfetivo :: Viagem -> Int
tempoEfetivo [] = 0
tempoEfetivo (e:es) = duracaoEtapa e + tempoEfetivo es
        where duracaoEtapa (h1,h2) = horaMin h2 - horaMin h1


tempoEspera :: Viagem -> Int
tempoEspera [] = 0
tempoEspera [e] = 0
tempoEspera (e1:e2:es) = (horaMin (fst e2) - horaMin (snd e1)) + tempoEspera (e2:es)

tempoEspera' viagem = let (p,c) = horaPartArriv viagem
                          totalviagem = horaMin c - horaMin p
                      in totalviagem - (tempoViagem viagem)



data Ponto = Cartesiano Double Double | Polar Double Double
                                   deriving (Show, Eq)

type Poligonal  = [Ponto]


comprimento :: Poligonal -> Double
comprimento [] = 0
comprimento [x] = 0
comprimento (h:s:t) = (dist h s) + comprimento (s:t)
       where dist (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt ( (x1-x2)^2 + (y1-y2)^2 )


closeLine :: Poligonal -> Bool
closeLine fig = head fig == last fig


data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving (Show,Eq)

pt = [Cartesiano 0 2, Cartesiano 2 2,Cartesiano 2 0,Cartesiano 0 0, Cartesiano 0 2 ]

triangula :: Poligonal -> [Figura]
triangula (p1:p2:p3:[]) = []
triangula (p1:p2:p3:ps) = (Triangulo p1 p2 p3) : (triangula (p1:p3:ps))


area :: Poligonal -> Double
area (p1:p2:p3:[]) = 0
area (p1:p2:p3:ps) = (dist p1 p3)/2 * sqrt ( (dist p2 p3)^2)
         where dist (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt ( (x1-x2)^2 + (y1-y2)^2 )



data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]


acresEmail :: Nome -> String -> Agenda -> Agenda
acresEmail nome email (h:t) | nome == fst h = (nome, snd h ++ [Email email]) : t
                            | otherwise = h : acresEmail nome email t


verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [] = Nothing
verEmails nome (h:t) | nome == fst h = mostraEmail (snd h)
                     | otherwise = verEmails nome t


mostraEmail :: [Contacto] -> Maybe [String]
mostraEmail [] = Nothing
mostraEmail (Email a : t) = (Just [a])
mostraEmail (_:t) = mostraEmail t


consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs lista = telef lista

    where telef [] = []
          telef (Casa a : t) = a : telef t
          telef (Trab a : t) = a : telef t
          telef (Tlm a : t) = a : telef t
          telef (_:t) = telef t


casa :: Nome -> Agenda -> Maybe Integer
casa a [] = Nothing
casa a (h:t) | a == fst h = tfcasa (snd h)
             | otherwise = casa a t

      where tfcasa [] = Nothing
            tfcasa (Casa a :t) = Just a
            tfcasa (_ : t) = tfcasa t


type Dia = Int
type Mes = Int
type Ano = Int
--type Nome = String

data Data = D Dia Mes Ano
    deriving Show

type TabDN = [(Nome,Data)]


procura :: Nome -> TabDN -> Maybe Data
procura name [] = Nothing
procura name (h:t) | name == fst h = Just (snd h)
                   | otherwise = procura name t


idade :: Data -> Nome -> TabDN -> Maybe Int
idade date name [] = Nothing
idade date name (h:t) | name == fst h = diferenca date (snd h)
                      | otherwise = idade date name t

        where diferenca (D d1 m1 a1) (D d2 m2 a2) = Just (a1 - a2)

anterior :: Data -> Data -> Bool
-- considerando True se a primeira data for anterior a segunda
anterior (D d1 m1 a1) (D d2 m2 a2)| a2 > a1 = True
                                  | a2 < a1 = False
                                  | m2 > m1 = True
                                  | m2 < m1 = False
                                  | d2 > d1 = True
                                  | d2 < d1 = False
