
somaLista :: [Int] -> Int
--somaLista [] = 0
--somaLista (x:xs) = x + somaLista xs
somaLista = foldr (+) 0 
    
multDois :: [Int] -> [Int]
--multDois [] = []
--multDois (x:xs) = 2*x : multDois xs
multDois = map ( 2 * )

multLista :: Int -> [Int] -> [Int]
--multLista i [] = []
--multLista i (x:xs) = i * x : ( multLista i xs )
multLista i = map ( i * ) 

elemento :: Int -> [Int] -> Bool
elemento i [] = False
elemento i (x:xs) = ( i == x ) || elemento i xs 
  --  | i == x = True
  --  | otherwise = elemento i xs
    
conta :: Int -> [Int] -> Int
conta i [] = 0
conta i (x:xs) 
    | i == x = 1 + conta i xs 
    | otherwise = conta i xs

contaMaiores :: Int -> [Int] -> Int
contaMaiores i [] = 0
contaMaiores i (x:xs)
    | i < x = 1 + contaMaiores i xs
    | otherwise = 0 + contaMaiores i xs

maiores :: Int -> [Int] -> [Int]
maiores i [] = []
maiores i (x:xs) 
    | i < x = x : maiores i xs
    | otherwise = maiores i xs

geraLista :: Int -> Int -> [Int]
geraLista 0 0 = []
geraLista m n 
    | m == 0 = []
    | otherwise = n : geraLista (m - 1) n 

addFim :: Int -> [Int] -> [Int]
addFim i [] = [i]
addFim i (x:xs) = x : addFim i xs

join :: [Int] -> [Int] -> [Int]
join [] [] = []
join (x:xs) ys = x : join xs ys 
join [] (y:ys) = y : join [] ys

inverte :: [Int] -> [Int]
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]

membro :: Int -> [Int] -> Bool
membro = elemento 

membroNum :: Int -> [Int] -> Int
membroNum = conta 

membroUsandoMN :: Int -> [Int] -> Bool
membroUsandoMN i xs = ( membroNum i xs ) > 0

removeItem :: Int -> [Int] -> [Int]
removeItem i [] = []
removeItem i (x:xs) 
    | i == x = removeItem i xs
    | otherwise = x : removeItem i xs

-- Fix this one
unico :: [Int] -> [Int]
unico [] = []
unico (x:xs) 
    | membroNum x xs /= 0 = unico ( removeItem x xs )
    | otherwise = x : unico xs

-- Try to understand better 
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = quickSort ( menores_an x xs )
                   ++ [x] ++ 
                   quickSort ( maiores_dp x xs )

menores_an :: Int -> [Int] -> [Int]
menores_an i [] = []
menores_an i (x:xs) 
    | i > x = x : menores_an i xs
    | otherwise = menores_an i xs
    
maiores_dp :: Int -> [Int] -> [Int]
maiores_dp i [] = []
maiores_dp i (x:xs) 
    | i < x = x : maiores_dp i xs
    | otherwise = maiores_dp i xs


