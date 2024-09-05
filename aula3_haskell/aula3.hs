
module Aula3 where

pegaPosicao ::  Int -> [Int]  -> Int
pegaPosicao i [] = (-1)
pegaPosicao i (x:xs)
    | i == 0 = x 
    | otherwise = pegaPosicao ( i - 1 ) xs 

retira :: Int -> [a] -> [a]
retira i [] = [] 
retira i (x:xs) 
    | i == 1 = xs
    | otherwise = retira ( i - 1 ) xs

num_elem :: [a] -> Int 
num_elem [] = 0
num_elem (x:xs) = 1 + num_elem xs

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs) 

ins :: Int -> [Int] -> [Int]
ins i [] =  i : []
ins i (x:xs) 
    | i <= x = i : x : xs 
    | i >= x = x : ins i xs

pega :: Int -> [Int] -> [Int]
pega i [] = []
pega i (x:xs) 
    | i /= 0 = x : pega ( i - 1 ) xs 
    | otherwise = []

pegaMaiores :: Int -> [Int] -> [Int]
pegaMaiores i [] = []
pegaMaiores i xs 
    | i == 0 = []
    | otherwise = maior xs : pegaMaiores (i - 1) ( retiraMaior xs )

retiraMaior :: [Int] -> [Int]
retiraMaior [] = []
retiraMaior (x:xs) 
    | x >= maior xs = xs
    | otherwise = x : retiraMaior xs

maior :: [Int] -> Int
maior [] = -1
maior (x:xs) 
    | x >= maior xs = x
    | otherwise = maior xs

mediaLista :: [Int] -> Int 
mediaLista [] = 0
mediaLista (x:xs) = div ( soma xs )( num_elem xs )

soma :: [Int] -> Int
soma [] = 0
soma (x:xs) = x + soma xs

contaMaiores :: Int -> [Int] -> Int
contaMaiores i [] = 0 
contaMaiores i xs
    | maior xs > i = 1 + contaMaiores i ( retiraMaior xs )
    | otherwise = 0

intercala :: [Int] -> [Int] -> [Int]
intercala [] [] = []
intercala (x:xs) [] = x : intercala xs []
intercala [] (y:ys) = y : intercala [] ys
intercala (x:xs) (y:ys) = x : y : intercala xs ys

dupli :: [Int] -> [Int]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

repli :: Int -> String -> String
repli i [] = "" 
repli i (x:xs) 
    | otherwise = ( criaLista i x ) ++ repli i xs 

criaLista :: Int -> Char -> String
criaLista 0 c = []
criaLista i c 
    | i == 0 = ""
    | otherwise = c : criaLista ( i - 1 ) c

dropEvery :: Int -> String -> String
dropEvery i [] = "" 
dropEvery i xs 
    | i > num_elem xs = xs
    | otherwise = getUntil i xs ++ dropEvery i ( getAfter i xs )

getAfter :: Int -> String -> String
getAfter i [] = ""
getAfter i (x:xs)
    | i == 0 = (x:xs)
    | otherwise = getAfter ( i - 1 ) xs

-- get n elements until not including "i"
getUntil :: Int -> String -> String
getUntil i [] = ""
getUntil i (x:xs) 
    | i <= 0 = ""
    | i == 1 = ""
    | otherwise = x : getUntil ( i - 1 ) xs

getUntil_Inc :: Int -> String -> String
getUntil_Inc i [] = ""
getUntil_Inc i (x:xs)
    | i <= 0 = ""
    | i == 0 = "" 
    | otherwise = x : getUntil_Inc( i - 1 ) xs

-- Fix not working
removeFromAt :: Int -> Int -> String -> String
removeFromAt i j [] = []
removeFromAt i j (x:xs) 
    | j >= num_elem xs = x : xs 
    | i >= num_elem xs = x : xs
    | i < 0 || j < 0 = xs
    | i == 0 = removeAt j (x:xs)
    | i /= 0 = x : removeFromAt ( i - 1 ) j xs
   
removeAt :: Int -> String -> String
removeAt j [] = []
removeAt j (x:xs) 
    | j < 0 = x : xs
    | j >= num_elem (x:xs) = x : xs
    | j /= 0 = x : removeAt ( j - 1 ) xs
    | j == 0 = xs

split :: Int -> String -> [String]
split i [] = [[]]
split i xs = [getUntil_Inc i xs, getAfter i xs]

getFirst :: [Int] -> Int
getFirst [] = 0
getFirst (x:xs) = x

getLast :: [Int] -> Int
getLast (x:xs) 
    | [] == xs = x
    | otherwise = getLast xs

maiorEmenor :: [Int] -> (Int, Int)
maiorEmenor [] = (0,0)
maiorEmenor xs = ( getFirst (iSort xs), getLast (iSort xs) )

