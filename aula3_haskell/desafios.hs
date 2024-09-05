
module Desafios where

retira :: Int -> [a] -> [a]
retira i [] = [] 
retira i (x:xs) 
    | i == 1 = (x:xs)
    | otherwise = retira ( i - 1 ) xs

getAfter :: Int -> String -> String
getAfter i [] = ""
getAfter i (x:xs)
    | i == 0 = (x:xs)
    | otherwise = getAfter ( i - 1 ) xs

removeAfter :: Int -> String -> String
removeAfter i [] = ""
removeAfter i (x:xs)
    | i == 0 = ""
    | otherwise = x : removeAfter ( i - 1 ) xs

slice :: Int -> Int -> String -> String
slice x y [] = []
slice x y xs = removeAfter (y - x) ( retira x xs )  

compress :: String -> String
compress "" = ""
compress (x:xs) 
    | x == next xs = compress xs
    | otherwise = x : compress xs

next :: String -> Char
next (x:xs) = next xs
    





