
sinal :: Int -> Int
sinal i
  | i < 0 = (-1)
  | i > 0 = 1
  | otherwise = 0

menorTres x y z 
    | x <= y && x <= z = x
    | y <= x && y <= z = y
    | otherwise = z

uns :: [ Int ]
uns = 1:uns

fat :: Int -> Int
fat 0 = 1
fat 1 = 1
fat n = n * fat( n - 1 )

potencia :: Int -> Int -> Int

potencia 0 0 = error "undefined"
potencia x 0 = 1
potencia x 1 = x
potencia x n = x * potencia x (n - 1)  

osQuatroIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroIguais x y z k = ( x == y ) && ( y == z ) && ( z == k ) 

quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais x y z
    | x == y && x == z = 3
    | x == z = 2
    | y == z = 2 
    | x == y = 2
    | otherwise = 0

todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes n m p = ( ( n/=m ) && ( m /= p ) && ( n /= p ) )

todosIguais x y z = ( x == y && y == z )

quantosSaoIguais2 x y z 
    | todosDiferentes x y z = 0
    | todosIguais x y z = 3 
    | x == y = 2
    | y == z = 2

elevadoDois :: Int -> Int
elevadoQuatro :: Int -> Int

elevadoDois x = x * x
elevadoQuatro x = elevadoDois(x) * elevadoDois(x)

palindromo :: String -> Bool
palindromo s = s == reverse s 

vendas :: Int -> Int
vendas i = i

vendaTotal :: Int -> Int

vendaTotal 0 = vendas(0) 
vendaTotal n = vendaTotal( n - 1 ) + vendas(n)

