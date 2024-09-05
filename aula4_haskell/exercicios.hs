
data Arvore = Folha Int | Nodo Int Arvore Arvore
    deriving(Eq, Show)

arv1 = Nodo 3 ( Folha 2 ) ( Nodo 4 ( Folha 3 ) ( Folha 2 ) )
arv2 = Nodo 10 ( Nodo 14 ( Nodo 1 ( Folha 4 ) ( Folha 2 ) ) (Folha 6)) (Folha 9)

multArvore :: Int -> Arvore -> Arvore
multArvore i (Folha n) = Folha( n * i )
multArvore i (Nodo n a1 a2) =
              Nodo ( n * i ) ( multArvore i a1 ) ( multArvore i a2 ) 

contaFolhas :: Arvore -> Int
contaFolhas (Folha n) = 1
contaFolhas (Nodo n a1 a2) =
    contaFolhas a1 + contaFolhas a2

contaNodos :: Arvore -> Int
contaNodos (Folha n) = 0
contaNodos (Nodo n a1 a2) =
        1 + contaNodos a1 + contaNodos a2

quantasVezes :: Int -> Arvore -> Int
quantasVezes i (Folha n)  
    | i == n = 1
    | otherwise = 0
quantasVezes i (Nodo n a1 a2) 
    | i == n = 1 + quantasVezes i a1 + quantasVezes i a2
    | otherwise = quantasVezes i a1 + quantasVezes i a2 

maxArvore :: Arvore -> Int
maxArvore (Folha n) = n
maxArvore (Nodo n a1 a2) = max (max n (maxArvore a1)) (maxArvore a2)

refleteArvore :: Arvore -> Arvore
refleteArvore (Folha n) = Folha n
refleteArvore (Nodo n a1 a2)  =
        Nodo n ( refleteArvore a2 ) ( refleteArvore a1 )

geraLista :: Arvore -> [Int]
geraLista (Folha n) = [n]
geraLista (Nodo n a1 a2) =
    [n] ++ geraLista a1 ++ geraLista a2
    
