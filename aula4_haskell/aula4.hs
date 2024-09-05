
data Temperatura = Frio | Calor
    deriving(Eq, Show)

data Estacao = Outono | Inverno | Primavera | Verao
    deriving(Eq, Show)

tempo :: Estacao -> Temperatura
tempo Verao = Calor
tempo _ = Frio

data Forma = Circulo Float | Retangulo Float Float
    deriving(Eq, Show)

redondo :: Forma -> Bool
redondo (Circulo x) = True
redondo (Retangulo x y) = False

area :: Forma -> Float
area (Circulo r) = pi * r ** 2
area (Retangulo b h) = b * h

data Arvore = Folha Int | Nodo Int Arvore Arvore
    deriving(Eq, Show)

arv1 = Nodo 3 ( Folha 2 ) ( Nodo 4 ( Folha 3 ) ( Folha 2 ) )
arv2 = Nodo 10 ( Nodo 14 ( Nodo 1 ( Folha 4 ) ( Folha 2 ) ) (Folha 6)) (Folha 9)

somaArvore :: Arvore -> Int
somaArvore (Folha n) = n
somaArvore (Nodo n a1 a2) =
            n + somaArvore a1 + somaArvore a2

multDoisArvore :: Arvore -> Arvore
multDoisArvore (Folha n) = Folha ( 2 * n )
multDoisArvore (Nodo n a1 a2) = 
            Nodo ( 2 * n ) ( multDoisArvore a1 ) ( multDoisArvore a2 )

