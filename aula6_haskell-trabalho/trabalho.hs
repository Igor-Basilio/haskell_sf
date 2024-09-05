-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
      |Div E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E    -- menor ou igual
      | Le E E    -- menor ou igual
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | DoWhile C B      ---- Do C While B: executa C enquanto B avalie para verdadeiro
    | Unless B C C   ---- Unless B C1 C2: se B avalia para falso, então executa C1, caso contrário, executa C2
    | Loop E C    --- Loop E C: Executa E vezes o comando C
    | Swap E E --- recebe duas variáveis e troca o conteúdo delas
    | DAttrib E E E E -- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.
   deriving(Eq,Show)                


-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):


type Memoria = [(String,Int)]

--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
--- *Main> procuraVar exSigma "x"
--- 10

procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v

--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
--- variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. A
--- chamada
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
---
---
--- essa chamada é equivalente a operação exSigma[temp->20]

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n

-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------

ebigStep :: (E,Memoria) -> Int
ebigStep (Var x,s) = procuraVar s x
ebigStep (Num n,s) = n
ebigStep (Soma e1 e2,s)  = ebigStep (e1,s) + ebigStep (e2,s)
ebigStep (Sub e1 e2,s)  = ebigStep (e1,s) - ebigStep (e2,s)
ebigStep (Mult e1 e2,s)  = ebigStep (e1,s) * ebigStep (e2,s)
ebigStep (Div e1 e2,s)  = div ( ebigStep (e1,s) ) ( ebigStep (e2,s) )

---------------------------------- *

bbigStep :: (B,Memoria) -> Bool
bbigStep (TRUE, s)  = True
bbigStep (FALSE,s) = False

bbigStep (Not b, s) 
   | bbigStep (b,s) == True     = False
   | otherwise                  = True 

bbigStep (And b1 b2,s )  
    | bbigStep (b1,s) == True = bbigStep(b2,s)
    | otherwise = False

bbigStep (Or b1 b2,s )
    | bbigStep(b1, s) == False = bbigStep(b2, s)
    | otherwise = True

bbigStep (Leq e1 e2, s) = ebigStep ( e1, s ) <= ebigStep ( e2, s )
bbigStep (Le e1 e2, s) = ebigStep ( e1, s ) < ebigStep ( e2, s )
bbigStep (Igual e1 e2, s) = ebigStep( e1, s ) == ebigStep ( e2, s )

---------------------------------- *

cbigStep :: (C,Memoria) -> (C,Memoria)
cbigStep (Skip,s) = (Skip,s)

cbigStep (If b c1 c2, s)  
    | bbigStep(b, s) == True = cbigStep( c1, s )
    | otherwise = cbigStep( c2, s )

cbigStep (Seq c1 c2, s) =
    (Skip, s2) where
      ( _, s2) = cbigStep(c2, s1)
      ( _, s1) = cbigStep(c1, s)                          

cbigStep (Atrib (Var x) e, s) = (Skip, (mudaVar s x (ebigStep(e, s))) )
     
cbigStep (While b  c, s)  
    | bbigStep( b, s ) == True = cbigStep( (Seq c (While b c), s) )
    | otherwise = ( Skip, s )

cbigStep (DoWhile c b, s) =
    let
        (_, s1) = cbigStep (c, s)
    in
        if bbigStep (b, s1) == False
        then (Skip, s1)  
        else cbigStep (DoWhile c b, s1)

cbigStep (Loop e c, s)  
    | bbigStep( (Le (Num 0) e), s ) = cbigStep (  Seq c (Loop (Sub e (Num 1)) c) , s )
    | otherwise = (c, s)

--Unless B C C   ---- Unless B C1 C2: se B avalia para falso, então executa C1, caso contrário, executa C2
cbigStep (Unless b c1 c2, s) 
    | bbigStep(b, s) == False = cbigStep( c1, s )
    | otherwise = cbigStep( c2, s )

cbigStep (Swap (Var x) (Var y), s) = ( Skip, ( mudaVar ( mudaVar s x (procuraVar s y) ) y (procuraVar s x) ) )
    
cbigStep (DAttrib (Var x) (Var y) e1 e2, s) =
 (Skip, mudaVar ( mudaVar s x (ebigStep (e1, s) ) ) y (ebigStep (e2, s)) )            

--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM:
--- * Loop             
--- * Dupla Atribuição
--- * Do While       
-------------------------------------

-- Exemplos feitos : 

exSigma3 :: Memoria
exSigma3 = [ ("sum", 0), ("i", 0), ("n", 10) ]

-- Soma dos números naturais até n ( Loop ) 
natural_sum :: C                                  
natural_sum = Seq ( Atrib (Var "sum") (Num 0) ) ( Loop (Var "n") ( Seq (Atrib (Var "i") (Soma (Var "i") (Num 1)))
                                                ( Atrib (Var "sum") (Soma (Var "sum") (Var "i") ) ) ) ) 

progExpLoop :: C 
progExpLoop = Loop (Num 2) ( Atrib (Var "x" ) (Soma (Num 3) (Soma (Var "x") (Var "y"))) )
-- ( x + y + 3 ) 

-- Número de digitos de um número ( Do While ) em decimal
-- Ex : 2 -> 1, 341 -> 3, 923942 -> 6 
-- Funciona fazendo shift ( divisão por 10 ) 
exSigma4 :: Memoria
exSigma4 = [ ("count", 0), ("num", 1011) ]

digit_count :: C
digit_count = Seq ( Atrib (Var "count") (Num 0) ) 
                  ( DoWhile ( Seq ( Atrib (Var "num") (Div (Var "num") (Num 10) ) )
                  ( Atrib (Var "count") (Soma (Var "count") (Num 1) ) ) ) 
                  ( Not ( Igual (Var "num") (Num 0) ) ) )

-- Fibonnaci
exSigma5 :: Memoria
exSigma5 = [ ("x", 0), ("y", 1), ("times", 10), ("fib", 0) ]

fibonnaci :: C
fibonnaci = Seq ( DAttrib (Var "x") (Var "y") (Num 0) (Num 1) )
                ( Seq (Atrib (Var "fib") (Num 0)) (Loop (Sub (Var "times") (Num 1))
                ( Seq (Atrib (Var "fib") (Soma (Var "x") (Var "y")))
                ( (DAttrib (Var "x") (Var "y") (Var "y") (Var "fib")) )) ) )
                  
-- Minimo de três valores ( "a" terá o minimo )
exSigma6 :: Memoria
exSigma6 = [ ("a", 923), ("b", 283182), ("c", 92) ]

minimo_tres :: C
minimo_tres = Seq    ( Seq  ( Unless (Leq (Var "a") (Var "b")) 
                     ( Swap (Var "b") (Var "a") )
                     ( Skip ) )

                     ( Unless (Leq (Var "a") (Var "c"))
                     ( Swap (Var "a") (Var "c") )
                     ( Skip ) ) )
                    
                     ( Unless (Leq (Var "b") (Var "c" )) 
                     ( Swap (Var "b") (Var "c") )
                     ( Skip ) ) 

--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação inicial  fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- *Main> ebigStep (progExp1, exSigma)
-- 13
-- *Main> ebigStep (progExp1, exSigma2)
-- 6

--- Para rodar os próximos programas é necessário primeiro implementar as regras da semântica
---
---
--- Exemplos de expressões booleanas:

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",3), ("z",2)]

teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))
-- True - exSigma
-- True - exSigma2

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))
-- False - exSigma
-- True  - exSigma2

---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) (Atrib (Var "y") (Var "z")))
-- (Skip,[("x",0),("y",3),("z",3)]) -- exSigma2

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

-- (Skip,[("x",1),("y",6),("z",2)]) exSigma2
-- (Skip,[("x",1),("temp",0),("y",3628800)]) exSigma

