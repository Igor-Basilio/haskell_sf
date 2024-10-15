
-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E
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

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]


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

smallStepE :: (E, Memoria) -> (E, Memoria)
smallStepE (Var x, s)                  = (Num (procuraVar s x), s)
smallStepE (Soma (Num n1) (Num n2), s) = (Num (n1 + n2), s)
smallStepE (Soma (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Soma (Num n) el, sl)
smallStepE (Soma e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Soma el e2,sl)
smallStepE (Mult (Num n1) (Num n2), s) = (Num (n1 * n2), s)
smallStepE (Mult (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Mult (Num n) el, sl)
smallStepE (Mult e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Mult el e2, sl)

smallStepE (Sub (Num n1) (Num n2), s)   =   (Num (n1 - n2), s) 
smallStepE (Sub (Num n) e, s)           =   let (el,s1) = smallStepE (e, s)
                                            in (Sub (Num n) el, s1)

smallStepE (Sub e1 e2, s)           =   let (el,s1) = smallStepE (e1, s)
                                        in (Sub el e2, s1)

-- Mudei TRUE | FALSE para -> Main.True | Main.False
-- por causa do retorno de operações do tipo menor ou igual
-- usando Main explicitamente é possível ver a diferença 
-- da definição em Prelude.
smallStepB :: (B,Memoria) -> (B, Memoria)

mapPreludeToMain :: Prelude.Bool -> B
mapPreludeToMain b  
    | b == False = FALSE
    | otherwise = TRUE

mapMainToPrelude :: B -> Prelude.Bool
mapMainToPrelude b
    | b == FALSE = Prelude.False
    | otherwise = Prelude.True

smallStepB (Not TRUE, s)  = (FALSE, s)
smallStepB (Not FALSE, s) = (TRUE, s)
smallStepB (Not b, s)      = let (b1, s1) = smallStepB(b, s)
                             in  (Not b1, s1)

smallStepB (And FALSE b2, s ) = (FALSE , s)
smallStepB (And TRUE b2, s )  = (b2, s)
smallStepB (And b1 b2, s )    = let (bl, s1) = smallStepB( b1, s )
                                in  ( And bl b2, s1)

smallStepB (Or TRUE b2, s )  = (TRUE, s)
smallStepB (Or FALSE b2,s )  = (b2, s)
smallStepB (Or b1 b2, s )    = let (bl, s1) = smallStepB( b1, s )
                               in  ( Or bl b2, s1)

smallStepB (Leq (Num n1) (Num n2), s) = (mapPreludeToMain (n1 <= n2), s)

smallStepB (Leq (Num n) e2, s) = let (el, s1) = smallStepE( e2, s )
                                 in  ( Leq (Num n) el, s1 )

smallStepB (Leq e1 e2, s) = let (el, s1) = smallStepE( e1, s )
                            in  ( Leq el e2, s1 )

-- recebe duas expressões aritméticas e devolve um valor booleano dizendo se são iguais
smallStepB (Igual (Num n1) (Num n2), s) = (mapPreludeToMain(n1 == n2), s)

smallStepB (Igual (Num n) e2, s) = let (el, s1) = smallStepE( e2, s )
                                   in ( Igual (Num n) el, s )

smallStepB (Igual e1 e2, s) =  let (el, s1) = smallStepE( e1, s)
                               in ( Igual el e2, s1 )

smallStepC :: (C,Memoria) -> (C,Memoria)

smallStepC (If TRUE  c1 c2,s) = (c1, s)
smallStepC (If FALSE c1 c2,s) = (c2, s)

smallStepC (If b c1 c2,s) = let (bl, s1) = smallStepB( b, s )
                            in ( If bl c1 c2, s1 )

smallStepC (Seq Skip c2,s) = ( c2, s )
smallStepC (Seq c1 c2,s) = let (cl, s1) = smallStepC( c1, s )
                           in ( Seq cl c2, s1 )
                           
smallStepC (Atrib (Var x) (Num n), s) = (Skip, mudaVar s x n)
smallStepC (Atrib (Var x) e, s) = let (el, s1) = smallStepE( e, s )
                                  in ( Atrib (Var x) el, s1 )

smallStepC (While b c, s)  = ( If b (Seq c (While b c)) Skip, s )

-- Check if valid functions under here v : v
smallStepC (DoWhile c b, s) =  let (cl, s1) = smallStepC( c, s )
                               in ( While b c, s1)

smallStepC( Unless FALSE c1 c2, s ) = ( c1, s )
smallStepC( Unless TRUE  c1 c2, s ) = ( c2, s )
smallStepC( Unless b c1 c2, s ) = let (bl, s1) = smallStepB(b, s)
                                  in  ( Unless bl c1 c2, s1 )

smallStepC( Loop (Num n) c, s )   
    | (0 < n)   =   ( Seq c ( Loop (Sub (Num n) (Num 1)) c), s )
    | otherwise =   ( Skip, s )

smallStepC( Loop e c, s ) = let( e', s' ) = smallStepE( e, s ) 
                            in ( Loop e' c, s' )

smallStepC( Swap (Var x) (Var y), s ) = ( Skip,
    ( mudaVar ( mudaVar s x (procuraVar s y ) ) y ( procuraVar s x ) ) )

smallStepC( DAttrib (Var x) (Var y) (Num n1) (Num n2), s ) = 
    (Skip, mudaVar ( mudaVar s x n1 ) y n2 )            

smallStepC (DAttrib (Var x) (Var y) (Num n1) e2, s) =
                                  let ( el2, s2 ) = smallStepE( e2, s )
                                  in ( DAttrib (Var x) (Var y) (Num n1) el2, s2 )

smallStepC (DAttrib (Var x) (Var y) e1 e2, s) = let ( el, s1 ) = smallStepE( e1, s )
                                              in ( DAttrib (Var x) (Var y) el e2, s1 )

----------------------
--  INTERPRETADORES
----------------------


--- Interpretador para Expressões Aritméticas:
isFinalE :: E -> Bool
isFinalE (Num n)  = True
isFinalE  _       = False

interpretadorE :: (E,Memoria) -> (E, Memoria)
interpretadorE (e,s) = if (isFinalE e) then (e,s) else interpretadorE (smallStepE (e,s))

--- Interpretador para expressões booleanas


isFinalB :: B -> Bool
isFinalB TRUE    = True
isFinalB FALSE   = True
isFinalB _       = False

-- Descomentar quanto a função smallStepB estiver implementada:

interpretadorB :: (B,Memoria) -> (B, Memoria)
interpretadorB (b,s) = if (isFinalB b) then (b,s) else interpretadorB (smallStepB (b,s))

-- Interpretador da Linguagem Imperativa

isFinalC :: C -> Bool
isFinalC Skip    = True
isFinalC _       = False

-- Descomentar quando a função smallStepC estiver implementada:

interpretadorC :: (C,Memoria) -> (C, Memoria)
interpretadorC (c,s) = if (isFinalC c) then (c,s) else interpretadorC (smallStepC (c,s))

--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS *** DIFERENTES *** DE PROGRAMAS QUE USEM:
--  * Unless  
--  * Loop   
--  * Swap 
--  * DAtrrib 

exSigma2 :: Memoria
exSigma2 = [("x", 3), ("y",0), ("z",0)]

-- Minimo de três valores ( "a" terá o minimo )
exSigma6 :: Memoria
exSigma6 = [ ("a", 20302042), ("b", 283182), ("c", 9293929392) ]

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

-- Fibonnaci
exSigma5 :: Memoria
exSigma5 = [ ("x", 0), ("y", 1), ("times", 10), ("fib", 0) ]

fibonnaci :: C
fibonnaci = Seq ( DAttrib (Var "x") (Var "y") (Num 0) (Num 1) )
                ( Seq (Atrib (Var "fib") (Num 0)) (Loop (Sub (Var "times") (Num 1))
                ( Seq (Atrib (Var "fib") (Soma (Var "x") (Var "y")))
                ( (DAttrib (Var "x") (Var "y") (Var "y") (Var "fib")) )) ) )

---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação que fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- A função smallStepE anda apenas um passo na avaliação da Expressão

-- *Main> smallStepE (progExp1, exSigma)
-- (Soma (Num 3) (Soma (Num 10) (Var "y")),[("x",10),("temp",0),("y",0)])

-- Note que no exemplo anterior, o (Var "x") foi substituido por (Num 10)

-- Para avaliar a expressão até o final, deve-se usar o interpretadorE:

-- *Main> interpretadorE (progExp1 , exSigma)
-- (Num 13,[("x",10),("temp",0),("y",0)])

-- *Main> interpretadorE (progExp1 , exSigma2)
-- (Num 6,[("x",3),("y",0),("z",0)])


--- Para rodar os próximos programas é necessário primeiro implementar as regras que faltam
--- e descomentar os respectivos interpretadores


---
--- Exemplos de expressões booleanas:


teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 5))  (Mult (Num 2) (Num 3)))

---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

