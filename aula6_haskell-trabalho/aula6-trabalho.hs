
data E = Num Int | Soma E E | Mult E E
    deriving(Show, Eq)

proql :: E
proql = Soma ( Num 1 ) ( Mult ( Num 2 ) ( Num 3 ) )

proql2 :: E
proql2 = Soma ( Mult ( Num 2 ) (Num 3) ) ( Mult ( Num 4 ) ( Num 5 ) )

bigStepE :: E -> Int 

bigStepE (Num n) = n
bigStepE (Soma e1 e2) = bigStepE e1 + bigStepE e2
bigStepE (Mult e1 e2) = bigStepE e1 * bigStepE e2

