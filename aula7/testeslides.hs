
type Memoria = [(String,Int)]

data E = Num Int | Var String | Soma E E | Mult E E
    deriving(Eq, Show)

smallStepE (Soma (Num n1) (Num n2), s) = (Num (n1 + n2), s)

smallStepE (Soma (Num n) e, s) =
        let (el,sl) = smallStepE (e,s)
        in (Soma (Num n) el, sl)

smallStepE (Soma e1 e2,s) =
    let (el,sl) = smallStepE (e1,s)
    in (Soma el e2,sl)

smallStepE (Mult (Num n1) (Num n2), s) = (Num (n1 * n2), s)

smallStepE (Mult (Num n) e, s) =
    let (el,sl) = smallStepE (e,s)
    in (Mult (Num n) el, sl)

smallStepE (Mult e1 e2,s) =
        let (el,sl) = smallStepE (e1,s)
        in (Mult el e2,sl)

isFinal :: E -> Bool
isFinal (Num n) = True
isFinal _ = False

interpretador :: (E,Memoria) -> (E, Memoria)
interpretador (e,s) = if (isFinal e)
    then (e,s)
    else interpretador (smallStepE (e,s))

