
data Nat = Zero | Suc Nat
    deriving(Eq, Show)

soma :: Nat -> Nat -> Nat
soma Zero n = n
soma (Suc n1) n2 = soma n1 (Suc n2) 

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Suc n1) n2 = soma n2 ( mult n1 n2 )

loq :: Nat -> Nat -> Bool -- n1 <= n2
loq Zero n = True
loq n Zero = False
loq (Suc n1) (Suc n2) = loq n1 n2 

to_int :: Nat -> Int
to_int Zero = 0
to_int (Suc n1) = 1 + to_int n1

to_nat :: Int -> Nat
to_nat 0 = Zero
to_nat n = Suc ( to_nat ( n - 1 ) )

sub :: Nat -> Nat -> Nat -- n1 - n2
sub n1 Zero = n1
sub (Suc n1) (Suc n2) =  sub n1 n2

divi :: Nat -> Nat -> Nat -- n1 / n2
divi Zero n2 = Zero
divi n1 Zero = n1
divi n1 (Suc n2)
    | loq (Suc (Suc n2) ) Zero = n1
    | otherwise = divi ( sub n1 (Suc n2) ) n2

resto_divi :: Nat -> Nat -> Nat
resto_divi n Zero = undefined
resto_divi Zero n = n

goq :: Nat -> Nat -> Bool -- n1 >= n2
goq Zero n = False
goq n Zero = True
goq (Suc n1) (Suc n2) = goq n1 n2

mult_2 :: Int -> Int -> Nat
mult_2 n1 n2 = to_nat ( n1 * n2 )
sub_2 :: Int -> Int -> Nat
sub_2 n1 n2 = to_nat ( n1 - n2 )
divi_2 :: Int -> Int -> Nat
divi_2 n1 n2 = to_nat ( div n1 n2 )

mult_3 :: Nat -> Nat -> Int
mult_3 n1 n2 = to_int ( mult n1 n2 )
sub_3 :: Nat -> Nat -> Int
sub_3 n1 n2 = to_int ( sub n1 n2 )
divi_3 :: Nat -> Nat -> Int
divi_3 n1 n2 = to_int ( divi n1 n2 )

n1 = Suc (Suc (Suc ( Zero )) ) -- 3
n2 = Suc (Suc Zero)            -- 2
n3 = Suc (Zero)                -- 1

