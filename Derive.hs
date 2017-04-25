module Diff where

data Polynomial = Add | Sub | Mul | Div | Pow

data Fomula a = Opr Polynomial (Fomula a) (Fomula a) | X | Otr Double

instance Show Polynomial where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Pow = "^"

class Derive a where
    derive :: a -> a

instance Show (Fomula a) where
    show (Opr f a b) = "(" ++ show a ++ ' ' : show f ++ ' ' : show b ++ ")"
    show (Otr x)     = show x
    show X           = "x"

instance Derive (Fomula a) where
    derive (Otr x) = Otr 0
    derive (Opr Add X _) = Otr 1
    derive (Opr Sub X _) = Otr 1
    derive (Opr Add _ X) = Otr 1
    derive (Opr Sub _ X) = Otr 1
    derive (Opr Mul X f) = f
    derive (Opr Mul f X) = f
    derive (Opr Div X f) = Opr Div (Otr 1) f
    derive (Opr Pow X f) = Opr Mul f (Opr Pow X (Opr Sub f (Otr 1)))
    derive (Opr Add f g) = Opr Add (derive f) (derive g)
    derive (Opr Sub f g) = Opr Sub (derive f) (derive g)
    derive (Opr Mul f g) = Opr Add (Opr Mul (derive f) g) (Opr Mul f (derive g))
    derive (Opr Div f g) = Opr Div (Opr Sub (Opr Mul f g) (Opr Mul f (derive g))) (Opr Pow g (Otr 2))
    derive (Opr Pow f g) = Opr Mul (Opr Mul g (Opr Pow f (Opr Sub g (Otr 1)))) (derive f)

instance Num (Fomula a) where
    (+) x y     = Opr Add x y
    (-) x y     = Opr Sub x y
    (*) x y     = Opr Mul x y
    negate      = error "ERROR!"
    abs         = error "ERROR!"
    signum      = error "ERROR!"
    fromInteger = error "ERROR!"

instance Fractional (Fomula a) where
    (/) x y      = Opr Div x y
    recip        = error "ERROR!"
    fromRational = error "ERROR!"

instance Floating (Fomula a) where
    (**) x y = Opr Pow x y
    pi       = error "ERROR!"
    exp      = error "ERROR!"
    log      = error "ERROR!"
    sin      = error "ERROR!"
    cos      = error "ERROR!"
    asin     = error "ERROR!"
    acos     = error "ERROR!"
    atan     = error "ERROR!"
    sinh     = error "ERROR!"
    cosh     = error "ERROR!"
    asinh    = error "ERROR!"
    acosh    = error "ERROR!"
    atanh    = error "ERROR!"

optimization x@(Otr _) = x
optimization X = X
optimization (Opr Add (Otr x) (Otr y)) = (Otr (x + y))
optimization (Opr Sub (Otr x) (Otr y)) = (Otr (x - y))
optimization (Opr Mul (Otr x) (Otr y)) = (Otr (x * y))
optimization (Opr Div (Otr x) (Otr y)) = (Otr (x / y))
optimization (Opr Pow (Otr x) (Otr y)) = (Otr (x ** y))
optimization (Opr Add (Otr 0) x) = x
optimization (Opr Sub (Otr 0) x) = x
optimization (Opr Add x (Otr 0)) = x
optimization (Opr Sub x (Otr 0)) = x
optimization (Opr Mul (Otr 0) _) = Otr 0
optimization (Opr Div (Otr 0) _) = Otr 0
optimization (Opr Mul (Otr x) (Opr Mul (Otr y) f)) = Opr Mul (Otr (x * y)) (optimization f)
optimization (Opr Mul (Otr x) (Opr Mul f (Otr y))) = Opr Mul (Otr (x * y)) (optimization f)
optimization (Opr Mul (Opr Mul (Otr y) f) (Otr x)) = Opr Mul (Otr (x * y)) (optimization f)
optimization (Opr Mul (Opr Mul f (Otr y)) (Otr x)) = Opr Mul (Otr (x * y)) (optimization f)
optimization (Opr f x y) = Opr f (optimization x) (optimization y)

execute fomula var = exec fomula
    where
        exec X = var
        exec (Otr x) = x
        exec (Opr Add f g) = exec f + exec g
        exec (Opr Sub f g) = exec f - exec g
        exec (Opr Mul f g) = exec f * exec g
        exec (Opr Div f g) = exec f / exec g
        exec (Opr Pow f g) = exec f ** exec g