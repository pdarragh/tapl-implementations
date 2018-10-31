module Arith where

{- Implementation of 3-2: Arithmetic expressions (p. 41). -}

-- This definition allows monadic small-step evaluation without headaches.
(<#>) :: Applicative f => f (a -> b) -> a -> f b
(<#>) func x = func <*> pure x
infixl 4 <#>

data Term
    = TrueTerm
    | FalseTerm
    | IfTerm Term Term Term
    | ZeroTerm
    | SuccTerm Term
    | PredTerm Term
    | IsZeroTerm Term
    deriving (Show)

eval' :: Term -> Maybe Term
eval' t =
    case t of
        IfTerm TrueTerm  t  _       -> return t
        IfTerm FalseTerm _  t       -> return t
        IfTerm t1        t2 t3      -> IfTerm <$> eval' t1 <#> t2 <#> t3
        SuccTerm t                  -> eval' t
        PredTerm ZeroTerm           -> return ZeroTerm
        PredTerm (SuccTerm nv)      -> return nv
        PredTerm t                  -> eval' t
        IsZeroTerm ZeroTerm         -> return TrueTerm
        IsZeroTerm (SuccTerm nv)    -> return FalseTerm
        IsZeroTerm t                -> IsZeroTerm <$> eval' t
        _                           -> Nothing

eval :: Term -> Term
eval t = case (eval' t) of
            (Just t') -> eval t'
            Nothing -> t
