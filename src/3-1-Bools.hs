module Bools where

{- Implementation of Figure 3-1: Booleans (p. 34). -}

-- This definition allows monadic small-step evaluation without headaches.
(<#>) :: Applicative f => f (a -> b) -> a -> f b
(<#>) func x = func <*> pure x
infixl 4 <#>

data Term
    = TrueTerm
    | FalseTerm
    | IfTerm Term Term Term
    deriving (Show)

eval' :: Term -> Maybe Term
eval' t =
    case t of
        IfTerm TrueTerm  t  _  -> return t
        IfTerm FalseTerm _  t  -> return t
        IfTerm t1        t2 t3 -> IfTerm <$> eval' t1 <#> t2 <#> t3
        _ -> Nothing

eval :: Term -> Term
eval t = case (eval' t) of
            (Just t') -> eval t'
            Nothing -> t
