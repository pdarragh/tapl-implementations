module Bools where

{- Implementation of Figure 3-1: Booleans (p. 34). -}

data Term
    = TrueTerm
    | FalseTerm
    | IfTerm Term Term Term
    deriving (Show)

eval' :: Term -> Maybe Term
-- E-IfTrue
eval' (IfTerm TrueTerm t _) = Just t
-- E-IfFalse
eval' (IfTerm FalseTerm _ t) = Just t
-- E-If
eval' (IfTerm t1 t2 t3) = Just (IfTerm (eval t1) t2 t3)
-- (Nothing matches.)
eval' _ = Nothing

eval :: Term -> Term
eval t = case (eval' t) of
            (Just t') -> eval t'
            Nothing -> t
