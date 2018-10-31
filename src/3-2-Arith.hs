module Arith where

{- Implementation of 3-2: Arithmetic expressions (p. 41). -}

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
-- E-IfTrue
eval' (IfTerm TrueTerm t _) = Just t
-- E-IfFalse
eval' (IfTerm FalseTerm _ t) = Just t
-- E-If
eval' (IfTerm t1 t2 t3) = Just (IfTerm (eval t1) t2 t3)
-- E-Succ
eval' (SuccTerm t) = eval' t
-- E-PredZero
eval' (PredTerm ZeroTerm) = Just ZeroTerm
-- E-PredSucc
eval' (PredTerm (SuccTerm nv)) = Just nv
-- E-Pred
eval' (PredTerm t) = eval' t
-- E-IsZeroZero
eval' (IsZeroTerm ZeroTerm) = Just TrueTerm
-- E-IsZeroSucc
eval' (IsZeroTerm (SuccTerm nv)) = Just FalseTerm
-- E-IsZero
eval' (IsZeroTerm t) = Just (IsZeroTerm (eval t))
-- (Nothing matches.)
eval' _ = Nothing

eval :: Term -> Term
eval t = case (eval' t) of
            (Just t') -> eval t'
            Nothing -> t
