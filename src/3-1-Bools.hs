module Bools where

{- Implementation of Figure 3-1: Booleans (p. 34). -}

data Term
    = TrueTerm
    | FalseTerm
    | IfTerm Term Term Term
    deriving (Show)

eval' :: Term -> Maybe Term
eval' (IfTerm TrueTerm t _) = Just t
eval' (IfTerm FalseTerm _ f) = Just f
eval' (IfTerm c t f) = Just (IfTerm (eval c) t f)
eval' _ = Nothing

eval :: Term -> Term
eval t = case (eval' t) of
            (Just t') -> eval t'
            Nothing -> t
