module TyArith where

{- Implementation of 8-2: Typed arithmetic expressions (p. 93). -}

-- This definition allows monadic small-step evaluation without headaches.
(<#>) :: Applicative f => f (a -> b) -> a -> f b
(<#>) func x = func <*> pure x
infixl 4 <#>

data Type
    = NatType
    | BoolType
    deriving (Show, Eq)

data Term
    = TrueTerm
    | FalseTerm
    | IfTerm Term Term Term
    | ZeroTerm
    | SuccTerm Term
    | PredTerm Term
    | IsZeroTerm Term
    deriving (Show, Eq)

typeOf :: Term -> Maybe Type
typeOf t =
    case t of
        TrueTerm        -> return BoolType
        FalseTerm       -> return BoolType
        IfTerm t1 t2 t3 -> case (typeOf t1) of
                            Just ty1    -> ty23
                                            where
                                                ty2 = typeOf t2
                                                ty3 = typeOf t3
                                                ty23 = if ty2 == ty3 then ty2 else Nothing
                            Nothing     -> Nothing
        ZeroTerm        -> return NatType
        SuccTerm t'     -> case (typeOf t') of
                            Just NatType    -> return NatType
                            _               -> Nothing
        PredTerm t'     -> case (typeOf t') of
                            Just NatType    -> return NatType
                            _               -> Nothing
        IsZeroTerm t'   -> case (typeOf t') of
                            Just NatType    -> return BoolType
                            _               -> Nothing
