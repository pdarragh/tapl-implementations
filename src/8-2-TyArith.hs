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

checkType :: Term -> Type -> Maybe ()
checkType t compTy =
    case (typeOf t) of
        Just tTy
            | tTy == compTy -> return ()
        _                   -> Nothing

compTypes :: Term -> Term -> Maybe Type
compTypes t1 t2 =
    let ty1 = typeOf t1
        ty2 = typeOf t2
    in case ((==) <$> ty1 <*> ty2) of
        Just True   -> ty1
        _           -> Nothing

typeOf :: Term -> Maybe Type
typeOf t =
    case t of
        TrueTerm        -> return BoolType
        FalseTerm       -> return BoolType
        IfTerm t1 t2 t3 -> checkType t1 BoolType *> compTypes t2 t3
        ZeroTerm        -> return NatType
        SuccTerm t'     -> checkType t' NatType *> return NatType
        PredTerm t'     -> checkType t' NatType *> return NatType
        IsZeroTerm t'   -> checkType t' NatType *> return BoolType

isNumericVal :: Term -> Bool
isNumericVal t =
    case t of
        ZeroTerm    -> True
        SuccTerm t' -> isNumericVal t'
        PredTerm t' -> isNumericVal t'
        _           -> False

isVal :: Term -> Bool
isVal t =
    case t of
        TrueTerm                -> True
        FalseTerm               -> True
        t' | isNumericVal t'    -> True
        _                       -> False

eval' :: Term -> Maybe Term
eval' t =
    case t of
        IfTerm TrueTerm t2 _        -> return t2
        IfTerm FalseTerm _ t3       -> return t3
        IfTerm t1 t2 t3             -> IfTerm <$> eval' t1 <#> t2 <#> t3
        SuccTerm t'                 -> SuccTerm <$> eval' t'
        PredTerm ZeroTerm           -> return ZeroTerm
        PredTerm (SuccTerm nv)
            | isNumericVal nv       -> return nv
        PredTerm t'                 -> PredTerm <$> eval' t'
        IsZeroTerm ZeroTerm         -> return TrueTerm
        IsZeroTerm (SuccTerm nv)
            | isNumericVal nv       -> return FalseTerm
        IsZeroTerm t'               -> IsZeroTerm <$> eval' t'
        _                           -> Nothing

eval :: Term -> Term
eval t =
    case (eval' t) of
        Just t' -> eval t'
        Nothing -> t
