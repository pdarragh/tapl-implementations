module FullUntyped where

{-
    Implementation of the full untyped lambda calculus, which is given in
    chapter 6. The implementation is based on the description from chapter 7.

    Note that this implementation does not include support for records,
    projections, floats, strings, or let-binding. These features are not
    covered until later in the book, so I left them out.
-}

-- This definition allows monadic small-step evaluation without headaches.
(<#>) :: Applicative f => f (a -> b) -> a -> f b
(<#>) func x = func <*> pure x
infixl 4 <#>

data Term
    = TrueTerm
    | FalseTerm
    | IfTerm Term Term Term
    | VarTerm Int Int
    | AbsTerm String Term
    | AppTerm Term Term
    | ZeroTerm
    | SuccTerm Term
    | PredTerm Term
    | IsZeroTerm Term
    deriving (Show)

data Binding
    = NameBind
    | AppTermBind Term

type Name = String
type Context = [(Name, Binding)]

emptyContext :: Context
emptyContext = []

isNameBound :: Context -> Name -> Bool
isNameBound [] _ = False
isNameBound ((y, _):ys) x = if y == x then True else isNameBound ys x

pickFreshName :: Context -> Name -> Context
pickFreshName ctx x = if isNameBound ctx x
                        then pickFreshName ctx (x ++ "'")
                        else (x, NameBind) : ctx

indexToName :: Context -> Int -> Name
indexToName ctx i = fst (ctx !! i)

nameToIndex :: Context -> Name -> Int
nameToIndex [] x = error $ "Identifier " ++ x ++ " is unbound."
nameToIndex ((y, _):ys) x = if y == x then 0 else 1 + (nameToIndex ys x)

termMap :: (Int -> Int -> Int -> Term) -> Int -> Term -> Term
termMap varFunc d t = walk 0 t
    where walk c t
            = case t of
                TrueTerm        -> t
                FalseTerm       -> t
                IfTerm t1 t2 t3 -> IfTerm (walk c t1) (walk c t2) (walk c t3)
                VarTerm i n     -> varFunc i n c
                AbsTerm x t'    -> AbsTerm x (walk (c + 1) t')
                AppTerm t1 t2   -> AppTerm (walk c t1) (walk c t2)
                ZeroTerm        -> t
                SuccTerm t'     -> SuccTerm (walk c t')
                PredTerm t'     -> PredTerm (walk c t')
                IsZeroTerm t'   -> IsZeroTerm (walk c t')

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d = termMap (\i n c -> if i >= c then (VarTerm (i + d) (n + d)) else (VarTerm i (n + d)))

termShift :: Int -> Term -> Term
termShift d t = termShiftAbove d 0 t

bindingShift :: Binding -> Int -> Binding
bindingShift NameBind _ = NameBind
bindingShift (AppTermBind t) d = AppTermBind (termShift d t)

termSubst :: Int -> Term -> Term -> Term
termSubst j t' t = termMap (\i n c -> if i == j + c then (termShift c t') else (VarTerm i n)) 0 t

termSubstTop :: Term -> Term -> Term
termSubstTop t' t = termShift (-1) (termSubst 0 (termShift 1 t') t)

getBinding :: Context -> Int -> Binding
getBinding ctx i = snd (ctx !! i)

isNumericalVal :: Context -> Term -> Bool
isNumericalVal ctx t =
    case t of
        ZeroTerm    -> True
        SuccTerm t' -> isNumericalVal ctx t'
        _           -> False

isVal :: Context -> Term -> Bool
isVal ctx t =
    case t of
        TrueTerm    -> True
        FalseTerm   -> True
        _           -> isNumericalVal ctx t

eval' :: Context -> Term -> Maybe Term
eval' ctx t =
    case t of
        IfTerm TrueTerm t2 _        -> return t2
        IfTerm FalseTerm _ t3       -> return t3
        IfTerm t1 t2 t3             -> IfTerm <$> eval' ctx t1 <#> t2 <#> t3
        VarTerm i _                 -> case (getBinding ctx i) of
                                        AppTermBind t'  -> return t'
                                        _               -> Nothing
        AppTerm (AbsTerm x t12) v2
            | isVal ctx v2          -> return $ termSubstTop v2 t12
        AppTerm v1 t2
            | isVal ctx v1          -> AppTerm v1 <$> eval' ctx t2
        AppTerm t1 t2               -> AppTerm <$> eval' ctx t1 <#> t2
        SuccTerm t'                 -> SuccTerm <$> eval' ctx t'
        PredTerm ZeroTerm           -> return ZeroTerm
        PredTerm (SuccTerm nv)
            | isNumericalVal ctx nv -> return nv
        PredTerm t'                 -> PredTerm <$> eval' ctx t'
        IsZeroTerm ZeroTerm         -> return TrueTerm
        IsZeroTerm (SuccTerm nv)    -> return FalseTerm
        IsZeroTerm t'               -> IsZeroTerm <$> eval' ctx t'
        _                           -> Nothing

eval :: Context -> Term -> Term
eval ctx t =
    case (eval' ctx t) of
        Just t' -> eval ctx t'
        Nothing -> t

startEval :: Term -> Term
startEval = eval emptyContext

evalBinding :: Context -> Binding -> Binding
evalBinding ctx b =
    case b of
        AppTermBind t   -> AppTermBind (eval ctx t)
        _               -> b
