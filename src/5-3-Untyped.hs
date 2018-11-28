module Untyped where

{-
    Implementation of Figure 5-3: Untyped lambda calculus (p. 72)
    The implementation is based on the description from chapter 7.
-}

-- This definition allows monadic small-step evaluation without headaches.
(<#>) :: Applicative f => f (a -> b) -> a -> f b
(<#>) func x = func <*> pure x
infixl 4 <#>

data Term
    = VarTerm Int
    | AbsTerm String Term
    | AppTerm Term Term
    deriving (Show)

data Binding = NameBind

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

termMap :: (Int -> Int -> Term) -> Int -> Term -> Term
termMap varFunc d t = walk 0 t
    where walk c t = case t of
                        (VarTerm i)     -> varFunc i c
                        (AbsTerm x t')  -> AbsTerm x (walk (c + 1) t')
                        (AppTerm t1 t2) -> AppTerm (walk c t1) (walk c t2)


termShift :: Int -> Term -> Term
termShift d = termMap (\i c -> if i >= c then (VarTerm (i + d)) else (VarTerm i)) 0

termSubst :: Int -> Term -> Term -> Term
termSubst j t' = termMap (\i c -> if i == j + c then (termShift c t') else (VarTerm i)) 0

termSubstTop :: Term -> Term -> Term
termSubstTop t' t = termShift (-1) (termSubst 0 (termShift 1 t') t)

isVal :: Context -> Term -> Bool
isVal _ (AbsTerm _ _) = True
isVal _ _ = False

eval' :: Context -> Term -> Maybe Term
eval' ctx t =
    case t of
        AppTerm (AbsTerm x t12) v2
            | isVal ctx v2  -> return $ termSubstTop v2 t12
        AppTerm v1 t2
            | isVal ctx v1  -> AppTerm v1 <$> eval' ctx t2
        AppTerm t1 t2       -> AppTerm <$> eval' ctx t1 <#> t2
        _                   -> Nothing


eval :: Context -> Term -> Term
eval ctx t = case (eval' ctx t) of
            (Just t') -> eval ctx t'
            Nothing -> t
