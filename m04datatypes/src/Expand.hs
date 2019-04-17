module Expand where

infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

-- expand :: Expr -> Expr
-- expand ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
-- expand (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
-- expand (e1 :+: e2) = expand e1 :+: expand e2
-- expand (e1 :*: e2) = expand e1 :*: expand e2
-- expand e = e  


expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand ( e1 :*: e ) :+: expand ( e2 :*: e )
expand (e :*: (e1 :+: e2)) = expand ( e :*: e1 ) :+: expand ( e :*: e2 )
expand (e1 :+: e2) = expand e1 :+: expand e2
expand e@(e1 :*: e2) = 
    if containsPlus e then expand ( expand e1 :*: expand e2 ) else (e1 :*: e2)
    where
        containsPlus (e1 :*: e2) = (containsPlus e1) || (containsPlus e2)
        containsPlus (e1 :+: e2) = True
        containsPlus (Val _)    =  False
expand e = e  
