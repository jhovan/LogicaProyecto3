module Syntax where

import Data.List

-- Tipo que representa el indice de las variables
type Ind = Int

-- Tipo que representa nombres de funciones y predicados
type Nombre = String

-- Tipo de dato que representa un termino
-- Variables con su indice, y Funciones con su nombre y lista de terminos
data Term = V Ind | F Nombre [Term]

-- Tipo de dato que representa literales
data Lit = TrueF
         | FalseF
         | Pr Nombre [Term]
         | Eq Term Term 


-- Define el comportamiento del metodo show para terminos
instance Show Term where
    show t = case t of
        V x -> show x
        F nom [] -> show nom 
        F nom lt -> show nom ++ show lt

-- Define el comportamiento del motodo show para formulas
instance Show Lit where
    show phi = case phi of
        FalseF -> "F"
        TrueF -> "T"
        Pr p lt -> show p ++ show lt
        Eq t1 t2 -> "(" ++ show t1 ++ "=" ++ show t2 ++ ")"

-- Define el comportamiento del metodo Eq (==) entre terminos
instance Eq Term where
    (==) t1 t2 = case t1 of
        V x -> case t2 of
            V y -> if (x == y)
                   then True
                   else False
            _ -> False
        F nom1 lt1 -> case t2 of
            F nom2 lt2 -> if (nom1 == nom2 && lt1 == lt2)
                          then True
                          else False
            _ -> False

-- Define el comportamiento del metodo Eq (==) entre literales
instance Eq Lit where
    (==) l1 l2 = case (l1,l2) of
            (FalseF,FalseF) -> True
            (TrueF,TrueF) -> True
            (Pr p lt1, Pr q lt2) -> if  p == q && length lt1 == length lt2 
                then and [t1 == t2 | (t1,t2) <- (zip lt1 lt2)]
                else False
            (Eq t1 s1, Eq t2 s2) ->  t1 == t2 && s1 == s2
            _ -> False
        

-- Recibe un termino y devuelve una lista con los indices de sus variables
varT :: Term -> [Ind]
varT t = case t of
    V x -> [x]
    F _ l -> nub(concat[varT t | t <- l])


-- Tipo que define una substitucion (el indice de la variable a sustituir por el termino que la remplazara)
type Subst = [(Ind,Term)] 

-- Recibe una substitucion y verifica si es valida o no
-- Es decir que una variable solo sea substituida una vez en la substitucion
verifSus :: Subst -> Bool
verifSus s = tieneRep [v |(v,t) <- s] 

-- Recibe un termino y una substitucion
-- Devuelve un nuevo termino con la substitucion aplicada
apsubT :: Term -> Subst -> Term
apsubT t sus = case t of
    V x -> case sus of 
        [] -> V x
        (v,t2):xs -> if x == v
                     then t2
                     else apsubT (V x) xs
    F f lt -> F f [apsubT t sus | t <- lt]


-- Recibe una literal y una substitucion
-- Devuelve una nueva liteal con la substitucion aplicada
apsubL :: Lit -> Subst -> Lit
apsubL phi sus = case phi of
    TrueF -> TrueF
    FalseF -> FalseF
    Pr p lt -> Pr p [apsubT t sus | t <- lt]
    Eq t1 t2 -> Eq (apsubT t1 sus) (apsubT t2 sus)

-- Recibe una lista de objetos comparables, y regresa verdadero si hay repeticiones
tieneRep :: (Eq a) => [a] -> Bool
tieneRep l = case l of
    [] -> False
    x:xs -> if(elem x xs)
            then True
            else tieneRep xs
