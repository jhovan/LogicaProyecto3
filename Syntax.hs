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
         | Eq Term Term deriving (Show, Eq)

-- Tipo de dato que representa una formula de primer orden
-- Constantes, predicados (con su nombre y lista de terminos), operadores unarios, binarios
-- Y cuantificadores existencial y universal con el indice de su variable lugada y su formula
data Form = TrueF | FalseF | Pr Nombre [Term] | Eq Term Term | Neg Form
            | Conj Form Form | Disy Form Form | Imp Form Form | Equiv Form Form
            | All Ind Form | Ex Ind Form

-- Define el comportamiento del metodo show para terminos
instance Show Term where
    show t = case t of
        V x -> show x
        F nom [] -> show nom 
        F nom lt -> show nom ++ show lt

-- Define el comportamiento del motodo show para formulas
instance Show Form where
    show phi = case phi of
        FalseF -> "F"
        TrueF -> "T"
        Pr p lt -> show p ++ show lt
        Eq t1 t2 -> "(" ++ show t1 ++ "=" ++ show t2 ++ ")"
        Neg p -> "(~" ++ show p ++ ")"
        Conj p q -> "(" ++ show p ++ " ^ " ++ show q ++ ")"
        Disy p q -> "(" ++ show p ++ " v " ++ show q ++ ")"
        Imp p q -> "(" ++ show p ++ " -> " ++ show q ++ ")"
        Equiv p q -> "(" ++ show p ++ " <-> " ++ show q ++ ")"
        All x p -> "All " ++ show x ++ " (" ++ show p ++ ")"
        Ex x p -> "Exist " ++ show x ++ " (" ++ show p ++ ")"

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

-- Recibe un termino y devuelve una lista con los indices de sus variables
varT :: Term -> [Ind]
varT t = case t of
    V x -> [x]
    F _ l -> nub(concat[varT t | t <- l])

-- Recibe una formula y devuelve una lista con los indices de sus variables libres
fv :: Form -> [Ind]
fv p = case p of
    TrueF -> []
    FalseF -> []
    Pr _ l -> concat[varT t | t <- l]
    Eq t1 t2 -> (varT t1)  `union` (varT t2)
    Neg p ->  (fv p)
    Conj p q ->  (fv p) `union`  (fv q)
    Disy p q ->  (fv p) `union`  (fv q)
    Imp p q ->  (fv p) `union`  (fv q)
    Equiv p q ->  (fv p) `union`  (fv q)
    All x p -> (fv p) \\ [x]
    Ex x p -> (fv p) \\ [x]

-- Recibe una formula y devuelve una lista con los indices de sus variables ligadas
bv :: Form -> [Ind]
bv p = case p of
     TrueF -> []
     FalseF -> []
     Pr _ _ -> []
     Eq _ _ -> []
     Neg p ->  bv p
     Conj p q ->  (bv p) `union`  (bv q)
     Disy p q ->  (bv p) `union`  (bv q)
     Imp p q ->  (bv p) `union`  (bv q)
     Equiv p q ->  (bv p) `union`  (bv q)
     All x p -> [x] `union` (bv p)
     Ex x p -> [x] `union` (bv p)

-- Cerradura universal
-- Recibe una formula y devuelve una formula donde todas las variables libres
-- de la formula original, estan ligadas a un cuantificador universal
aCl :: Form -> Form
aCl phi = aCl_aux phi (fv phi)

-- Recibe una formula y una lista de indices de variables 
-- Devuelve una formula donde todas las variables en la lista estan ligadas a un cuantificador universal
aCl_aux :: Form -> [Ind] -> Form
aCl_aux phi l = case l of
    [] -> phi
    x:xs -> All x (aCl_aux phi xs)

-- Cerradura universal
-- Recibe una formula y devuelve una formula donde todas las variables libres
-- de la formula original, estan ligadas a un cuantificador existencial
eCl :: Form -> Form
eCl phi = eCl_aux phi (fv phi)

-- Recibe una formula y una lista de indices de variables 
-- Devuelve una formula donde todas las variables en la lista estan ligadas a un cuantificador existencial
eCl_aux :: Form -> [Ind] -> Form
eCl_aux phi l = case l of
    [] -> phi
    x:xs -> Ex x (eCl_aux phi xs)

-- Recibe una formula y devuelve una lista de tuplas (A,B), donde A es una cuanticiacion
-- que es una subformula de la entrada, y B es su alcance
alcF :: Form -> [(Form,Form)]
alcF phi = case phi of
    TrueF -> []
    FalseF -> []
    Pr _ _ -> []
    Eq _ _ -> []
    Neg p ->  alcF p
    Conj p q ->  (alcF p) ++ (alcF q)
    Disy p q ->  (alcF p) ++ (alcF q)
    Imp p q ->  (alcF p) ++ (alcF q)
    Equiv p q ->  (alcF p) ++ (alcF q)
    All x p -> [(All x p, p)] ++ (alcF p)
    Ex x p -> [(Ex x p, p)] ++ (alcF p)

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

-- Recibe una formula y una substitucion
-- Devuelve una nueva funcion con la substitucion aplicada
-- Si la variable a substituir o el nuevo termino es igual a una variable ligada
-- Arrojara un error
apsubF :: Form -> Subst -> Form
apsubF phi sus = case phi of
    TrueF -> TrueF
    FalseF -> FalseF
    Pr p lt -> Pr p [apsubT t sus | t <- lt]
    Eq t1 t2 -> Eq (apsubT t1 sus) (apsubT t2 sus)
    Neg p -> Neg (apsubF p sus)
    Conj p q -> Conj (apsubF p sus) (apsubF q sus)
    Disy p q -> Disy (apsubF p sus) (apsubF q sus)
    Imp p q -> Imp (apsubF p sus) (apsubF q sus)
    Equiv p q -> Equiv (apsubF p sus) (apsubF q sus)
    All x p -> if elem x lv
               then error "Sustitucion invalida"
               else All x (apsubF p sus)
        where lv = union xs ts 
              (xs, tt) = unzip sus
              ts = concat (map varT tt)
              --ts = concat [varT t|t <- tt]
    Ex x p -> if elem x lv
              then error "Sustitucion invalida"
              else Ex x (apsubF p sus)
        where lv = union xs ts 
              (xs, tt) = unzip sus
              ts = concat (map varT tt)
              --ts = concat [varT t|t <- tt]


-- Recibe una lista de objetos comparables, y regresa verdadero si hay repeticiones
tieneRep :: (Eq a) => [a] -> Bool
tieneRep l = case l of
    [] -> False
    x:xs -> if(elem x xs)
            then True
            else tieneRep xs

--Puntos extras: vAlfaEq, renVL (alfa equivalente), renVLconj, apSubF2

-- Recive una formula y una lista de indices de variables
-- Devuelve una formula alfa-equivalente donde los nombres de las variables ligadas
-- no conciden con los de la lista dada
renVLconj :: Form -> [Ind] -> Form
renVLconj phi l = case phi of 
    TrueF -> TrueF
    FalseF -> FalseF
    Pr p lt -> Pr p lt
    Eq t1 t2 -> Eq t1 t2
    Neg p -> Neg (renVLconj p l)
    Conj p q -> Conj (renVLconj p l) (renVLconj q l)
    Disy p q -> Disy (renVLconj p l) (renVLconj q l)
    Imp p q -> Imp (renVLconj p l) (renVLconj q l)
    Equiv p q -> Equiv (renVLconj p l) (renVLconj q l)
    All x p -> if (elem x l)
               then All xp (renVLconj (apsubF p [(x,V xp)]) (xp:l))
               else All x (renVLconj p (x:l))
        where xp = (maximum (l `union` (fv p) `union` (bv p))) + 1
    Ex x p -> if (elem x l)
              then Ex xp (renVLconj (apsubF p [(x,V xp)]) (xp:l))
              else Ex x (renVLconj p (x:l))
        where xp = (maximum (l `union` (fv p) `union` (bv p))) + 1

-- Recibe una formula y devuelve una formula alfa-equivalente donde los nombres
-- de las variables ligadas no coniciden con los de las variables libres
-- Hace uso de la funcion renVLconj con las variables libres de la formula como entrada
renVL:: Form -> Form
renVL p = renVLconj p (fv p)

-- Recibe dos formulas y regresa verdadero si son alfa-equivalentes
vAlfaEq:: Form -> Form -> Bool
vAlfaEq phi psi = case phi of
    TrueF -> case psi of
        TrueF -> True
        _ -> False
    FalseF -> case psi of
        FalseF -> True
        _ -> False
    Pr p1 lt1 -> case psi of 
        Pr p2 lt2 -> if (p1 == p2 && lt1 == lt2)
                     then True
                     else False
        _ -> False
    Eq t1 t2 -> case psi of
        Eq t3 t4 -> if (t1 == t3 && t2 == t4)
                    then True
                    else False 
        _ -> False
    Neg p1 -> case psi of
        Neg p2 -> vAlfaEq p1 p2
        _ -> False
    Conj p1 q1 -> case psi of
        Conj p2 q2 -> (vAlfaEq p1 p2) && (vAlfaEq q1 q2)
        _ -> False
    Disy p1 q1 -> case psi of
        Disy p2 q2 -> (vAlfaEq p1 p2) && (vAlfaEq q1 q2)
        _ -> False
    Imp p1 q1 -> case psi of
        Imp p2 q2 -> (vAlfaEq p1 p2) && (vAlfaEq q1 q2)
        _ -> False
    Equiv p1 q1 -> case psi of
        Equiv p2 q2 -> (vAlfaEq p1 p2) && (vAlfaEq q1 q2)
        _ -> False
    All x p -> case psi of 
        All y q -> vAlfaEq (apsubF p [(x,V xp)]) (apsubF q [(y,V xp)]) 
            where xp = (maximum ((fv p) `union` (bv p) `union` (fv q) `union` (bv q))) + 1
        _ -> False
    Ex x p -> case psi of 
        Ex y q -> vAlfaEq (apsubF p [(x,V xp)]) (apsubF q [(y,V xp)]) 
            where xp = (maximum ((fv p) `union` (bv p) `union` (fv q) `union` (bv q))) + 1
        _ -> False

-- Recibe una formula y una substitucion
-- Devuelve una nueva funcion con la substitucion aplicada
-- Si la variable a substituir o el nuevo termino es igual a una variable ligada
-- La substitucion se aplicara sobre una formula alfa-equivalente donde no coincidan
-- Para que la sustitucion sea posible
apSubF2 :: Form -> Subst -> Form
apSubF2 phi sus = apsubF (renVLconj phi lv) sus
    where lv = union xs ts 
          (xs, tt) = unzip sus
          ts = concat (map varT tt)