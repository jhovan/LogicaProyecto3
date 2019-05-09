module Mar_Mont where

import Data.List
import Syntax

-- Recibe una sustitucion y elimina los casos 
-- donde una variable se sustituye por si misma
simpSus :: Subst -> Subst
simpSus sus = [(x,t)|(x,t) <- sus, V x /= t]

-- Recibe una dos sustituciones y las compone como una sola
compSus :: Subst -> Subst -> Subst
compSus s1 s2 = zs ++ ws
    where zs = [(x,apsubT t s2) | (x,t) <- s1]
          ws = [(x,t) | (x,t) <- s2, not (elem x vs1)]
          vs1 = fst (unzip s1)

-- Recibe una lista y hace una lista de pares encadenando a los elementos
hazPares :: [a] -> [(a,a)]
hazPares l = case l of
    [] -> []
    x:y:xs -> (x,y):(hazPares (y:xs))
    x:xs -> []

-- Recibe una lista de pares de terminos y devuelve el umg de estos
-- (Es Martelli-Montanari para terminos)
unificaC_aux :: [(Term, Term)] -> Subst
unificaC_aux pares = case pares of
    [] -> []
    (t1, t2):lp -> case (t1, t2) of
        (F f lt1, F g lt2) -> if f == g && length (lt1) == length (lt2)
                              then unificaC_aux ((zip lt1 lt2) ++ lp) -- DESC
                              else error "Imposible unificar" -- DFALLA
        (V x, V y) -> if x == y 
                      then unificaC_aux lp -- ELIM
                      else compSus d (unificaC_aux lps) -- SUST (cuando t es variable)
                where d = [(x, V y)]
                      lps = [(apsubT t1 d, apsubT t2 d)|(t1,t2) <- lp]
        (V x, F f lt) -> if elem x (varT (F f lt))
                         then error "Imposible unificar" -- SFALL
                         else compSus d (unificaC_aux lps) -- SUST (cuando t es funcion)
                where d = [(x, F f lt)]
                      lps = [(apsubT t1 d, apsubT t2 d)|(t1,t2) <- lp]
        (F f lt, V x) -> unificaC_aux ((t2, t1):lp) --SWAP

-- Recibe una lista de terminos y devuelve el umg
-- Se auxilia de unificaC_aux y hazPares
unificaConj :: [Term] -> Subst
unificaConj = unificaC_aux.hazPares

-- Recibe dos terminos y devuelve el umg de estos
unifica :: Term -> Term -> Subst
unifica s t = unificaConj [s,t]

-- Recibe dos literales y devuelve el umg de estos
-- Incluye las reglas extra de Martelli-Montanari extendido
-- Hace uso de unificaC_aux en los terminos de las literales
unificaLit :: Lit -> Lit -> Subst
unificaLit phi psi = case (phi,psi) of 
    (TrueF, TrueF) -> []
    (FalseF, FalseF) -> []
    (Pr p lt1, Pr q lt2) -> if  p == q && length lt1 == length lt2
                            then unificaC_aux (zip lt1 lt2)
                            else error "Imposible unificar"
    (Eq t1 s1, Eq t2 s2) ->  unificaC_aux [(t1,t2),(s1,s2)]
    _ -> error "Imposible unificar"

-- Recibe una lista de pares de literales
-- Devuelve una lista de pares de terminos 
-- en caso de que el conjunto sea unificable 
-- bajo los criterios extra de Martelli-Montanari extendido
-- que serviran de entrada para unificaC_aux
mmE_aux :: [(Lit, Lit)] -> [(Term, Term)]
mmE_aux pares = case pares of
    [] -> []
    (phi, psi):lp -> case (phi, psi) of
        (TrueF, TrueF) -> mmE_aux lp
        (FalseF, FalseF) -> mmE_aux lp
        (Pr p lt1, Pr q lt2) -> if  p == q && length lt1 == length lt2 
                                then union (zip lt1 lt2) (mmE_aux lp) -- Desc_Ex
                                else error "Imposible unificar" -- DFalla_Ex
        (Eq t1 s1, Eq t2 s2) ->  union [(t1,t2),(s1,s2)] (mmE_aux lp)
        _ -> error "Imposible unificar"

-- Recibe una lista de literales y devuelve el umg
-- Hace uso de hazPares para hacer pares de literales
-- mmE_aux para obtener una lista de pares de terminos
-- que sirven de entrada para unificaC_aux
mmE :: [Lit] -> Subst
mmE = unificaC_aux.mmE_aux.hazPares

-- Aplica una sustitucion a una lista de literales
sust_G :: [Lit] -> Subst -> [Lit]
sust_G literales sus = case literales of
    [] -> []
    l:lp -> union [apsubL l sus] (sust_G lp sus)
