module Main where

import Syntax
import Mar_Mont

main = do
    putStrLn "Ejemplo unificacion de Literales"
    putStrLn "Literales:"
    let lista = [Pr "P" [V 1, V 2, F "f" [V 1]],Pr "P" [V 3, V 2, F "f" [V 3]],Pr "P" [V 3, V 4, F "f" [V 3]]]
    print $ lista
    let sus = mmE lista
    putStrLn "Sustitucion:"
    print $ sus
    putStrLn "Conjunto unificado:"
    print $ sust_G lista sus 
    putStrLn "Literales:"
    let p1 = Pr "P" [V 1, V 2, F "f" [V 1]]
    let p2 = Pr "P" [V 3, V 2, F "f" [V 3]]
    print $ p1
    print $ p2
    let sus = unificaLit p1 p2
    putStrLn "Sustitucion:"
    print $ sus
    putStrLn "Terminos:"
    let lista = [F "f" [V 2], F "f" [V 1], F "f" [V 1]]
    print $ lista
    let sus = unificaConj lista
    putStrLn "Sustitucion:"
    print $ sus
    putStrLn "Terminos:"
    let p1 = F "f" [V 2]
    let p2 = F "f" [V 1]
    print $ p1
    print $ p2
    let sus = unifica p1 p2
    putStrLn "Sustitucion:"
    print $ sus
