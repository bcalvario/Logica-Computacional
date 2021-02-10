-- Práctica 1
-- Calvario González Berenice

module Practica1 where

-- Ejercicio 1. Variables representadas como cadenas.
type Variable = String


-- Ejercicio 2. Valuaciones representadas como listas.
type Valuaciones = [Variable]


-- Ejercicio 3. Función que recibe una fórmula y regresa la lista
-- de las variables usadas en la fórmula.
listaVariables :: PL -> [String]
listaVariables Top = []
listaVariables Bot = []
listaVariables (Var x) = [x]
listaVariables (Imp p q) = (listaVariables p) ++ (listaVariables q)
listaVariables (Dis p q) = (listaVariables p) ++ (listaVariables q)
listaVariables (Con p q) = (listaVariables p) ++ (listaVariables q)
listaVariables (Neg p) = listaVariables p

-- Tests:
-- listaVariables Top
-- listaVariables Bot
-- listaVariables (Var "p")
-- listaVariables (Imp (Var "p") (Var "q"))
-- listaVariables (Dis (Var "p") (Var "q"))
-- listaVariables (Con (Var "p") (Var "q"))
-- listaVariables (Neg (Var "p"))


-- Ejercicio 4.
data PL = Bot | Top | Var Variable | Imp PL PL | Dis PL PL | Con PL PL | Neg PL | Nor PL PL
-- Instancia de la clase show para el tipo de dato PL.
instance Show PL where
    show Top = "true"
    show Bot = "false"
    show (Var x) = show x
    show (Nor p q) = "(" ++ show p ++ " nor " ++ show q ++ ")"
    show (Imp p q) = "(" ++ show p ++ " -> " ++ show q ++ ")"
    show (Dis p q) = "(" ++ show p ++ " v " ++ show q ++ ")"
    show (Con p q) = "(" ++ show p ++ " ^ " ++ show q ++ ")"
    show (Neg p) = "¬" ++ (show p)

-- Tests:
--
-- Top
-- Bot
-- (Var "p")
-- (Imp (Var "p") (Var "q"))
-- (Dis (Var "p") (Var "q"))
-- (Con (Var "p") (Var "q"))
-- (Neg (Var "p"))


-- Ejercicio 5. Función que recibe una fórmula de Lógica Proposicional y
-- regresa una fórmula de Lógica Proposicional sin apariciones del operador
-- implicación.
quitaImp :: PL -> PL
quitaImp (Imp p1 p2) = (Dis (Neg p1) p2)
quitaImp p = p 

-- Tests:
--
-- quitaImp (Imp (Var "p") (Var "q"))
-- quitaImp (Var "p")
-- quitaImp (Con (Var "p") (Var "q"))


--Ejercicio 6. Función que recibe una fórmula de Lógica Proposicional y regresa
--una fórmula en donde solo aparece el operador nor.
lNor :: PL -> PL
lNor Top = Top
lNor Bot = Bot 
lNor (Imp p q) = Nor (Nor (Nor (lNor p) (lNor p)) (lNor q)) (Nor (Nor (lNor p) (lNor p)) (lNor q))
lNor (Dis p q) = Nor (Nor (lNor p) (lNor q)) (Nor (lNor p) (lNor q))
lNor (Con p q) = Nor (Nor (lNor p) (lNor p)) (Nor (lNor q) (lNor q))
lNor (Neg p) = Nor (lNor p) (lNor p)
lNor p = p

-- Tests:
--
-- lNor (Imp (Var "p") (Var "q"))
-- lNor (Dis (Var "p") (Var "q"))
-- lNor (Con (Var "p") (Var "q"))
-- lNor (Neg (Var "p"))
-- lNor Top
-- lNor Bot
-- lNor (Var "p")

type Modelo = [Variable]

mSatisface :: Modelo -> PL -> Bool
mSatisface m phi = case phi of
    Top             -> True
    Bot             -> False
    Var x           -> if m == ["0"] then False else True
    (Imp p q)       -> not(mSatisface m p) || (mSatisface m q)
    (Dis p q)       -> (mSatisface m p) || (mSatisface m q)
    (Con p q)       -> (mSatisface m p) && (mSatisface m q)
    (Neg p)         -> not(mSatisface m p)

-- Tests:
-- 
-- mSatisface [] Top
-- mSatisface [] Bot
-- mSatisface ["0"] (Var "p")
-- mSatisface ["1"] (Var "p")
-- mSatisface ["1"] (Imp (Var "p") (Var "q"))
-- mSatisface ["1"] (Dis (Var "p") (Var "q"))
-- mSatisface ["1"] (Con (Var "p") (Var "q"))
-- mSatisface ["0"] (Neg(Var "p"))
-- mSatisface ["1"] (Neg(Var "p"))
