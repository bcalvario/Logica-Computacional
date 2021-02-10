--Práctica 2
--Calvario González Berenice

module Practica2 where

type Variable = String
data Pl v = Bot | Top | Var v | Imp (Pl v) (Pl v) | Dis (Pl v) (Pl v) | Con (Pl v) (Pl v) | Neg (Pl v) deriving (Eq,Show)  
type PL = Pl Variable  
type Valuacion = Variable -> Int
type Modelo = [Variable]
    
--Si i en PL es una disyunción de literales,
--entonces disLit2ListLit transforma phi en una lista de literales.
--Ejemplos: (x3 | !x4) --> [x3,!x4], Bot ---> []
disLit2ListLit :: PL -> [PL]
disLit2ListLit phi = case phi of
    Top                 -> []
    Bot                 -> []
    Var x               -> [Var x]
    (Imp alpha beta)    -> ((disLit2ListLit (Neg(alpha)))) ++ (disLit2ListLit beta)
    (Dis alpha beta)    -> (disLit2ListLit alpha) ++ (disLit2ListLit beta)
    (Con alpha beta)    -> (disLit2ListLit (Neg(alpha))) ++ (disLit2ListLit (Neg(beta)))
    Neg (Var x)         -> [Neg (Var x)]
    _                   -> error $
            "disLit2ListLit: phi no es una disyuncion de literales, phi = " ++ (show phi)

-- Tests
-- 
-- disLit2ListLit Top
-- disLit2ListLit Bot
-- disLit2ListLit (Var "p")
-- disLit2ListLit (Imp (Var "p") (Var "q"))
-- disLit2ListLit (Dis (Var "p") (Var "q"))
-- disLit2ListLit (Con (Var "p") (Var "q"))
-- disLit2ListLit (Neg(Var "p"))


-- Dado un literal l en PL, litComp calcula el literal complementario de l.
litComp :: PL -> PL
litComp phi = case phi of
    Top             -> Bot
    Bot             -> Top
    Var x           -> Neg (Var x)
    Imp alpha beta  -> (Neg(litComp alpha)) `Dis` (litComp beta)
    Dis alpha beta  -> (litComp alpha) `Dis` (litComp beta)
    Con alpha beta  -> (litComp alpha) `Con` (litComp beta)
    Neg (Var x)     -> Var x
    _               -> error $ "litComp: phi no es literal, phi = " ++ (show phi)

-- Tests
--
-- litComp Top
-- litComp Bot
-- litComp (Var "p")
-- litComp (Imp (Var "p") (Var "q"))
-- litComp (Dis (Var "p") (Var "q"))
-- litComp (Con (Var "p") (Var "q"))
-- litComp (Neg(Var "p"))


-- Dada una clausula de PL, representada por una lista de literales ll,
-- clausulaVAL determina si ll es una clausula valida.
-- ll es valida sii ll tiene al menos dos literales complementarios.
clausulaVal :: [PL] -> Bool
clausulaVal ll = case ll of
    []      -> False
    (l:ls)  -> (litComp l) `elem` ll || clausulaVal ls

-- Tests
--
-- clausulaVal [(Var "p"), (Neg (Var "q"))]
-- clausulaVal [(Neg (Var "p")), (Var "q")]
-- clausulaVal [(Var "p"), (Var "q")]                   
-- clausulaVal [(Neg (Var "p")), (Neg (Var "q"))]                                      
      

-- Dada phi en PL, cnf2LListLit transforma phi a una formula phi' en CNF,
-- donde phi' esta representada como una lista de listas de literales.
--Ejemplos: (x1 | x2) & (x3 | !x4) ---> [[x1,x2], [x3,!x4]], Top ---> []
cnf2LListLit :: PL -> [[PL]]
cnf2LListLit phi = case phi of
    Top                 -> []
    Bot                 -> []
    Var x               -> [[Var x]]
    (Imp alpha beta)    -> [disLit2ListLit phi]
    (Dis _ _)           -> [disLit2ListLit phi]
    (Con alpha beta)    -> (cnf2LListLit alpha) ++ (cnf2LListLit beta)
    Neg (Var x)         -> [[Neg (Var x)]]
    _                   -> error $ "cnf2LListLit: phi no esta en CNF, phi = " ++ (show phi)

-- Tests 
--
-- cnf2LListLit Top
-- cnf2LListLit Bot
-- cnf2LListLit (Var "p")
-- cnf2LListLit (Imp (Var "p") (Var "q"))
-- cnf2LListLit (Dis (Var "p") (Var "q"))
-- cnf2LListLit (Con (Var "p") (Var "q"))
-- cnf2LListLit (Neg(Var "p"))


-- Dada phi en CNF, representada como una lista de listas de literales lc,
-- clauListTrue determina si todas las clausulas de lc son validas.
-- Es decir clauListTrue determina si todos los elementos de lc son clausulas validas.
clauListVal :: [[PL]] -> Bool
clauListVal lc = case lc of
    []      -> True
    (c:cs)  -> clausulaVal c && clauListVal cs
            
-- Tests
--
-- clauListVal Top
-- clauListVal Bot
-- clauListVal (Var "p")
-- clauListVal (Imp (Var "p") (Var "q"))
-- clauListVal (Dis (Var "p") (Var "q"))
-- clauListVal (Con (Var "p") (Var "q"))
-- clauListVal (Neg(Var "p"))              


-- Dada phi en PL, decide si phi pertenece, o no, a VAL:={phi in PL | forall m: m |= phi}.
-- Esto se hace transformando primero phi a una formula en CNF representada mediante una lista de listas de literales,
-- y luego aplicando clauListVal a dicha lista.
decideCNFenVAL :: PL -> Bool
decideCNFenVAL phi = clauListVal (cnf2LListLit phi)

--Tests
--
-- decideCNFenVA Top
-- decideCNFenVA Bot
-- decideCNFenVA (Var "p")
-- decideCNFenVA (Imp (Var "p") (Var "q"))
-- decideCNFenVA (Dis (Var "p") (Var "q"))
-- decideCNFenVA (Con (Var "p") (Var "q"))
-- decideCNFenVA (Neg(Var "p"))


-- Ejercicio.Función que decide si una fórmula en DNF está en SAT.
decideDNFenSAT :: PL -> Bool
decideDNFenSAT phi = not (clauListVal (cnf2LListLit phi))

-- decideDNFenSAT Top
-- decideDNFenSAT Bot
-- decideDNFenSAT (Var "p")
-- decideDNFenSAT (Imp (Var "p") (Var "q"))
-- decideDNFenSAT (Dis (Var "p") (Var "q"))
-- decideDNFenSAT (Con (Var "p") (Var "q"))
-- decideDNFenSAT (Neg(Var "p"))