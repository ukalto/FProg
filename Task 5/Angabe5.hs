module Angabe5 where
import Data.List

{-# ANN module "HLint: ignore Use camelCase" #-}

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}

type Nat0 = Int
-- Die selbstdefinierte Typklasse Menge_von:

class Eq a => Menge_von a where
  leer :: [] a
  vereinige :: [] a -> [] a -> [] a
  schneide :: [] a -> [] a -> [] a
  ziehe_ab :: [] a -> [] a -> [] a
  ist_teilmenge :: [] a -> [] a -> Bool
  ist_obermenge :: [] a -> [] a -> Bool
  ist_element :: a -> [] a -> Bool
  ist_leer :: [] a -> Bool
  sind_gleich :: [] a -> [] a -> Bool
  anzahl :: a -> [] a -> Nat0

  -- Protoimplementierungen
  leer = []
  vereinige xs ys = xs ++ ys
  ist_teilmenge xs ys = ist_obermenge ys xs
  ist_obermenge xs ys = ist_teilmenge ys xs
  ist_element x xs = anzahl x xs >= 1
  ist_leer xs = xs == leer
  sind_gleich xs ys = ist_teilmenge xs ys && ist_teilmenge ys xs 

-- Weitere Typen:

newtype Paar a b = P (a, b) deriving (Eq, Show)

data Zahlraum_0_10
  = N
  | I
  | II
  | III
  | IV
  | V
  | VI
  | VII
  | VIII
  | IX
  | X
  | F
  deriving (Eq, Ord, Show, Enum)

newtype Funktion = Fkt {f :: Zahlraum_0_10 -> Zahlraum_0_10}

data Baum a = Blatt a | Knoten (Baum a) a (Baum a) deriving (Eq, Show)

newtype ElemTyp a = ET a

-- Pseudoheterogene Elementtypen
data PH_ElemTyp a b c d e = A a | B b | C c | D d | E e deriving (Eq, Show)

data PH_ElemTyp' q r s = Q q | R r | S s deriving (Eq, Show)

-- Aufgabe A.1

instance Num Zahlraum_0_10 where
  (+) num1 num2
    | num1 == F || num2 == F = F
    | ((fromEnum (num1) + fromEnum (num2)) <= 10) = toEnum (fromEnum (num1) + fromEnum (num2))
    | otherwise = F
  (-) num1 num2
    | num1 == F || num2 == F = F
    | fromEnum (num1) >= fromEnum (num2) = toEnum (fromEnum (num1) - fromEnum (num2))
    | otherwise = F
  (*) num1 num2
    | num1 == F || num2 == F = F
    | ((fromEnum (num1) * fromEnum (num2)) <= 10) = toEnum (fromEnum (num1) * fromEnum (num2))
    | otherwise = F
  fromInteger num
    | num >= 0 && num <= 10 = toEnum $ fromIntegral num
    | otherwise = F
  signum num1
    | num1 == F = F
    | num1 == N = N
    | fromEnum (num1) > 0 = I
  negate num
    | num == N = N
    | otherwise = F
  abs num = num

{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer Num folgendermassen vor:
   ...
-}

-- Aufgabe A.2

instance Eq Funktion where
  (==) f1 f2
    | show f1 == show f2 = True 
    | otherwise = False
  (/=) f1 f2
    | show f1 /= show f2 = True 
    | otherwise = False

instance Show Funktion where
   show f = ("{" ++ (showFunktion (createTupel (N, f))) ++ "}")

showFunktion :: [(Zahlraum_0_10,Zahlraum_0_10)] -> String
showFunktion (x:xs)
   | x /= (F,F) = show(x) ++ "," ++ showFunktion xs
   | otherwise = show(x)

createTupel :: (Zahlraum_0_10, Funktion) -> [(Zahlraum_0_10,Zahlraum_0_10)]
createTupel (z, (fkt@(Fkt f)))
   | z == F = [(z,f z)]
   | otherwise = [(z,f z)] ++ createTupel(succ(z), (fkt))

{- Knapp, aber gut nachvollziehbar gehen die beiden Instanzbildungen fuer
   Eq und Show folgendermassen vor:
   ...
-}

-- Aufgabe A.3

instance Menge_von Int where
   vereinige int1 int2
      | validArray int1 && validArray int2 = int1 `union` int2
      | otherwise = error "Fehler"
   schneide int1 int2
      | validArray int1 && validArray int2 = int1 `intersect` int2
      | otherwise = error "Fehler"
   ziehe_ab int1 int2
      | validArray int1 && validArray int2 = int1 \\ int2
      | otherwise = error "Fehler"
   anzahl el list
      | count el list > 1 || not(validArray list) = error "Fehler"
      | otherwise = count el list
   ist_teilmenge int1 int2
      | validArray int1 && validArray int2 = null (int1 \\ int2)
      | otherwise = error "Fehler"

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

validArray :: Eq a => [a] -> Bool
validArray list = length(list) == length(nub(list))

instance Menge_von Zahlraum_0_10 where
   vereinige int1 int2
      | validArray int1 && validArray int2 = int1 `union` int2
      | otherwise = error "Fehler"
   schneide int1 int2
      | validArray int1 && validArray int2 = int1 `intersect` int2
      | otherwise = error "Fehler"
   ziehe_ab int1 int2
      | validArray int1 && validArray int2 = int1 \\ int2
      | otherwise = error "Fehler"
   anzahl el list
      | count el list > 1 || not(validArray list) = error "Fehler"
      | otherwise = count el list
   ist_teilmenge int1 int2
      | validArray int1 && validArray int2 = null (int1 \\ int2)
      | otherwise = error "Fehler"

instance Menge_von Funktion where
   vereinige f1 f2
      | validArray f1 && validArray f2 = f1 `union` f2
      | otherwise = error "Fehler"
   schneide f1 f2
      | validArray f1 && validArray f2 = f1 `intersect` f2
      | otherwise = error "Fehler"
   ziehe_ab f1 f2
      | validArray f1 && validArray f2 = f1 \\ f2
      | otherwise = error "Fehler"
   anzahl el list
      | count el list > 1 || not(validArray list) = error "Fehler"
      | otherwise = count el list
   ist_teilmenge f1 f2
      | validArray f1 && validArray f2 = null (f1 \\ f2)
      | otherwise = error "Fehler"

{- Knapp, aber gut nachvollziehbar gehen die drei Instanzbildungen fuer
   Menge_von folgendermassen vor:
   ...
-}

-- Aufgabe A.4

instance (Eq a,Eq b) => Menge_von (Paar a b) where
   vereinige a b
      | validArray a && validArray b = a `union` b
      | otherwise = error "Fehler"
   schneide a b
      | validArray a && validArray b = a `intersect` b
      | otherwise = error "Fehler"
   ziehe_ab a b
      | validArray a && validArray b = a \\ b
      | otherwise = error "Fehler"
   anzahl el list
      | count el list > 1 || not(validArray list) = error "Fehler"
      | otherwise = count el list
   ist_teilmenge a b
      | validArray a && validArray b = null (a \\ b)
      | otherwise = error "Fehler"

instance Eq a => Menge_von (Baum a) where
   vereinige a b
      | validArray a && validArray b = a `union` b
      | otherwise = error "Fehler"
   schneide a b
      | validArray a && validArray b = a `intersect` b
      | otherwise = error "Fehler"
   ziehe_ab a b
      | validArray a && validArray b = a \\ b
      | otherwise = error "Fehler"
   anzahl el list
      | count el list > 1 || not(validArray list) = error "Fehler"
      | otherwise = count el list
   ist_teilmenge a b
      | validArray a && validArray b = null (a \\ b)
      | otherwise = error "Fehler"

{- Knapp, aber gut nachvollziehbar gehen die beiden Instanzbildungen fuer
   Menge_von folgendermassen vor:
   ...
-}

-- Aufgabe A.5

instance Eq a => Eq (ElemTyp a) where
   (==) (ET a1) (ET a2) = a1 == a2
   (/=) (ET a1) (ET a2) = a1 /= a2

instance Show a => Show (ElemTyp a) where
   show (ET a) = show(a)

{- Knapp, aber gut nachvollziehbar gehen die beiden Instanzbildungen fuer
   Eq und Show folgendermassen vor:
   ...
-}

-- Aufgabe A.6

instance Eq a => Menge_von (ElemTyp a) where
   vereinige a b = a `union` b
   schneide a b = a `intersect` b
   ziehe_ab a b = a \\ b
   anzahl el list = count el list
   ist_teilmenge a b = null (a \\ b)

{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer
   Menge_von folgendermassen vor:
   ...
-}

-- Aufgabe A.7

instance (Eq a,Eq b,Eq c,Eq d,Eq e) => Menge_von (PH_ElemTyp a b c d e) where
   vereinige a b
      | validArray a && validArray b = a `union` b
      | otherwise = error "Fehler"
   schneide a b
      | validArray a && validArray b = a `intersect` b
      | otherwise = error "Fehler"
   ziehe_ab a b
      | validArray a && validArray b = a \\ b
      | otherwise = error "Fehler"
   anzahl el list
      | count el list > 1 || not(validArray list) = error "Fehler"
      | otherwise = count el list
   ist_teilmenge a b
      | validArray a && validArray b = null (a \\ b)
      | otherwise = error "Fehler"

{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer
   Menge_von folgendermassen vor:
   ...
-}

-- Aufgabe A.8

instance (Eq p,Eq q,Eq r) => Menge_von (PH_ElemTyp' p q r) where
   vereinige a b = a `union` b
   schneide a b = a `intersect` b
   ziehe_ab a b = a \\ b
   anzahl el list = count el list
   ist_teilmenge a b = null (a \\ b)

{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer
   Menge_von folgendermassen vor:
   ...
-}
