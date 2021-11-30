module Angabe6 where
import Data.List ( transpose )
{-# ANN module "HLint: ignore Use camelCase" #-}

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
   5. Ersetzen Sie die Trivialimplementierungen error "Nicht implementiert" durch
      sinnvolle Implementierungen, die die jeweilige Aufgabenstellung erfüllen.
-}

m1 = Mf (2, 2) (\z s -> if z == 1 then s else z + s) :: MatrixF

m2 = Mf (2, 2) (\z s -> s + ((z - 1) * (snd (2, 2)))) :: MatrixF

m3 = Mf (2, 2) (\z s -> s + ((z - 1) * (snd (mtyp m2)))) :: MatrixF

m4 = Mf (2, 2) (\z s -> if z == 1 then (succ (fib (s - 1))) else ((+) z (binom z (s - 1)))) :: MatrixF

m5 = Mf (3, 2) (\z s -> if z == 1 then s else if z == 2 then z + s else succ (z + s)) :: MatrixF

m6 = Mf (3, 2) (\z s -> s + ((z - 1) * (snd (mtyp m5)))) :: MatrixF

m7 = Mf (0, 0) (\_ _ -> 0) :: MatrixF

m8 = Mf (0, 0) (\z s -> z + s) :: MatrixF

type Nat0 = Int

type Nat1 = Int

type Zeilenzahl = Nat1

type Spaltenzahl = Nat1

type Zeile = Nat1

type Spalte = Nat1

type Skalar = Int

type Matrixtyp = (Zeilenzahl, Spaltenzahl)

type Matrixfkt = Zeile -> Spalte -> Skalar -- ausschliessl. total def. Abb.!

-- Matrizenwerte als Typ und funktionale Darstellung
data MatrixF = Mf {mtyp :: Matrixtyp, mf :: Matrixfkt}

-- Namesvereinbarung fuer den Fehlerwert
fehler = Mf (0, 0) (\_ _ -> 0) :: MatrixF

-- Aufgabe A.1

instance Show MatrixF where
  show (Mf (0, 0) _) = "()"
  show (Mf t f) = "(" ++ showFunktion (t, f) ++ ")"

showFunktion :: (Matrixtyp, Matrixfkt) -> String
showFunktion ((1, s), f) = show (showZeile ((1, s), f))
showFunktion ((z, s), f) = showFunktion ((z - 1, s), f) ++ " " ++ show (showZeile ((z, s), f))

showZeile :: (Matrixtyp, Matrixfkt) -> [Skalar]
showZeile ((z, 1), f) = [f z 1]
showZeile ((z, s), f) = showZeile ((z, s - 1), f) ++ [f z s]

fib :: Nat0 -> Nat0
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

binom :: Nat0 -> Nat0 -> Nat1
binom n k
  | n == 0 || n == k = 1
  | True = binom (n - 1) (k - 1) + binom (n - 1) k

{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer Show folgendermassen vor:
   ...
-}

-- Aufgabe A.2

matrixtyp :: MatrixF -> Maybe Matrixtyp
matrixtyp (Mf (0, _) f) = Nothing
matrixtyp (Mf (_, 0) f) = Nothing
matrixtyp (Mf t f) = Just t

{- Knapp, aber gut nachvollziehbar geht natrixtyp folgendermassen vor:
   ...
-}

-- Aufgabe A.4

instance Eq MatrixF where
  (==) (Mf t1 f1) (Mf t2 f2)
    | t1 == (0, 0) || t2 == (0, 0) = error "Gleichheit undefiniert"
    | show (Mf t1 f1) == show (Mf t2 f2) = True
    | otherwise = False
  (/=) (Mf t1 f1) (Mf t2 f2)
    | t1 /= (0, 0) || t2 /= (0, 0) = error "Ungleichheit undefiniert"
    | show (Mf t1 f1) /= show (Mf t2 f2) = True
    | otherwise = False

{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer Eq folgendermassen vor:
   ...
-}

-- Aufgabe A.5

instance Num MatrixF where
  (Mf t1 f1) + (Mf t2 f2)
    | t1 /= t2 || t1 == (0, 0) || t2 == (0, 0) = fehler
    | otherwise = Mf t1 (\z s -> f1 z s + f2 z s)
  (Mf t1 f1) - (Mf t2 f2)
    | t1 /= t2 || t1 == (0, 0) || t2 == (0, 0) = fehler
    | otherwise = Mf t1 (\z s -> f1 z s - f2 z s)
  {-   (Mf t1@(z1, s1) f1) * (Mf t2@(z2, s2) f2)
    | t1 == (0, 0) || t2 == (0, 0) || s1 /= z2 = fehler
    | otherwise = Mf t1 (\z s -> sum[f1 z x*f2 x s | x <- [1..s1]]) -}
  m1@(Mf (z1,s1) f1) * m2@(Mf (z2,s2) f2) 
   | s1 == z2 = Mf (z1,s2) (\z s -> multi m1 m2 (z,s) 1)
   | otherwise = fehler
  negate (Mf t f)
    | t == (0, 0) = fehler
    | otherwise = Mf t (\z s -> (-1) * f z s)
  abs (Mf t f) = Mf t (\z s -> if f z s < 0 then negate (f z s) else f z s)
  signum mf@(Mf t f)
    | t == (0, 0) = fehler
    | Mf t (\_ _ -> 0) == mf = 0
    | mf == abs mf && checkZero mf = 1
    | negate mf == abs mf = -1
    | otherwise = error "Vorzeichenfunktion undefiniert"
  fromInteger n = Mf (1, 1) (\_ _ -> fromInteger n)

checkZero :: MatrixF -> Bool 
checkZero (Mf t@(z,s) f) = Mf t (\z s -> if f z s == 0 then 0 else 1) == Mf t (\_ _ -> 1)

multi :: MatrixF -> MatrixF -> (Zeilenzahl,Spaltenzahl) -> Int -> Int
multi  m1@(Mf (z1,s1) f1) m2@(Mf (z2,s2) f2) (z,s) count 
  | count < s1 = f1 z count * f2 count s + multi m1 m2 (z,s) (count + 1)
  | otherwise = f1 z count * f2 count s

matrixToList :: MatrixF -> [[Skalar]]
matrixToList (Mf (r, c) f) = map (\i -> map (f i) [1 .. c]) [1 .. r]
{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer Num folgendermassen vor:
   ...
-}