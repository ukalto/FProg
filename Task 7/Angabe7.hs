module Angabe7 where

import Data.List ( transpose )
{- import Data.Matrix
 -}
{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
   5. Ersetzen Sie die Trivialimplementierungen error "Nicht implementiert" durch
      sinnvolle Implementierungen, die die jeweilige Aufgabenstellung erfüllen.
   6. Kopieren Sie Ihre Implementierungen von Angabe 3 bzw. 6 an den
      entsprechenden Stellen ein. Beachten Sie, dass dafür drei Umbennennungen
      erforderlich sind, um Namenskonflikte zwischen Bezeichnungen von
      Angabe 3 und 6 zu vermeiden.
-}

type Nat0 = Int

type Nat1 = Int

-- Aufgabe A.1

-- Von Angabe 3 wiederholt:

-- Umbenennung 1: Zeile von Angabe 3 wird fuer Angabe 7 umbenannt zu Matrixzeile
type Matrixzeile = [Int]

-- Matrizen konzeptuell als Listen von Matrixzeilen dargestellt:
newtype Matrix = M [Matrixzeile]

fehlerwert = M [] :: Matrix

data Matrixtyp = Mat (Nat1, Nat1) | KeineMatrix deriving (Eq, Show)

createBrackets :: [Matrixzeile] -> String
createBrackets z = "(" ++ showMatrix z ++ ")"

showMatrix :: [Matrixzeile] -> String
showMatrix [] = []
showMatrix a@(x : xs)
  | (x == last a && xs == []) = show (x)
  | otherwise = show (x) ++ " " ++ showMatrix xs

instance Show Matrix where
  show (M z) = createBrackets z

matrixtyp :: Matrix -> Matrixtyp
matrixtyp (M x)
  | x == [] = KeineMatrix
  | (validMatrix x && length x > 0 && length (head x) > 0) = Mat (length x, length (head x))
  | otherwise = KeineMatrix

validMatrix :: [Matrixzeile] -> Bool
validMatrix [] = True
validMatrix a@(x : xs)
  | length x == length (last a) = validMatrix xs
  | otherwise = False

instance Eq Matrix where
  (==) (M x) (M y)
    | (matrixtyp (M x) == KeineMatrix || matrixtyp (M y) == KeineMatrix) = error "Gleichheit undefiniert"
    | sameSigns (x, y) = True
    | otherwise = False
  (/=) (M x) (M y)
    | (matrixtyp (M x) == KeineMatrix || matrixtyp (M y) == KeineMatrix) = error "Ungleichheit undefiniert"
    | sameSigns (x, y) = False
    | otherwise = True

sameSigns :: ([Matrixzeile], [Matrixzeile]) -> Bool
sameSigns ([], []) = True
sameSigns ([], ys) = False
sameSigns (xs, []) = False
sameSigns ((x : xs), (y : ys))
  | (head x) == (head y) = sameSigns (xs, ys)
  | otherwise = False

instance Num Matrix where
  (+) (M x) (M y)
    | checkBothMatrix ((M x), (M y)) = (M (add x y))
    | otherwise = M []
  (-) (M x) (M y)
    | checkBothMatrix ((M x), (M y)) = (M (sub x y))
    | otherwise = M []
  (*) (M x) (M y)
    | matrixtyp (M x) /= KeineMatrix && matrixtyp (M y) /= KeineMatrix && length (head x) == length y = (M (mmmult x y))
    | otherwise = M []

  signum (M x)
    | matrixtyp (M x) == KeineMatrix = error "Vorzeichenfunktion undefiniert"
    | length x == checkSig x = 1
    | checkSig x == 0 = -1
    | length x * 2 == checkSig x = 0

  fromInteger x = M [[fromInteger x]]

  negate (M x)
    | matrixtyp (M x) == KeineMatrix = M []
    | otherwise = (M (combineNegatives x))

  abs (M x)
    | matrixtyp (M x) == KeineMatrix = M []
    | otherwise = (M (createAbs x))

checkSig :: [Matrixzeile] -> Int
checkSig [] = 0
checkSig (x : xs)
  | all (< 0) x = 0 + checkSig xs
  | all (> 0) x = 1 + checkSig xs
  | all (== 0) x = 2 + checkSig xs
  | otherwise = error "Vorzeichenfunktion undefiniert"

checkBothMatrix :: (Matrix, Matrix) -> Bool
checkBothMatrix ((M x), (M y))
  | (matrixtyp (M x) == matrixtyp (M y)) && (matrixtyp (M x) /= KeineMatrix && matrixtyp (M y) /= KeineMatrix) = True
  | otherwise = False

add :: Num a => [[a]] -> [[a]] -> [[a]]
add m n = zipWith (zipWith (+)) m n

sub :: Num a => [[a]] -> [[a]] -> [[a]]
sub m n = zipWith (zipWith (-)) m n

mmmult :: Num a => [[a]] -> [[a]] -> [[a]]
mmmult m n = [[sum $ zipWith (*) ar bc | bc <- (transpose n)] | ar <- m]

getColumns :: Matrix -> Int
getColumns (M x) = length x

getRows :: Matrix -> Int
getRows (M x) = length (head x)

combineNegatives :: [Matrixzeile] -> [Matrixzeile]
combineNegatives [] = []
combineNegatives (x : xs) = [multEveryElement x] ++ combineNegatives xs

multEveryElement :: [Int] -> [Int]
multEveryElement = foldr (\x -> (++) [x * (-1)]) []

createAbs :: [Matrixzeile] -> [Matrixzeile]
createAbs [] = []
createAbs (x : xs)
  | all (>= 0) x = [x] ++ combineNegatives xs
  | otherwise = [multEveryElementIfNegative x] ++ combineNegatives xs

multEveryElementIfNegative :: [Int] -> [Int]
multEveryElementIfNegative [] = []
multEveryElementIfNegative (x : xs)
  | x >= 0 = [x] ++ multEveryElementIfNegative xs
  | otherwise = [x * (-1)] ++ multEveryElementIfNegative xs

-- Von Angabe 6 wiederholt:

type Zeilenzahl = Nat1

type Spaltenzahl = Nat1

type Zeile = Nat1

type Spalte = Nat1

type Skalar = Int

-- Umbenennung 2: Matrixtyp von Angabe 6 wird fuer Angabe 7 umbenannt zu Matrixtyp'
type Matrixtyp' = (Zeilenzahl, Spaltenzahl)

type Matrixfkt = Zeile -> Spalte -> Skalar -- ausschliessl. total def. Abb.!

-- Matrizenwerte als Typ und funktionale Darstellung
data MatrixF = Mf {mtyp :: Matrixtyp', mf :: Matrixfkt}

-- Namesvereinbarung fuer den Fehlerwert des Typs MatrixF
fehler = Mf (0, 0) (\_ _ -> 0) :: MatrixF

instance Show MatrixF where
  show (Mf (0, 0) _) = "()"
  show (Mf t f) = "(" ++ showFunktion (t, f) ++ ")"

showFunktion :: (Matrixtyp', Matrixfkt) -> String
showFunktion ((1, s), f) = show (showZeile ((1, s), f))
showFunktion ((z, s), f) = showFunktion ((z - 1, s), f) ++ " " ++ show (showZeile ((z, s), f))

showZeile :: (Matrixtyp', Matrixfkt) -> [Skalar]
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

matrixtyp' :: MatrixF -> Maybe Matrixtyp'
matrixtyp' (Mf (0, _) f) = Nothing
matrixtyp' (Mf (_, 0) f) = Nothing
matrixtyp' (Mf t f) = Just t

instance Eq MatrixF where
  (==) (Mf t1 f1) (Mf t2 f2)
    | t1 == (0, 0) || t2 == (0, 0) = error "Gleichheit undefiniert"
    | show (Mf t1 f1) == show (Mf t2 f2) = True
    | otherwise = False
  (/=) (Mf t1 f1) (Mf t2 f2)
    | t1 /= (0, 0) || t2 /= (0, 0) = error "Ungleichheit undefiniert"
    | show (Mf t1 f1) /= show (Mf t2 f2) = True
    | otherwise = False

instance Num MatrixF where
  (Mf t1 f1) + (Mf t2 f2)
    | t1 /= t2 || t1 == (0, 0) || t2 == (0, 0) = fehler
    | otherwise = Mf t1 (\z s -> f1 z s + f2 z s)
  (Mf t1 f1) - (Mf t2 f2)
    | t1 /= t2 || t1 == (0, 0) || t2 == (0, 0) = fehler
    | otherwise = Mf t1 (\z s -> f1 z s - f2 z s)
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

-- Aufgabe A.2

class (Eq a, Num a, Show a) => MatrixTyp a where
  madd, msub, mmult :: a -> a -> a
  msmult :: Int -> a -> a
  mtransp :: a -> a
  mdet :: a -> Maybe Int
  mfehler :: a

  -- Protoimplementierungen
  madd = (+)
  mmult = (*)
  msub = (-)
  mfehler = error "Fehler"
 

instance MatrixTyp Matrix where
  msmult n (M zs) = M (multSkalar n zs)
  mdet (M zs) = error "Nicht implementiert!"
  mtransp (M zs) = M (transpose zs)
  mfehler = fehlerwert

instance MatrixTyp MatrixF where
  msmult n (Mf t f) = Mf t (\z s -> n * f z s)
  mdet (Mf t f) = error "Nicht implementiert!"
  mtransp (Mf t@(r, c) f) = Mf (c,r) (\s z -> f z s)
  mfehler = fehler

multSkalar :: Int -> [Matrixzeile] -> [Matrixzeile]
multSkalar _ [] = []
multSkalar n (x:xs) = [multRow n x] ++ multSkalar n xs

multRow :: Int -> [Int] -> [Int]
multRow _ [] = []
multRow n (x:xs) = [x*n] ++ multRow n xs

{- Knapp, aber gut nachvollziehbar gehen die Instanzbildungen fuer
   MatrixTyp folgendermassen vor:
   ... -}


-- Aufgabe A.3

konv1 :: Matrix -> MatrixF
konv1 (M zs) 
  | matrixtyp (M zs) /= KeineMatrix = Mf (length zs, length(head zs)) (\z s -> (zs !! (z-1)) !! (s-1))
  | otherwise = fehler

konv2 :: MatrixF -> Matrix
konv2 mf@(Mf t f) = M (skalarToMatrixzeile (matrixToList mf))

skalarToMatrixzeile :: [[Skalar]] -> [[Int]]
skalarToMatrixzeile [] = []
skalarToMatrixzeile (x:xs) = [toInt x] ++ skalarToMatrixzeile xs

toInt :: [Skalar] -> [Int]
toInt [] = []
toInt (x:xs) = [x] ++ toInt xs

{- Knapp, aber gut nachvollziehbar gehen die Konvertierungsfunktionen
   konv1 und konv2 folgendermassen vor:
   ...
-}

-- Aufgabe A.6

{- type Konservenzahl = Nat0

type Bahnhof     = String
type Ausgangsbhf = Bahnhof
type Zielbhf     = Bahnhof
type Von         = Bahnhof
type Nach        = Bahnhof

data VHDS   = Viertel | Halb | Dreiviertel | Schlag
               deriving (Eq,Ord,Enum,Show)
data Stunde = I | II | III | IV | V | VI | VII | VIII | IX | X
              | XI | XII | XIII | XIV | | XV | XVI | XVII | XVIII
              | XIX | XX | XXI | XXII | XXIII | XXIV
               deriving (Eq,Ord,Enum,Show)

data Abfahrtzeit = AZ { vds :: VHDS, std :: Stunde } deriving (Eq,Ord,Show)
data Reisedauer  = RD { vs :: Stunde, bt :: VHDS } deriving (Eq,Ord,Show)

type Fahrplan = [(Abfahrtzeit,Von,Nach,Reisedauer)]

konservenrechner :: Fahrplan -> Ausgangsbhf -> Zielbhf -> Maybe Konservenzahl
konservenrechner fp ab zb = error "Nicht implementiert!" -}

{- Knapp, aber gut nachvollziehbar geht die Funktion konservenrechner
   folgendermassen vor:
   ...
-}
