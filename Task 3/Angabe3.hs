module Angabe3 where
import Data.List ( transpose )

{-# ANN module "HLint: ignore Use camelCase" #-}

-- 1. Vervollstaendigen Sie gemaess Angabentext!
-- 2. Vervollst�ndigen Sie auch die vorgegebenen Kommentaranf�nge!
-- 3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
-- 4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!

type Nat1 = Int
type Zeile = [Int]

-- Matrizen konzeptuell als Listen von Zeilen dargestellt:

newtype Matrix = M [Zeile]

fehlerwert = M [] :: Matrix

data Matrixtyp = Mat (Nat1,Nat1) | KeineMatrix deriving (Eq,Show)

m1 = (M [[1]])
m1_2 = (M [[2]])
m3x3 = (M [[5, 10, 15], [3, 6, 9], [1, 2, 3]])
m3x3_2 = (M [[5, 10, 15], [4, 6, 9], [1, 2, 3]])
minvalid = (M [[1, 2, 3], [2, 3], [5, 6, 7]])
m3x5 = (M [[1, 2, 3, 4, 5], [10, 9, 8, 7, 6], [3, 4, 5, 6, 7]])
m3x5_2 = (M [[5, 5, 5, 5, 5], [0, 9, 3, 7, 8], [3, 3, 2, 3, 7]])
m5x2 = (M [[12, 21], [51, 9], [7, 4], [0, 8], [6, 9]])
m5x2_2 = (M [[12, 1], [1, 9], [7, 49], [9, 8], [0, 0]])
m2x4 = (M [[0, 1, 2, 4], [91, 42, 43, 8]])
m2x4_negative = (M [[0,-1,-2,-4], [-91,-42,-43,-8]])
m2x2 = (M [[1,2],[3,4]])
m2x2_2 = (M [[20,20],[30,40]])
m3x2 = (M [[1,2],[3,4],[5,6]])
m3xinvalid = (M [[1,2,3],[4,5],[6]])
m3xinvalid2 = (M [[1,2,3],[],[6]])
m3x0 = (M [[],[],[]])
mmempty = (M [])
mnegative = (M [[-1, -2, -4], [-8, -22, -4]])
mzero = (M [[0, 0, 0], [0, 0, 0]])

-- Aufgabe A.1

createBrackets :: [Zeile] -> String
createBrackets z = "(" ++ showMatrix z ++ ")"

showMatrix :: [Zeile] -> String
showMatrix [] = []
showMatrix a@(x:xs)
    | (x == last a && xs == []) = show(x)
    | otherwise = show(x)++ " " ++ showMatrix xs

instance Show Matrix where
    show (M z) = createBrackets z


-- Knapp, aber gut nachvollziebar geht die Instanzdeklaration fuer Show folgendermassen vor: 

-- Aufgabe A.2

matrixtyp :: Matrix -> Matrixtyp
matrixtyp (M x)
    | x == [] = KeineMatrix
    | (validMatrix x  && length x > 0 && length (head x) > 0)  = Mat (length x, length (head x))
    | otherwise = KeineMatrix

validMatrix :: [Zeile] -> Bool
validMatrix [] = True
validMatrix a@(x:xs)
    | length x == length (last a) = validMatrix xs
    | otherwise = False


-- Knapp, aber gut nachvollziehbar geht matrixtyp folgendermassen vor: 

-- Aufgabe A.3

instance Eq Matrix where
 (==) (M x) (M y)
    | (matrixtyp (M x) == KeineMatrix || matrixtyp (M y) == KeineMatrix) = error "Gleichheit undefiniert"
    | sameSigns (x,y) = True
    | otherwise = False
 (/=) (M x) (M y)
    | (matrixtyp (M x) == KeineMatrix || matrixtyp (M y) == KeineMatrix) = error "Ungleichheit undefiniert"
    | sameSigns (x,y) = False
    | otherwise = True

sameSigns :: ([Zeile],[Zeile]) -> Bool
sameSigns ([],[]) = True
sameSigns ([],ys) = False
sameSigns (xs,[]) = False
sameSigns ((x:xs),(y:ys))
    | (head x) == (head y) = sameSigns (xs,ys)
    | otherwise = False


-- Knapp, aber gut nachvollziebar geht die Instanzdeklaration fuer Eq folgendermassen vor:

-- Aufgabe A.4

instance Num Matrix where
 (+) (M x) (M y)
    | checkBothMatrix ((M x),(M y)) = (M (add x y))
    | otherwise = M []
 (-) (M x) (M y)
    | checkBothMatrix ((M x),(M y)) = (M (sub x y))
    | otherwise = M []
 (*) (M x) (M y)
    | matrixtyp (M x) /= KeineMatrix && matrixtyp (M y) /= KeineMatrix && length (head x) == length y = (M (mmult x y))
    | otherwise = M []

 signum (M x)
    | matrixtyp (M x) == KeineMatrix = error "Vorzeichenfunktion undefiniert"
    | length x == checkSig x = 1
    | checkSig x == 0 = -1
    | length x*2 == checkSig x = 0

 fromInteger x = M [[fromInteger x]]

 negate (M x)
    | matrixtyp (M x) == KeineMatrix = M []
    | otherwise = (M (combineNegatives x))

 abs (M x)
    | matrixtyp (M x) == KeineMatrix = M []
    | otherwise = (M (createAbs x))

checkSig :: [Zeile] -> Int
checkSig [] = 0
checkSig (x:xs)
    | all(<0) x = 0+checkSig xs
    | all(>0) x = 1+checkSig xs
    | all(==0) x = 2+checkSig xs
    | otherwise = error "Vorzeichenfunktion undefiniert"

checkBothMatrix :: (Matrix, Matrix) -> Bool
checkBothMatrix ((M x),(M y))
    | (matrixtyp (M x) == matrixtyp (M y)) && (matrixtyp (M x) /= KeineMatrix && matrixtyp (M y) /= KeineMatrix) = True
    | otherwise = False

add :: Num a => [[a]]-> [[a]] -> [[a]]
add m n = zipWith (zipWith (+)) m n

sub :: Num a => [[a]]-> [[a]] -> [[a]]
sub m n = zipWith (zipWith (-)) m n

mmult :: Num a => [[a]] -> [[a]] -> [[a]]
mmult m n = [ [ sum $ zipWith (*) ar bc | bc <- (transpose n) ] | ar <- m ]

getColumns :: Matrix -> Int
getColumns (M x) = length x

getRows :: Matrix -> Int
getRows (M x) = length (head x)

combineNegatives :: [Zeile] -> [Zeile]
combineNegatives [] = []
combineNegatives (x:xs) = [multEveryElement x] ++ combineNegatives xs

multEveryElement :: [Int] -> [Int]
multEveryElement = foldr (\ x -> (++) [x * (- 1)]) []

createAbs :: [Zeile] -> [Zeile]
createAbs [] = []
createAbs (x:xs)
    | all(>=0) x = [x] ++ combineNegatives xs
    | otherwise = [multEveryElementIfNegative x] ++ combineNegatives xs

multEveryElementIfNegative :: [Int] -> [Int]
multEveryElementIfNegative [] = []
multEveryElementIfNegative (x:xs)
    | x >= 0 = [x]++multEveryElementIfNegative xs
    | otherwise = [x * (- 1)]++multEveryElementIfNegative xs

-- Knapp, aber gut nachvollziebar geht die Instanzdeklaration fuer Num folgendermassen vor: 