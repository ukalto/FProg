module Angabe7 where


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

data Matrixtyp = Mat (Nat1,Nat1) | KeineMatrix deriving (Eq,Show)


instance Show Matrix where
 show (M zs) = error "Nicht implementiert!"


matrixtyp :: Matrix -> Matrixtyp
matrixtyp (M zs) = error "Nicht implementiert!"

 
instance Eq Matrix where
 (M zs1) == (M zs2) = error "Nicht implementiert!"


instance Num Matrix where
 (M zs1) + (M zs2) = error "Nicht implementiert!"
 (M zs1) - (M zs2) = error "Nicht implementiert!"
 (M zs1) * (M zs2) = error "Nicht implementiert!"
 negate (M zs)     = error "Nicht implementiert!"
 abs (M zs)        = error "Nicht implementiert!"
 signum (M zs)     = error "Nicht implementiert!"
 fromInteger       = error "Nicht implementiert!"


-- Von Angabe 6 wiederholt:

type Zeilenzahl  = Nat1
type Spaltenzahl = Nat1
type Zeile       = Nat1
type Spalte      = Nat1
type Skalar      = Int

-- Umbenennung 2: Matrixtyp von Angabe 6 wird fuer Angabe 7 umbenannt zu Matrixtyp'
type Matrixtyp'  = (Zeilenzahl,Spaltenzahl)
type Matrixfkt   = Zeile -> Spalte -> Skalar  -- ausschliessl. total def. Abb.!

-- Matrizenwerte als Typ und funktionale Darstellung
data MatrixF = Mf { mtyp :: Matrixtyp', mf :: Matrixfkt }

-- Namesvereinbarung fuer den Fehlerwert des Typs MatrixF
fehler = Mf (0,0) (\_ _ -> 0) :: MatrixF


instance Show MatrixF where
 show (Mf t f) = error "Nicht implementiert!"

-- Umbenennung 3: matrixtyp von Angabe 6 wird fuer Angabe 7 umbenannt zu matrixtyp'
matrixtyp' :: MatrixF -> Maybe Matrixtyp'
matrixtyp' (Mf t f) = error "Nicht implementiert!"

instance Eq MatrixF where
 (Mf t1 f1) == (Mf t2 f2) = error "Nicht implementiert!"

instance Num MatrixF where
 (Mf t1 f1) + (Mf t2 f2) = error "Nicht implementiert!"
 (Mf t1 f1) - (Mf t2 f2) = error "Nicht implementiert!"
 (Mf t1 f1) * (Mf t2 f2) = error "Nicht implementiert!"
 negate (Mf t f)         = error "Nicht implementiert!"
 abs (Mf t f)            = error "Nicht implementiert!"
 signum (Mf t f)         = error "Nicht implementiert!"
 fromInteger n           = error "Nicht implementiert!"



-- Aufgabe A.2

class (Eq a,Num a,Show a) => MatrixTyp a where
 madd, msub, mmult :: a -> a -> a
 msmult  :: Int -> a -> a
 mtransp :: a -> a
 mdet    :: a -> Maybe Int
 mfehler :: a

 -- Protoimplementierungen
 madd  = (+)
 mmult = (*)
 msub  = (-)


instance MatrixTyp Matrix where
 msmult n (M zs) = error "Nicht implementiert!"
 mdet (M zs)     = error "Nicht implementiert!"

instance MatrixTyp MatrixF where
 msmult n (Mf t f) = error "Nicht implementiert!"
 mdet (Mf t f)     = error "Nicht implementiert!"


{- Knapp, aber gut nachvollziehbar gehen die Instanzbildungen fuer 
   MatrixTyp folgendermassen vor:
   ...
-}



-- Aufgabe A.3

konv1 :: Matrix -> MatrixF
konv1 (M zs) = error "Nicht implementiert!"

konv2 :: MatrixF -> Matrix
konv2 (Mf t f) = error "Nicht implementiert!"

{- Knapp, aber gut nachvollziehbar gehen die Konvertierungsfunktionen 
   konv1 und konv2 folgendermassen vor:
   ...
-}



-- Aufgabe A.6


type Konservenzahl = Nat0

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
konservenrechner fp ab zb = error "Nicht implementiert!"

{- Knapp, aber gut nachvollziehbar geht die Funktion konservenrechner 
   folgendermassen vor:
   ...
-}
