module Angabe2 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}


-- Aufgabe A.1

-- Ergaenzen Sie fehlende Typklassen in deriving-Klauseln, wo noetig und nicht explizit
-- eine Instanz-Deklaration gefordert ist.


type Nat1              = Int
newtype Vorname        = Vorname String deriving Show
newtype Nachname       = Nachname String deriving Show
data VHDS              = Viertel | Halb | Dreiviertel | Schlag deriving (Eq,Ord)
data Stunde            = Eins | Zwei | Drei | Vier | Fuenf | Sechs
                         | Sieben | Acht | Neun | Zehn | Elf 
                         | Zwoelf deriving (Eq,Ord)
data VorNachMittag     = VM | NM deriving (Eq,Ord)
newtype Uhrzeit        = U (VHDS,Stunde,VorNachMittag) deriving (Eq,Ord)
data Tag               = I | II | III | IV | V | VI | VII | VIII | IX | X
                         | XI | XII | XIII | XIV | XV | XVI | XVII | XVIII 
                         | XIX | XX | XXI | XXII | XXIII | XXIV | XXV
                         | XXVI | XXVII | XXVIII | XXIX | XXX 
                         | XXXI deriving (Eq,Ord)
data Monat             = Jan | Feb | Mar | Apr | Mai | Jun 
                         | Jul | Aug | Sep | Okt | Nov | Dez deriving (Eq,Ord)
type Jahr              = Nat1
data Datum             = D Tag Monat Jahr deriving Eq
data Testart           = PCR | Antigen deriving Eq
data Impfstoff         = AstraZeneca | BioNTec | JundJ | Moderna 
                         | Sputnik | Sinovac deriving (Eq,Show)
data Anzahl            = Einmal | Zweimal deriving Eq
data DreiG_Status      = Geimpft (Impfstoff,Anzahl) | Genesen 
                         | Getestet Testart Datum Uhrzeit 
                         | Udrei deriving (Eq,Show)
                           -- Udrei: Ungetestet, Ungenesen, Ungeimpft
data Regel             = DreiG | ZweieinhalbG | ZweiG deriving Eq
data Person            = P Vorname Nachname DreiG_Status deriving (Eq,Ord)
type Einlassbegehrende = [Person]
type VorUndNachname    = String
type Einzulassende     = [VorUndNachname]
type Abzuweisende      = [VorUndNachname]
type Kontrollzeitpunkt = (Datum,Uhrzeit)
data Kontrollergebnis  = Einlassen | Abweisen | Ungueltig deriving (Eq,Show)


-- Aufgabe A.2

einzulassen :: (Person,Regel,Kontrollzeitpunkt) -> Kontrollergebnis


{- Knapp, aber gut nachvollziehbar geht einzulassen folgendermassen vor:
   ...
-}

-- Aufgabe A.3

einzulassende :: Einlassbegehrende -> Regel -> Kontrollzeitpunkt -> Einzulassende


{- Knapp, aber gut nachvollziehbar geht einzulassende folgendermassen vor: 
   ... 
-}

-- Aufgabe A.4

einzulassende_abzuweisende :: Einlassbegehrende -> Regel -> Kontrollzeitpunkt -> (Einzulassende,Abzuweisende)

{- Knapp, aber gut nachvollziehbar geht einzulassende_abzuweisende folgendermassen vor: 
   ... 
-}


-- Aufgabe A.5

instance Show Uhrzeit where
 show ...

{- Knapp, aber gut nachvollziehbar geht die Implementierung von show fuer Uhrzeit 
   folgendermassen vor:
   ...
-}


instance Show Datum where
 show ...

{- Knapp, aber gut nachvollziehbar geht die Implementierung von show fuer Datum 
   folgendermassen vor:
   ...
-}

-- Aufgabe A.5

instance Show Uhrzeit where
 show ...


instance Show Datum where
 show ...

{- Knapp, aber gut nachvollziehbar gehen die Implementierungen von show folgendermassen vor: 
   ... 
-}




