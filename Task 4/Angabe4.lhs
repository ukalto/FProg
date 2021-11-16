> module Angabe4 where
> import Data.List
> 
> 
> {-# ANN module "HLint: ignore Use camelCase" #-}
> 
> 
> -- 1. Vervollstaendigen Sie gemaess Angabentext!
> -- 2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
> -- 3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
> -- 4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat! 
> 
> -- Datenstrukturen fuer eine einfache Buchhaltung:
> 
> type Nat1    = Int
> type Name    = String
> newtype Cent = C {cents :: Nat1} deriving (Eq,Ord,Show)
> type Brutto  = Cent
> type Netto   = Cent
> data Skonto  = KeinSkonto | DreiProzent | FuenfProzent | ZehnProzent deriving (Eq,Ord,Show)
> data Tag     = I | II | III | IV | V | VI | VII | VIII | IX | X
>                | XI | XII | XIII | XIV | XV | XVI | XVII | XVIII 
>                | XIX | XX | XXI | XXII | XXIII | XXIV | XXV
>                | XXVI | XXVII | XXVIII | XXIX | XXX 
>                | XXXI deriving (Eq,Ord,Show,Enum)
> data Monat   = Jan | Feb | Mar | Apr | Mai | Jun | Jul | Aug
>                | Sep | Okt | Nov | Dez deriving (Eq,Ord,Show,Enum)
> type Jahr    = Nat1
> data Datum   = D {tag  :: Tag, monat :: Monat, jahr  :: Jahr} deriving (Eq,Show)
> data Geschaeftspartner = GP { partner :: Name, seit :: Datum} deriving (Eq,Ord,Show)
> data Geschaeftsvorfall = Zahlung { brutto :: Brutto, skonto :: Skonto, zahlung_vom :: Datum}
>                          | Gutschrift { gutschriftsbetrag :: Cent, gutschrift_vom :: Datum} deriving (Eq, Ord,Show)
> type Kassabucheintrag = (Geschaeftspartner,Geschaeftsvorfall)
> newtype Kassabuch = KB [Kassabucheintrag] deriving (Eq,Show, Ord)
> 
> 
> -- Aufgabe A.1
> 
> type P_Geschaeftspartner = Geschaeftspartner
> data AP_Geschaeftsvorfall = AP_Zahlung { netto :: Netto, zahlungsdatum :: Datum}
>                           | P_Gutschrift { gutschrift :: Cent, gutschriftsdatum :: Datum} deriving (Eq,Show)   
> type AP_Kassabucheintrag = (P_Geschaeftspartner,AP_Geschaeftsvorfall)
> 
> waup :: Kassabucheintrag -> AP_Kassabucheintrag
> waup ((GP name pdate), (Zahlung brutto KeinSkonto payedOn)) = (GP name (getDate pdate), (AP_Zahlung brutto (getDate payedOn)))
> waup ((GP name pdate), (Zahlung (C cents) DreiProzent payedOn)) = (GP name (getDate pdate), (AP_Zahlung (C (cents - (floor((fromIntegral cents)*0.03)))) (getDate payedOn)))
> waup ((GP name pdate), (Zahlung (C cents) FuenfProzent payedOn)) = (GP name (getDate pdate), (AP_Zahlung (C (cents - (floor((fromIntegral cents)*0.05)))) (getDate payedOn)))
> waup ((GP name pdate), (Zahlung (C cents) ZehnProzent payedOn)) = (GP name (getDate pdate), (AP_Zahlung (C (cents - (floor((fromIntegral cents)*0.1)))) (getDate payedOn)))
> waup ((GP name pdate), (Gutschrift gutschrift payedOn)) = (GP name (getDate pdate), (P_Gutschrift gutschrift (getDate payedOn)))
> 
> getDate :: Datum -> Datum
> getDate (D day month year) =
>     if(validateDate((D day month year)) == True)
>         then (D day month year)
>         else (D I (succ month) year)
> 
> validateDate :: Datum -> Bool
> validateDate (D day month year) =
>    if(month == Apr || month == Jun || month == Sep || month == Nov) && day > XXX -- more than 30 days
>       then False
>       else if((year < 100 && year`mod`4 == 0) && (month == Feb && day <= XXIX)) 
>          then True
>          else if((year`mod`100 == 0 && year`mod`400 == 0) && (month == Feb && day <= XXIX)) -- Leapyear Feb more than 29 days
>             then True
>             else if(month == Feb && day > XXVIII) -- Feb more than 28 days
>                then False
>                else True
> 
> -- Knapp, aber gut nachvollziehbar geht waup folgendermassen vor: 
> 
> -- Aufgabe A.2
> 
> -- Nur Werte zwischen 0 und 99 fuer cent!
> data EuroCent = EC {euro :: Nat1,cent :: Nat1} deriving (Eq,Ord,Show)
> data K_Geschaeftsvorfall = K_Zahlung { ec_netto :: EuroCent,zahlungsdatum' :: Datum}
>                            | K_Gutschrift {ec_gutschrift :: EuroCent, gutschriftsdatum' :: Datum} deriving (Eq,Show)        
> 
> type K_Kassabucheintrag = (P_Geschaeftspartner,K_Geschaeftsvorfall)
> newtype KonsolidiertesKassabuch = KKB [(P_Geschaeftspartner,K_Geschaeftsvorfall)] deriving (Eq,Show)   
> 
> konsolidiere :: Kassabuch -> KonsolidiertesKassabuch
> konsolidiere (KB k) = KKB (konsolidiereHelp k)
> 
> konsolidiereHelp :: [Kassabucheintrag] -> [(P_Geschaeftspartner,K_Geschaeftsvorfall)] 
> konsolidiereHelp [] = []
> konsolidiereHelp ((gp, gv):xs) = [((getGeschaeftspartner gp), (getGeschaeftsvorfall gv))]  ++ konsolidiereHelp xs
> 
> getGeschaeftspartner :: Geschaeftspartner -> P_Geschaeftspartner
> getGeschaeftspartner (GP name pdate) = (GP name (getDate pdate))
> 
> getGeschaeftspartner2 :: Geschaeftspartner -> Geschaeftspartner
> getGeschaeftspartner2 (GP name pdate) = (GP name (getDate pdate))
> 
> getGeschaeftsvorfall :: Geschaeftsvorfall -> K_Geschaeftsvorfall
> getGeschaeftsvorfall ((Zahlung brutto KeinSkonto payedOn)) = (K_Zahlung (getEuroCent brutto) (getDate payedOn))
> getGeschaeftsvorfall ((Zahlung (C cents) DreiProzent payedOn)) = (K_Zahlung (getEuroCent (C (cents - (floor((fromIntegral cents)*0.03))))) (getDate payedOn))
> getGeschaeftsvorfall ((Zahlung (C cents) FuenfProzent payedOn)) = (K_Zahlung (getEuroCent (C (cents - (floor((fromIntegral cents)*0.05))))) (getDate payedOn))
> getGeschaeftsvorfall ((Zahlung (C cents) ZehnProzent payedOn)) = (K_Zahlung (getEuroCent (C (cents - (floor((fromIntegral cents)*0.1))))) (getDate payedOn))
> getGeschaeftsvorfall ((Gutschrift gutschrift payedOn)) = (K_Gutschrift (getEuroCent gutschrift) (getDate payedOn))
> 
> getEuroCent :: Cent -> EuroCent
> getEuroCent (C cent) = (EC (floor ((fromIntegral cent)/100)) (cent`mod`100))
> 
> -- Knapp, aber gut nachvollziehbar geht konsolidiere folgendermassen vor:
> 
> -- Aufgabe A.3
> 
> data Saldo = Forderungssaldo { fs :: EuroCent }
>              | Zahlungssaldo { zs :: EuroCent }
>              | Ausgeglichen
>              | Keine_Geschaeftsbeziehung deriving (Eq,Show)
>                   
> saldo :: P_Geschaeftspartner -> KonsolidiertesKassabuch -> Saldo
> saldo gp (KKB kkb) 
>    | (checkGeschaeftspartner gp kkb) == False = Keine_Geschaeftsbeziehung
>    | getDifferenz gp kkb < 0 = Forderungssaldo (getEuroCent (C ((getDifferenz gp kkb)*(-1))))
>    | getDifferenz gp kkb > 0 = Zahlungssaldo (getEuroCent (C (getDifferenz gp kkb)))
>    | getDifferenz gp kkb == 0 = Ausgeglichen
> 
> getDifferenz :: P_Geschaeftspartner -> [(P_Geschaeftspartner,K_Geschaeftsvorfall)] -> Nat1
> getDifferenz gp [] = 0
> getDifferenz gp ((kgp, _):xs)
>    | gp /= kgp = 0 + getDifferenz gp xs
> getDifferenz gp ((kgp, (K_Zahlung ec_netto _)):xs)
>    | gp == kgp = (euroCentToCent ec_netto) + getDifferenz gp xs
> getDifferenz gp ((kgp, (K_Gutschrift ec_gutschrift _)):xs)
>    | gp == kgp = getDifferenz gp xs - (euroCentToCent ec_gutschrift)
> 
> euroCentToCent :: EuroCent -> Nat1
> euroCentToCent (EC euro cent) = ((euro*100)+cent)
> 
> checkGeschaeftspartner :: P_Geschaeftspartner -> [(P_Geschaeftspartner,K_Geschaeftsvorfall)]  -> Bool 
> checkGeschaeftspartner gp [] = False 
> checkGeschaeftspartner gp ((kkbgp, _):xs)
>    | getGeschaeftspartner gp == kkbgp = True 
>    | otherwise = checkGeschaeftspartner gp xs
> 
> -- Knapp, aber gut nachvollziehbar geht saldo folgendermassen vor: 
> 
> -- Aufgabe A.4
> 
> newtype SaldiertesKassabuch = SKB [(Geschaeftspartner,Saldo)] deriving (Eq,Show)
> 
> saldiere :: Kassabuch -> SaldiertesKassabuch
> saldiere kb = SKB (saldiereArray ((getSortedPartner kb), kb))
> 
> saldiereArray :: ([Geschaeftspartner],Kassabuch) -> [(Geschaeftspartner,Saldo)]
> saldiereArray ([],_) = []
> saldiereArray (x:xs, KB kb) = [(saldierePartner x (konsolidiere (KB kb)))] ++ saldiereArray (xs, (KB kb))
> 
> saldierePartner :: Geschaeftspartner -> KonsolidiertesKassabuch -> (Geschaeftspartner, Saldo)
> saldierePartner gp kkb = (gp, (saldo gp kkb))
> 
> getSortedPartner :: Kassabuch -> [Geschaeftspartner]
> getSortedPartner kb = (removeDuplicates(getPartner (unPackKonsolidiertesKassabuch (createSortedKonsoldiertesKassabuch kb))))
>  
> removeDuplicates :: [Geschaeftspartner] -> [Geschaeftspartner]
> removeDuplicates [] = []
> removeDuplicates (x:xs)
>    | x `elem` xs = removeDuplicates xs
>    | otherwise = x : removeDuplicates xs
> 
> getPartner :: [(P_Geschaeftspartner,K_Geschaeftsvorfall)] -> [Geschaeftspartner]
> getPartner [] = [] 
> getPartner ((gp,_):xs) = [gp] ++ getPartner xs
> 
> unPackKonsolidiertesKassabuch :: KonsolidiertesKassabuch -> [(P_Geschaeftspartner,K_Geschaeftsvorfall)]
> unPackKonsolidiertesKassabuch (KKB kkb) = kkb
> 
> createSortedKonsoldiertesKassabuch :: Kassabuch -> KonsolidiertesKassabuch
> createSortedKonsoldiertesKassabuch (KB kb) = KKB (konsolidiereHelp (quicksort kb))
> 
> quicksort :: (Ord a) => [a] -> [a]
> quicksort [] = []
> quicksort (x:xs) =
>   let smallerSorted = quicksort [a | a <- xs, a <= x]
>       biggerSorted = quicksort [a | a <- xs, a > x]
>   in  smallerSorted ++ [x] ++ biggerSorted
> 
> instance Ord Datum where
>   (D x1 y1 z1) <= (D x2 y2 z2)
>      | z1 < z2 = True
>      | z1 == z2 && y1 < y2 = True
>      | z1 == z2 && y1 == y2 && x1 < x2 = True
>      | z1 == z2 && y1 == y2 && x1 == x2 = True
>      | otherwise = False
> 
> -- Knapp, aber gut nachvollziehbar geht saldiere folgendermassen vor: 