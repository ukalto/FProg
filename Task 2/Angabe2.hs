module Angabe2 where

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: refact:Redundant if" #-}

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}


-- Aufgabe A.1

-- Ergaenzen Sie fehlende Typklassen in deriving-Klauseln, wo noetig und nicht explizit
-- eine Instanz-Deklaration gefordert ist.


type Nat1              = Int
newtype Vorname        = Vorname String deriving (Eq,Ord,Show)
newtype Nachname       = Nachname String deriving (Eq,Ord,Show)
data VHDS              = Viertel | Halb | Dreiviertel | Schlag deriving (Eq,Ord,Show,Enum)
data Stunde            = Eins | Zwei | Drei | Vier | Fuenf | Sechs
                         | Sieben | Acht | Neun | Zehn | Elf
                         | Zwoelf deriving (Eq,Ord,Show,Enum)
data VorNachMittag     = VM | NM deriving (Eq,Ord,Show,Enum)
newtype Uhrzeit        = U (VHDS,Stunde,VorNachMittag) deriving (Eq)
data Tag               = I | II | III | IV | V | VI | VII | VIII | IX | X
                         | XI | XII | XIII | XIV | XV | XVI | XVII | XVIII
                         | XIX | XX | XXI | XXII | XXIII | XXIV | XXV
                         | XXVI | XXVII | XXVIII | XXIX | XXX
                         | XXXI deriving (Eq,Ord,Show,Enum)
data Monat             = Jan | Feb | Mar | Apr | Mai | Jun
                         | Jul | Aug | Sep | Okt | Nov | Dez deriving (Eq,Ord,Show,Enum)
type Jahr              = Nat1
data Datum             = D Tag Monat Jahr deriving (Eq)
data Testart           = PCR | Antigen deriving (Eq,Ord,Show)
data Impfstoff         = AstraZeneca | BioNTec | JundJ | Moderna
                         | Sputnik | Sinovac deriving (Eq,Ord,Show)
data Anzahl            = Einmal | Zweimal deriving (Eq,Ord,Show)
data DreiG_Status      = Geimpft (Impfstoff, Anzahl) | Genesen
                         | Getestet Testart Datum Uhrzeit
                         | Udrei deriving (Eq,Ord)
                           -- Udrei: Ungetestet, Ungenesen, Ungeimpft
data Regel             = DreiG | ZweieinhalbG | ZweiG deriving Eq
data Person            = P Vorname Nachname DreiG_Status deriving (Eq,Ord)
type Einlassbegehrende = [Person]
type VorUndNachname    = String
type Einzulassende     = [VorUndNachname]
type Abzuweisende      = [VorUndNachname]
type Kontrollzeitpunkt = (Datum,Uhrzeit)
data Kontrollergebnis  = Einlassen | Abweisen | Ungueltig deriving (Eq,Show)

mickey = P (Vorname "Mickey") (Nachname "Mouse")
-- timez
xmas20 :: Kontrollzeitpunkt
xmas20 = (D XXIV Dez 20, U (Schlag, Sechs, NM))

-- Aufgabe A.2
instance Ord Uhrzeit where
  (U (x1,y1,z1)) <= (U (x2,y2,z2))
     | z1 < z2 = True
     | z1 == z2 && y1 < y2 = True
     | z1 == z2 && y1 == y2 && x1 < x2 = True
     | z1 == z2 && y1 == y2 && x1 == x2 = True
     | otherwise = False

instance Ord Datum where
  (D x1 y1 z1) <= (D x2 y2 z2)
     | z1 < z2 = True
     | z1 == z2 && y1 < y2 = True
     | z1 == z2 && y1 == y2 && x1 < x2 = True
     | z1 == z2 && y1 == y2 && x1 == x2 = True
     | otherwise = False

--instance Show Vorname where
--  v = v

einzulassen :: (Person,Regel,Kontrollzeitpunkt) -> Kontrollergebnis
einzulassen (P _ _ dg,r,k)
   | dg == Udrei = Abweisen
   | r == DreiG = checkDreiG(dg,k)
   | r == ZweieinhalbG = checkZweiEinHalbG(dg,k)
   | r == ZweiG = checkZweiG(dg,k)

checkDreiG :: (DreiG_Status, Kontrollzeitpunkt) -> Kontrollergebnis
checkDreiG (_,(kDat,kUhr))
   | validateDate(kDat) == False = Ungueltig
checkDreiG (Genesen,k) = Einlassen
checkDreiG (Geimpft (Sputnik, _), _) = Abweisen
checkDreiG (Geimpft (JundJ,Einmal), k) = Einlassen
checkDreiG (Geimpft (i,Zweimal), k) = Einlassen
checkDreiG (Getestet PCR tDat tUhr, (kDat,kUhr)) = checkTest(PCR,tDat,kDat,tUhr,kUhr)
checkDreiG (Getestet Antigen tDat tUhr, (kDat,kUhr)) = checkTest(Antigen,tDat,kDat,tUhr,kUhr)
checkDreiG (_, k) = Abweisen

checkZweiEinHalbG :: (DreiG_Status, Kontrollzeitpunkt) -> Kontrollergebnis
checkZweiEinHalbG (_,(kDat,kUhr))
   | validateDate(kDat) == False = Ungueltig
checkZweiEinHalbG (Genesen,k) = Einlassen
checkZweiEinHalbG (Geimpft (Sputnik, _), _) = Abweisen
checkZweiEinHalbG (Geimpft (JundJ,Einmal), k) = Einlassen
checkZweiEinHalbG (Geimpft (i,Zweimal), k) = Einlassen
checkZweiEinHalbG (Getestet PCR tDat tUhr, (kDat,kUhr)) = checkTest(PCR,tDat,kDat,tUhr,kUhr)
checkZweiEinHalbG (_, k) = Abweisen

checkZweiG :: (DreiG_Status, Kontrollzeitpunkt) -> Kontrollergebnis
checkZweiG (_,(kDat,kUhr))
   | validateDate(kDat) == False = Ungueltig
checkZweiG (Genesen,k) = Einlassen
checkZweiG (Geimpft (Sputnik, _), _) = Abweisen
checkZweiG (Geimpft (JundJ,Einmal), k) = Einlassen
checkZweiG (Geimpft (i,Zweimal), k) = Einlassen
checkZweiG (_, k) = Abweisen

checkTest :: (Testart, Datum, Datum, Uhrzeit, Uhrzeit) -> Kontrollergebnis
checkTest(t, tDate, kDate, tTime, kTime)
   | (validateDate(tDate) == False || validateDate(kDate) == False) = Ungueltig
   | (t == PCR && checkDateTimeTest(tDate, kDate, tTime, kTime,3) == True) = Einlassen
   | (t == Antigen && checkDateTimeTest(tDate, kDate, tTime, kTime,1) == True) = Einlassen
   | otherwise = Abweisen

calcHour :: (Stunde, VorNachMittag) -> Int
calcHour (hour, vnm) =
   if(vnm == VM)
      then fromEnum(hour)+1
      else fromEnum(hour)+13

calcMin :: VHDS -> Int
calcMin vhds =
   if(fromEnum(vhds) == 0)
      then 15
      else if(fromEnum(vhds) == 1)
         then 30
         else if(fromEnum(vhds) == 2)
            then 45
            else 60

giveMonthDays :: (Monat,Jahr) -> Int
giveMonthDays (month,year) =
   if(month == Apr || month == Jun || month == Sep || month == Nov) -- 30 days
         then 30
         else if(year`mod`4 == 0 || (year`mod`100 == 0 && year`mod`400 == 0)) && month == Feb -- 29 days
            then 29
            else if(month == Feb) -- 28 days
               then 28
               else 31 -- 31 days

-- 31 = Jan, Mar, Mai, Jul, Aug, Okt, Dez
-- 30 = Apr, Jun, Sep, Nov
-- 28/29 = Feb

validateDate :: Datum -> Bool
validateDate(D day month year) =
   if(month == Apr || month == Jun || month == Sep || month == Nov) && day > XXX -- more than 30 days
      then False
      else if((year < 100 && year`mod`4 == 0) && (month == Feb && day <= XXIX)) 
         then True 
         else if((year`mod`100 == 0 && year`mod`400 == 0) && (month == Feb && day <= XXIX)) -- Leapyear Feb more than 29 days
            then True
            else if(month == Feb && day > XXVIII) -- Feb more than 28 days
               then False
               else True

validateTime :: (Uhrzeit, Uhrzeit, Int, Int) -> Bool
validateTime (U (tvhds, thour, tvnm), U (kvhds, khour, kvnm), dayDiff, gueltigkeitsTage) =
   do
      let tStunden = calcHour(thour,tvnm) + 24*gueltigkeitsTage
      let kStunden = calcHour(khour,kvnm) + 24*dayDiff
      if(kStunden < tStunden)
         then True
         else if(kStunden == tStunden)
            then if(kvhds<=tvhds)
               then True
               else False
         else False

checkDateTimeTest :: (Datum, Datum, Uhrzeit, Uhrzeit, Int) -> Bool
checkDateTimeTest(D tDay tMonth tYear, D kDay kMonth kYear, tTime, kTime, gueltigkeitsTage) =
   do
      if(tYear == kYear) -- same year 
         then if(tMonth == kMonth) -- same month
            then if(fromEnum(kDay) <= ((fromEnum(tDay)+gueltigkeitsTage)) && ((fromEnum(tDay)+gueltigkeitsTage)-fromEnum(kDay) <= gueltigkeitsTage)) -- innerhalb der 3 Tage
               then if(validateTime(tTime, kTime, fromEnum(kDay)-fromEnum(tDay), gueltigkeitsTage) == True)
                  then True
                  else False
               else False
            else if(((fromEnum(kMonth)-fromEnum(tMonth)) == 1)) -- different month
               then if((giveMonthDays(tMonth,tYear)+fromEnum(kDay)) <= (fromEnum(tDay)+gueltigkeitsTage))
                  then if(validateTime(tTime, kTime, fromEnum(kDay)+1, gueltigkeitsTage) == True)
                     then True
                     else False
                  else False
               else False
         else if((kYear-tYear) == 1 && (tMonth == Dez && kMonth == Jan) && tDay >= XXX) --different year
            then if((giveMonthDays(kMonth,kYear)+fromEnum(kDay)) <= (giveMonthDays(tMonth,tYear)+gueltigkeitsTage))
               then if(validateTime(tTime, kTime, fromEnum(kDay)+1, gueltigkeitsTage))
                  then True
                  else False
               else False
            else False


{- Knapp, aber gut nachvollziehbar geht einzulassen folgendermassen vor:
   ...
-}

-- Aufgabe A.3
fullName :: Person -> VorUndNachname
fullName (P (Vorname vn) (Nachname nn) _) = vn ++ " " ++ nn

einzulassende :: Einlassbegehrende -> Regel -> Kontrollzeitpunkt -> Einzulassende
einzulassende [] r k = []
einzulassende (e:es) r k =
  if (einzulassen(e,r,k) == Einlassen)
      then collapseArray ([fullName e], (einzulassende es r k))
      else collapseArray([], (einzulassende es r k))



{- Knapp, aber gut nachvollziehbar geht einzulassende folgendermassen vor: 
   ... 
-}

-- Aufgabe A.4

collapseArray :: (Einzulassende,Einzulassende) -> Einzulassende
collapseArray (e1,e2) = e1 ++ e2

abzuweisende :: Einlassbegehrende -> Regel -> Kontrollzeitpunkt -> Einzulassende
abzuweisende [] r k = []
abzuweisende (e:es) r k =
  if (einzulassen(e,r,k) == Abweisen)
      then collapseArray ([fullName e], (abzuweisende es r k))
      else collapseArray([], (abzuweisende es r k))

einzulassende_abzuweisende :: Einlassbegehrende -> Regel -> Kontrollzeitpunkt -> (Einzulassende,Abzuweisende)
einzulassende_abzuweisende e r k = (einzulassende e r k, abzuweisende e r k)

{- Knapp, aber gut nachvollziehbar geht einzulassende_abzuweisende folgendermassen vor: 
   ... 
-}

-- Aufgabe A.5
calcHourString :: (Stunde, VorNachMittag) -> String
calcHourString (hour, vnm) =
   if(vnm == VM)
      then if(fromEnum(hour)+1 < 10)
         then "0"++show(fromEnum(hour))
         else show(fromEnum(hour))
      else show(fromEnum(hour)+12)

instance Show Uhrzeit where
   show (U (Schlag, hour, vnm)) = show(calcHour(hour,vnm))++":00 Uhr"
   show (U (vhds, hour, vnm)) = calcHourString(hour, vnm)++":"++show(calcMin(vhds))++" Uhr"



instance Show Datum where
   show (D day month year) =
      if(validateDate (D day month year) == True)
         then show(fromEnum(day)+1)++"."++show(fromEnum(month)+1)++"."++show(year)
         else "Datum ungueltig"

{- Knapp, aber gut nachvollziehbar gehen die Implementierungen von show folgendermassen vor: 
   ... 
-}