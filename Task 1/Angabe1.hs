module Angabe1 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}

type Nat0 = Int

type Zeichenreihe = String

type Teilzeichenreihe = String

type IstTeilzeichenreihe = Bool

type Zerlegungszeuge = (Zeichenreihe, Zeichenreihe, Zeichenreihe)

type Zerlegungszeugen = [Zerlegungszeuge]

-- Aufgabe A.1

ist_tzr :: Zeichenreihe -> Teilzeichenreihe -> IstTeilzeichenreihe
ist_tzr zeichenreihe teilzeichenreihe
  | length(zeichenreihe) < length (teilzeichenreihe) = False
  | zeichenreihe == "" = False
  | checkSim zeichenreihe teilzeichenreihe = True
  | not (checkSim zeichenreihe teilzeichenreihe) = ist_tzr (tail zeichenreihe) teilzeichenreihe
  | otherwise = False

checkSim :: Zeichenreihe -> Teilzeichenreihe -> IstTeilzeichenreihe
checkSim zeichenreihe teilzeichenreihe
  | teilzeichenreihe == "" = True
  | zeichenreihe == "" = False
  | head zeichenreihe == head teilzeichenreihe = checkSim (tail zeichenreihe) (tail teilzeichenreihe)
  | head zeichenreihe /= head teilzeichenreihe = False

{- Knapp, aber gut nachvollziehbar geht ist_tzr folgendermassen vor:
   ...
-}

-- Aufgabe A.2

tzr_zeuge :: Zeichenreihe -> Teilzeichenreihe -> Zerlegungszeuge
tzr_zeuge zeichenreihe teilzeichenreihe
  | ist_tzr zeichenreihe teilzeichenreihe = (getTeilzeichenreiheVorSim zeichenreihe teilzeichenreihe, teilzeichenreihe, getTeilzeichenreiheNachSim zeichenreihe teilzeichenreihe)
  | not (ist_tzr zeichenreihe teilzeichenreihe) = ("", teilzeichenreihe ++ teilzeichenreihe, "")

deleteTeilzeichenreihe :: Zeichenreihe -> Teilzeichenreihe -> Teilzeichenreihe
deleteTeilzeichenreihe zeichenreihe teilzeichenreihe
  | teilzeichenreihe == "" = zeichenreihe
  | head zeichenreihe == head teilzeichenreihe = deleteTeilzeichenreihe (tail zeichenreihe) (tail teilzeichenreihe)

getTeilzeichenreiheVorSim :: Zeichenreihe -> Teilzeichenreihe -> Teilzeichenreihe
getTeilzeichenreiheVorSim zeichenreihe teilzeichenreihe
  | zeichenreihe == "" = ""
  | checkSim zeichenreihe teilzeichenreihe = ""
  | not (checkSim zeichenreihe teilzeichenreihe) = [head zeichenreihe] ++ (getTeilzeichenreiheVorSim (tail zeichenreihe) teilzeichenreihe)

getTeilzeichenreiheNachSim :: Zeichenreihe -> Teilzeichenreihe -> Teilzeichenreihe
getTeilzeichenreiheNachSim zeichenreihe teilzeichenreihe
  | zeichenreihe == "" = ""
  | checkSim zeichenreihe teilzeichenreihe = deleteTeilzeichenreihe zeichenreihe teilzeichenreihe
  | not (checkSim zeichenreihe teilzeichenreihe) = getTeilzeichenreiheNachSim (tail zeichenreihe) teilzeichenreihe

{- Knapp, aber gut nachvollziehbar geht tzr_zeuge folgendermassen vor:
   ...
-}

-- Aufgabe A.3

tzr_zeugen :: Zeichenreihe -> Teilzeichenreihe -> Zerlegungszeugen
tzr_zeugen zeichenreihe teilzeichenreihe
  | countTeil zeichenreihe teilzeichenreihe > 1 = createCharArray zeichenreihe teilzeichenreihe ""
  | ist_tzr zeichenreihe teilzeichenreihe = [tzr_zeuge zeichenreihe teilzeichenreihe]
  | not (ist_tzr zeichenreihe teilzeichenreihe) = []

collapseArray :: Zerlegungszeugen -> Zerlegungszeugen -> Zerlegungszeugen
collapseArray zer1 zer2 = zer1 ++ zer2

createCharArray :: Zeichenreihe -> Teilzeichenreihe -> Teilzeichenreihe -> Zerlegungszeugen
createCharArray z t textDavor
   | z == "" && t == "" = [(textDavor, t, "")]
   | z == "" = []
   | checkSim z t == True
   = collapseArray (toStringArray textDavor t (getTeilzeichenreiheNachSim z t))
                   (createCharArray (tail z) t (textDavor++[(head z)]))
   | checkSim z t == False = createCharArray (tail z) t (textDavor++[(head z)])
  
      
toStringArray :: Zeichenreihe -> Teilzeichenreihe -> Teilzeichenreihe -> Zerlegungszeugen
toStringArray textDavor teilzeichenreihe textDanach  = [(textDavor, teilzeichenreihe, textDanach)]

{- Knapp, aber gut nachvollziehbar geht tzr_zeugen folgendermassen vor:
   ...
-}

-- Aufgabe A.4

wieOft :: Zeichenreihe -> Teilzeichenreihe -> Nat0
wieOft = countTeil

countTeil :: Zeichenreihe -> Teilzeichenreihe -> Int
countTeil zeichenreihe teilzeichenreihe
  | not (ist_tzr zeichenreihe teilzeichenreihe) = 0
  | zeichenreihe == "" = 0
  | teilzeichenreihe == "" = length zeichenreihe + 1
  | checkSim zeichenreihe teilzeichenreihe = 1 + countTeil (tail zeichenreihe) teilzeichenreihe
  | not (checkSim zeichenreihe teilzeichenreihe) = 0 + countTeil (tail zeichenreihe) teilzeichenreihe

{- Knapp, aber gut nachvollziehbar geht wieOft folgendermassen vor:
   ...
-}