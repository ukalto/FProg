{- module TestSuite7 where



import Angabe7
import Test.Tasty as T
import Test.Tasty.HUnit as T ( testCase, (@?=) )
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

{-# ANN module "HLint: ignore Use camelCase" #-}

main :: IO ()
main = defaultMainWithIngredients [consoleTestReporter] spec


t1 = (III, Viertel)
t2 = (XXII, Halb)
t3 = (IV, Viertel)
t4 = (III, Viertel)
t5 = (III, Viertel)

einUhr = AZ Schlag I
viertelZwanzig = AZ Viertel XX
halbZwanzig = AZ Halb XX
zwanzigUhr = AZ Schlag XX
halbVierzehn = AZ Halb XIV
dreiviertelSechs = AZ Dreiviertel XVIII
sechsUhr = AZ Schlag XVIII
siebenUhr = AZ Schlag XIX

b1 = "Bahnhof 1"
b2 = "Bahnhof 2"
b3 = "Bahnhof 3"
b4 = "Bahnhof 4"
b5 = "Bahnhof 5"
b6 = "Bahnhof 6"
b7 = "Bahnhof 7"
b8 = "Bahnhof 8"
b9 = "Bahnhof 9"
b10 = "Bahnhof 10"

vierundzwanzigEinhalb = RD XXIV Halb 
eineStunde = RD I Schlag
elfDreiviertel = RD XI Dreiviertel
zweiEinhalb = RD II Halb
zwoelfViertel = RD XII Viertel
zwoelfH = RD XII Schlag



spec :: TestTree
spec =
  testGroup
    "TestSuite7 Spec"
    [ 
        just_zero_Tests,
        two_stationTests,
        three_station_Tests,
        ten_station_Tests,
        four_station_Tests
    ]

-- Two Stations (b1 to b2)
planE1 = [(sechsUhr, b1, b2, zwoelfViertel)] -- Nothing (arrival too late)
planE2 = [(halbVierzehn, b1, b2, eineStunde)] -- Nothing (departure too early)
planE3 = [(einUhr, b1, b2, elfDreiviertel)] -- Nothing (arrival too late)
planE4 = [(dreiviertelSechs, b1, b2, eineStunde)] -- Nothing (departure too early)
planE6 = [(sechsUhr, b1, b2, vierundzwanzigEinhalb)] -- Nothing (duration too long)

just_zero_Tests :: TestTree
just_zero_Tests =
  testGroup
    "Tests für eine Station Fehlerhaft"
    [   testCase "plan E1" $ do
            konservenrechner planE1 b1 b2 @?= Nothing,
        testCase "plan E2" $ do
            konservenrechner planE2 b1 b2 @?= Nothing,
        testCase "plan E3" $ do
            konservenrechner planE3 b1 b2 @?= Nothing,
        testCase "plan E4" $ do
            konservenrechner planE4 b1 b2 @?= Nothing,
        testCase "plan E6" $ do
            konservenrechner planE6 b1 b2 @?= Nothing
    ]

planA1 = [(sechsUhr, b1, b2, elfDreiviertel)] -- Just 0
planA2 = [(einUhr, b1, b2, eineStunde)] -- Just 0
planA3 = [(viertelZwanzig, b1, b2, zweiEinhalb)] -- Just 0

two_stationTests :: TestTree
two_stationTests =
  testGroup
    "Tests für eine Station Korrekt"
    [   testCase "plan A1" $ do
            konservenrechner planA1 b1 b2 @?= Just 0,
        testCase "plan A2" $ do
            konservenrechner planA2 b1 b2 @?= Just 0,
        testCase "plan A3" $ do
            konservenrechner planA3 b1 b2 @?= Just 0,
        testCase "plan A3 b1 b1" $ do
            konservenrechner planA3 b1 b1 @?= Just 0,
        testCase "plan A3 b2 b2" $ do
            konservenrechner planA3 b2 b2 @?= Just 0
    ]

-- Three Stations (b1 to b3)
planA4 = [(sechsUhr, b1, b2, elfDreiviertel),(sechsUhr, b2, b3, elfDreiviertel),(viertelZwanzig, b1, b3, eineStunde)] -- Just 0 (direct connection)
planA5 = [(sechsUhr, b1, b2, elfDreiviertel),(sechsUhr, b2, b3, elfDreiviertel),(viertelZwanzig, b1, b3, elfDreiviertel)] -- Just 1 (two trains, second one unreachable for anyone)
planA6 = [(sechsUhr, b1, b2, eineStunde),(zwanzigUhr, b2, b3, eineStunde)] -- Just 0 (two trains, second one reachable for vladimir)
planA7 = [(sechsUhr, b1, b2, eineStunde),(halbZwanzig, b2, b3, eineStunde)] -- Just 1 (two trains, second one unreachable for vladimir)
planA8 = [(sechsUhr, b1, b2, eineStunde),(viertelZwanzig, b2, b3, eineStunde),(viertelZwanzig, b1, b3, eineStunde)] -- Just 0 (slow and fast connection)

planE5 = [(sechsUhr, b2, b3, eineStunde),(siebenUhr, b2, b3, eineStunde)] -- Nothing (no connection)

three_station_Tests :: TestTree
three_station_Tests =
  testGroup
    "Tests für drei Stationen"
    [   testCase "plan A4" $ do
            konservenrechner planA4 b1 b3 @?= Just 0,
        testCase "plan A5" $ do
            konservenrechner planA5 b1 b3 @?= Just 1,
        testCase "plan A6" $ do
            konservenrechner planA6 b1 b3 @?= Just 0,
        testCase "plan A7" $ do
            konservenrechner planA7 b1 b3 @?= Just 1,
        testCase "plan A8" $ do
            konservenrechner planA8 b1 b3 @?= Just 0,
        testCase "plan E5" $ do
            konservenrechner planE5 b1 b3 @?= Nothing
    ]

-- Ten Stations
planA9 = [(sechsUhr, b1, b2, eineStunde),(sechsUhr, b2, b3, eineStunde),(sechsUhr, b3, b4, eineStunde),(sechsUhr, b4, b5, eineStunde),(sechsUhr, b5, b6, eineStunde),(sechsUhr, b6, b7, eineStunde),(sechsUhr, b7, b8, eineStunde),(sechsUhr, b8, b9, eineStunde),(sechsUhr, b9, b10, eineStunde)]

ten_station_Tests :: TestTree
ten_station_Tests =
  testGroup
    "Tests für zehn Station"
    [   testCase "plan A9 b1 b3" $ do
            konservenrechner planA9 b1 b3 @?= Just 1,
        testCase "plan A9 b1 b5" $ do
            konservenrechner planA9 b1 b5 @?= Just 3,
        testCase "plan A9 b1 b10" $ do
            konservenrechner planA9 b1 b10 @?= Just 8,
        testCase "plan A9 b3 b5" $ do
            konservenrechner planA9 b3 b5 @?= Just 1,
        testCase "plan A9 b4 b9" $ do
            konservenrechner planA9 b4 b9 @?= Just 4,
        testCase "plan A9 b4 b3" $ do
            konservenrechner planA9 b4 b3 @?= Nothing
    ]

-- Random
planA10 = [(sechsUhr, b1, b2, eineStunde),(viertelZwanzig, b2, b3, eineStunde),(viertelZwanzig, b1, b3, eineStunde),(viertelZwanzig, b3, b5, elfDreiviertel),(einUhr, b3, b4, eineStunde),(sechsUhr, b3, b4, eineStunde),(viertelZwanzig, b4, b5, eineStunde)]
planA11 = [(sechsUhr, b1, b2, eineStunde),(sechsUhr, b2, b3, eineStunde),(sechsUhr, b3, b1, eineStunde)]
planA12 = [(sechsUhr, b1, b2, zwoelfH),(sechsUhr, b2, b3, zwoelfH)]

four_station_Tests :: TestTree
four_station_Tests =
  testGroup
    "Weitere Tests"
    [   testCase "plan A10 b1 b5" $ do
            konservenrechner planA10 b1 b5 @?= Just 1,
        testCase "plan A10 b1 b4" $ do
            konservenrechner planA10 b1 b4 @?= Just 0,
        testCase "plan A10 b3 b5" $ do
            konservenrechner planA10 b3 b5 @?= Just 1,
        testCase "plan A10 b5 b3" $ do
            konservenrechner planA10 b5 b3 @?= Nothing,
        testCase "plan A11 b1 b4" $ do
            konservenrechner planA11 b1 b4 @?= Nothing,
        testCase "plan A11 b1 b3" $ do
            konservenrechner planA11 b1 b3 @?= Just 1,
        testCase "plan A11 b3 b2" $ do
            konservenrechner planA11 b3 b2 @?= Just 1,
        testCase "plan A12 b1 b3" $ do
            konservenrechner planA12 b1 b3 @?= Just 1
    ] -}