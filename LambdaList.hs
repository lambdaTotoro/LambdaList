-- ######################################################
-- #                                                    #
-- #	== LambdaList ==                                #
-- #                                                    #
-- #    Ein kleines Haskellprogramm; geeignet um die    #
-- #    Getränkeliste der Fachschaft Technik an der     #
-- #    Uni Bielefeld zu managen.                       #
-- #                                                    #
-- #	Geschrieben von Jonas Betzendahl, 2013          #
-- #    jbetzend@techfak.uni-bielefeld.de               #
-- #                                                    #
-- #	Lizenz: CC0 / Public Domain                     #
-- #                                                    #
-- ######################################################

module Main where

import Data.List            (intercalate, sort)
import Data.List.Split      (splitOn)

import System.IO
import System.Directory     (doesFileExist)

-- NICE TO HAVES:
-- --> ausführliche Dokumentation
-- --> Einlesen von "1.2" Euro oder so

-- Kung-Fu mit Typen

type Name        = String
type Counter     = Int
type Flag        = Bool

newtype Guthaben = Guthaben Int

data NInterp     = NNull | NNothing
data TColor      = TBlau | TGruen | TRot | TGelb
data Trinker     = Trinker Name Guthaben Counter Flag

instance Eq Trinker where
    (Trinker a _ _ _) == (Trinker x _ _ _) = a == x

instance Ord Trinker where
    compare (Trinker a _ _ _)  (Trinker x _ _ _) = compare a x

instance Show Guthaben where
    show (Guthaben n) = addMinus $ show (div (abs n - a) 100) ++ "." ++ addZeros (show a)
         where a = abs n `mod` 100
               addMinus = if n >= 0 then id else ("-" ++)
               addZeros 
                  | abs a <= 9 = ("0" ++)
                  | otherwise  = id

instance Show Trinker where
    show (Trinker a b c f) = intercalate ";" updatedWerte
        where
          updatedWerte = if not f then [a, show b, show (c+1)]
                                  else [a, show b, show c]

-- Datei - Ein- und Ausgabe

parseListe :: FilePath -> IO [Trinker] 
parseListe fp = do a <- readFile fp
                   return $ map (parseTrinker . splitOn ";") (lines a)
    where
      parseTrinker :: [String] -> Trinker
      parseTrinker [x,y,z] = case cleanGuthaben y of Just u -> case readInt NNothing z of Just k  -> Trinker x (Guthaben u) k False
                                                                                          Nothing -> error $ "Parsingfehler bei Guthaben hier: " ++ z
                                                     Nothing -> error $ "Parsingfehler! Unkorrekter Betrag hier: " ++ concat [x,y,z]
      parseTrinker _       = error "Parsingfehler: inkorrekte Anzahl Elemente in mindestens einer Zeile"

writeFiles :: [Trinker] -> IO()
writeFiles trinker = let strinker = sort trinker in
                         do putStr    "\nSchreibe .txt und .tex auf Festplatte ... "
                            writeFile "mateliste.txt" $ unlines $ map show strinker
                            writeFile "mateliste.tex" $ unlines $ [latexHeader] ++ map toLaTeX strinker ++ [latexFooter]
                            putStrLn  "done!"
                            putStrLn  "Das Programm wird hiermit beendet. Ich hoffe es ist alles zu Ihrer Zufriedenheit. Bis zum nächsten Mal! :-)"

toLaTeX :: Trinker -> String
toLaTeX (Trinker nm gb@(Guthaben b) _ _)
    | b < -1000 = "\\rowcolor{dunkelgrau}\n" ++ latexRow
    | b < 0     = "\\rowcolor{hellgrau}\n"   ++ latexRow
    | otherwise =                               latexRow
      where
        latexRow :: String
        latexRow = nm ++ "&" ++ show gb ++ "& & & & & & \\\\\n\\hline"

latexHeader :: String
latexHeader = "\\documentclass[a4paper,10pt,landscape]{article}\n\\usepackage[utf8]{inputenc}\n"
              ++ "\\usepackage{german}\n\\usepackage{longtable}\n\\usepackage{eurosym}\n"
              ++ "\\usepackage{color}\n\\usepackage{colortbl}\n\\usepackage{geometry}"
              ++ "\n\\geometry{a4paper,left=0mm,right=0mm, top=0.5cm, bottom=0.75cm}"
              ++ "\n\n\\definecolor{dunkelgrau}{rgb}{0.6,0.6,0.6}\n\\definecolor{hellgrau}{rgb}{0.8,0.8,0.8}\n"
              ++ "\n\\begin{document}\n\\begin{longtable}{|l|p{3cm}|p{5cm}|l|l|p{2cm}|p{2cm}|p{2cm}|}\n\\hline"
              ++ "\n\\textbf{Login} & Guthaben & Club Mate (0,90 \\euro) & Cola \\slash\\ Brause (0,70 \\euro)"
              ++ "& Schokor. (0,50 \\euro) & 0,20 \\euro & 0,10 \\euro & 0,05 \\euro\\\\\n\\hline\n\\hline\n"

latexFooter :: String
latexFooter =  concat (replicate 10 "& & & & & & & \\\\\n\\hline\n") ++ "\\end{longtable}\\bigskip"
               ++ "\n\\begin{center} \n Neue Trinker tragen sich bitte im Stil vom TechFak-Login ein.\\\\ \n"
               ++ "\n(1. Buchstabe des Vornamens + 7 Buchstaben des Nachnamens (oder voller Nachname)) \\bigskip \\\\ \n"
               ++ "\\textbf{Je mehr Geld in der Kasse, desto schneller gibt es neue Getränke!} \\\\ \n"
               ++ "\\textbf{Also seid so freundlich und übt bitte ein bisschen \\glqq peer pressure\\grqq\\ auf die Leute im Minus aus.}\n"
               ++ "\\end{center} \n \\end{document}"

-- Helferfunktionen und Trivialitäten:

readInt :: NInterp -> String -> Maybe Int
readInt NNull    "" = Just 0
readInt NNothing "" = Nothing
readInt _        xs = case reads xs of [(n, "")] -> Just n
                                       _         -> Nothing

showFarbe :: TColor -> String -> String
showFarbe clr txt = case clr of TRot   -> "\x1b[31m" ++ txt ++ "\x1b[0m"
                                TGruen -> "\x1b[32m" ++ txt ++ "\x1b[0m"
                                TGelb  -> "\x1b[33m" ++ txt ++ "\x1b[0m"
                                TBlau  -> "\x1b[34m" ++ txt ++ "\x1b[0m"

showGuthaben :: Guthaben -> String
showGuthaben gld@(Guthaben betr)
    | betr < 0  = showFarbe TRot   $ show gld
    | otherwise = showFarbe TGruen $ show gld

showTrinkerInfo :: Trinker -> IO ()
showTrinkerInfo (Trinker nm gld ctr _) = putStrLn $ "\nDer User " ++ showFarbe TBlau nm ++ inac ++ " hat derzeit einen Kontostand von " ++ showGuthaben gld ++ "."
    where
      inac :: String
      inac = if ctr == 0 then "" else " (" ++ show ctr ++ " Mal inaktiv)"

cleanGuthaben :: String -> Maybe Int
cleanGuthaben s = case readInt NNull $ filter (not . (`elem` ",.")) s
                       of {Just n -> Just n ; _ -> Nothing}

frage :: String -> IO Bool
frage fr = do putStr fr ; q <- getLine
              return (q == "ok")

ifM :: Monad m => m Bool -> m b -> m b -> m b
ifM p a b = do { p' <- p ; if p' then a else b }

-- Hauptprogrammlogik:

processTrinker :: Trinker -> [Int] -> IO Trinker 
processTrinker (Trinker nm (Guthaben gld) cntr _) werte@[enzhlng, nnzg, sbzg, fnfzg, zwnzg, zhn, fnf]
               = return $ if all (==0) werte then Trinker nm (Guthaben gld)                          (cntr+1) True
                                             else Trinker nm (Guthaben (gld + enzhlng - vertrunken)) 0        True
    where
      vertrunken = sum $ zipWith (*) [90, 70, 50, 20, 10, 5] (tail werte)

getAmounts :: Name -> IO [Int]
getAmounts nm = mapM (abfrage nm) fragen
    where
      fragen :: [String]
      fragen = ("-- Wie viel Geld hat " ++ nm ++ showFarbe TGelb " in Cent" ++ " eingezahlt? "):map (strichFragen nm) ["90", "70", "50", "20", "10", " 5"]
     
      strichFragen :: Name -> String -> String
      strichFragen nm amnt = "-- Wie viele Striche hat " ++ nm ++ " in der Spalte für " ++ amnt ++ " Cent? "

      abfrage :: Name -> String -> IO Int
      abfrage nm frg = do putStr frg ; x <- getLine
                          case readInt NNull x of {Just n  -> return n ; Nothing -> putStr "-- Eingabe unklar!" >> abfrage nm frg}
       
neuTrinker :: IO Trinker
neuTrinker = do putStrLn "Neuer Trinker wird erstellt."
                x <- askName
                y <- askKontostand
                putStr $ "Bitte geben Sie \"ok\" zum Bestätigen ein: Trinker " ++ showFarbe TBlau x ++ " mit einem Kontostand von " ++ showGuthaben (Guthaben y) ++ "  "
                o <- getLine
                if o == "ok" then return $ Trinker x (Guthaben y) 0 True else putStrLn "Bestätigung nicht erhalten. Neuer Versuch:\n" >> neuTrinker
                   where askName :: IO String
                         askName = do putStr "Bitte geben Sie einen Nicknamen ein: " ; n <- getLine
                                      case n of {"" -> askName ; x -> return x}

                         askKontostand :: IO Int
                         askKontostand = do putStr $ "Bitte geben Sie einen validen Kontostand " ++ showFarbe TGelb "in Cent" ++ " ein: " ; l <- getLine
                                            case readInt NNull l of {Just d -> return d ; _ -> askKontostand}

listLoop :: IO [Trinker] -> Int -> IO ()
listLoop xs i = do
                as <- xs
                if i >= length as 
                   then do putStrLn $ "\n!! Sie haben das " ++ showFarbe TGelb "Ende" ++ " der aktuellen Liste erreicht. !!"
                           putStr     "!! Bitte wählen sie aus: speichern/b(e)enden | (a)bbrechen | (n)euer Trinker | (z)urück : "
                           c <- getLine
                           case c of
                                "e" -> ifM (frage "Wirklich beenden (bisherige Änderungen werden geschrieben)? Bitte geben Sie \"ok\" ein: ")
                                       (writeFiles as) (putStrLn "Doch nicht? Okay, weiter geht's!" >> listLoop xs i)

                                "a" -> ifM (frage "Wirklich abbrechen (bisherige Änderungen werden verworfen)? Bitte geben Sie \"ok\" ein: ")
                                       (putStrLn "Dann bis zum nächsten Mal! :)") (putStrLn "Doch nicht? Okay, weiter geht's!" >> listLoop xs i)
                               
                                "n" -> do neu <- neuTrinker ; listLoop (return (as ++ [neu])) i

                                "z" -> let z q = max (i-q) 0 in case (readInt NNothing . tail) c of {Nothing -> listLoop xs (z 1); Just n -> listLoop xs (z n)}

                                _   -> putStrLn "Eingabe nicht verstanden. Ich wiederhole: " >> listLoop xs i
  
                   else do let tr = (head . drop i) as
                           showTrinkerInfo tr
                           putStr "Bitte wählen Sie aus! (a)bbrechen | (b)earbeiten | b(e)enden | (l)öschen | übe(r)schreiben | (v)or | (z)urück : "
                           c <- getLine
                           case c of
                                "a"    -> ifM (frage "Wirklich abbrechen (bisherige Änderungen werden verworfen)? Bitte geben Sie \"ok\" ein: ")
                                          (putStrLn "Dann bis zum nächsten Mal! :)") (putStrLn "Doch nicht? Okay, weiter geht's!" >> listLoop xs i)

                                "e"    -> ifM (frage "Wirklich beenden (bisherige Änderungen werden geschrieben)? Bitte geben Sie \"ok\" ein: ")
                                          (writeFiles as) (putStrLn "Doch nicht? Okay, weiter geht's!" >> listLoop xs i)

                                "l"    -> do putStr $ "Bitte geben Sie \"ok\" ein um " ++ showFarbe TBlau ((\(Trinker nm _ _ _) -> nm) tr) ++ " aus der Liste entfernen: " ; q <- getLine
                                             if q == "ok" then listLoop (return (take i as ++ drop (i+1) as)) i else listLoop xs i  

                                "r"    -> do neu <- neuTrinker ; listLoop (return (take i as ++ neu:drop (i+1) as)) i

                                "b"    -> let foobar ti p = do putStr "Bitte geben Sie \"ok\" zum Bestätigen ein: " ; q <- getLine
                                                               case q of "ok" -> listLoop (return (take i as ++ p : drop (i+1) as)) (i+1)
                                                                         ""   -> foobar ti p
                                                                         _    -> putStr "Vorgang abgebrochen. Wiederhole:" >> listLoop xs i
                                          in do p <- (\(Trinker name gth ctr f) -> (getAmounts name >>= processTrinker (Trinker name gth ctr True))) tr
                                                showTrinkerInfo p ; foobar tr p

                                'v':bs -> let z q = min (i+q) (length as) in case (readInt NNothing . tail) c of {Nothing -> listLoop xs (z 1); Just n -> listLoop xs (z n)}
                                'z':bs -> let z q = max (i-q) 0           in case (readInt NNothing . tail) c of {Nothing -> listLoop xs (z 1); Just n -> listLoop xs (z n)}

                                ""     -> listLoop xs (min (i+1) (length as))
                                _      -> putStr "Eingabe nicht verstanden. Ich wiederhole: " >> listLoop xs i

main :: IO()
main = do hSetBuffering stdout NoBuffering

          putStrLn "++ LambdaList v. 1.0 ++ \n\nWillkommen, User!"
          putStrLn "Dies ist ein automatisches Matelistenprogramm. Bitte beantworten Sie die Fragen auf dem Schirm."
          putStr   "Scanne Verzeichnis nach vorhandener mateliste.txt ... "
          f <- doesFileExist "mateliste.txt" 
          if f 
             then putStrLn " Liste gefunden!" >> listLoop (parseListe "mateliste.txt") 0
             else putStrLn " keine Liste gefunden. Beim Beenden des Programms wird eine neue geschrieben werden." >> listLoop (return []) 0
