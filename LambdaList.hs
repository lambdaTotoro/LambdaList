 {-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

-- ######################################################
-- #                                                    #
-- #	== LambdaList ==                                #
-- #                                                    #
-- #    Ein kleines Haskellprogramm; geeignet um die    #
-- #    Getränkeliste der Fachschaft Technik an der     #
-- #    Uni Bielefeld zu managen.                       #
-- #                                                    #
-- #	Geschrieben von Jonas Betzendahl, 2013/14       #
-- #    jbetzend@techfak.uni-bielefeld.de               #
-- #                                                    #
-- #	Lizenz: CC0 / Public Domain                     #
-- #                                                    #
-- ######################################################

module Main where

import Data.Time
import Data.List                 (intercalate, sort)
import Data.List.Split           (splitOn)

import qualified Data.Text       as T
import qualified Data.Text.Lazy  as TL

import System.IO
import System.Exit
import System.Directory

import Network.Mail.SMTP

-- NICE TO HAVES:
-- --> ausführliche Dokumentation

-- Kung-Fu mit Typen

type Name        = String
type User        = String
type Domain      = String

newtype Guthaben = Guthaben Int

data NInterp     = NNull | NNothing

data NumberType  = Money | Amount

data MailAdress  = Adress User Domain -- user provided an e-mail adress
                 | DefaultAdress      -- user has the standard e-mail pattern
                 | NoAdress           -- user provided no e-mail adress
                 | Mty                -- E-mail adress was not evaluated until now

data TColor      = TBlau | TGruen | TRot | TGelb

data Trinker     = Trinker { name     :: String
                           , guthaben :: Guthaben
                           , mailadr  :: MailAdress
                           , counter  :: Int
                           , inactive :: Bool
                           }

data Config      = Config { stdDomain  :: String
                          , stdHost    :: String
                          , absender   :: String
                          , logcc      :: String
                          , grenze     :: Int
                          , extra      :: Int
                          , kontodaten :: String
                          }

instance Eq Trinker where
      t0 == t1 = (name t0) == (name t1)

instance Ord Trinker where
    compare t0 t1 = compare (name t0) (name t1)

instance Show Trinker where
    show (Trinker a b c d f) = intercalate ";" updatedWerte
        where
          updatedWerte = if not f then [a, show b, showMail c a, show (d+1)]
                                  else [a, show b, showMail c a, show d]

          showMail :: MailAdress -> String -> String
          showMail (Adress u d)    _  = u  ++ '@':d
          showMail (DefaultAdress) nm = nm ++ " auf der Standarddomain "
          showMail (NoAdress)      _  = "n/a"
          showMail (Mty)           _  = ""

instance Show Guthaben where
    show (Guthaben n) = addMinus $ show (div (abs n - a) 100) ++ "." ++ addZeros (show a)
         where a = abs n `mod` 100
               addMinus = if n >= 0 then id else ("-" ++)
               addZeros
                  | abs a <= 9 = ("0" ++)
                  | otherwise  = id

-- Datei - Ein- und Ausgabe

parseListe :: FilePath -> IO [Trinker]
parseListe fp = do a <- readFile fp
                   return $ map (parseTrinker . splitOn ";") (lines a)
    where
      parseTrinker :: [String] -> Trinker
      parseTrinker [a,b,c]   = parseTrinker [a,b,"",c]
      parseTrinker [a,b,c,d] = case cleanGuthaben b of
                                  Just u -> case readInt NNothing d of
                                     Just k  -> case splitOn "@" c of
                                        [y,z]   -> Trinker a (Guthaben u) (Adress y z) k False -- with E-Mail
                                        ["n/a"] -> Trinker a (Guthaben u) NoAdress     k False -- without E-Mail (silent)
                                        [""]    -> Trinker a (Guthaben u) Mty          k False -- without E-Mail (vocal)
                                        _       -> error $ "Parsingfehler (E-Mail) hier: "   ++ c
                                     Nothing ->    error $ "Parsingfehler (Counter) hier: "  ++ d
                                  Nothing ->       error $ "Parsingfehler (Guthaben) hier: " ++ b
      parseTrinker _         =                     error   "Parsingfehler: inkorrekte Anzahl Elemente in mindestens einer Zeile"

writeFiles :: [Trinker] -> Config -> IO()
writeFiles trinker c = let strinker = sort trinker
                        in do putStr    "\nSchreibe .txt und .tex auf Festplatte ... "
                              writeFile "mateliste.txt" $ unlines $ map show strinker
                              writeFile "mateliste.tex" $ unlines $ [latexHeader] ++ map (toLaTeX c) strinker ++ [latexFooter]
                              putStrLn  "fertig!"
                              putStrLn  "\nZuletzt müssen Benachrichtigungen verschickt werden."
                              sendAllMails strinker c
                              putStrLn  "Das Programm wird hiermit beendet. Ich hoffe es ist alles zu Ihrer Zufriedenheit. Bis zum nächsten Mal! :-)"

toLaTeX :: Config -> Trinker -> String
toLaTeX conf (Trinker nm gb@(Guthaben b) _ _ _)
    | b < (grenze conf) = "\\rowcolor{dunkelgrau}\n" ++ latexRow
    | b < 0             = "\\rowcolor{hellgrau}\n"   ++ latexRow
    | otherwise         =                              latexRow
      where
        latexRow :: String
        latexRow = nm ++ "&" ++ show gb ++ "& & & & & & \\\\\n" ++ if b > (extra conf) then "& & & & & & & \\\\\n\\hline" else "\\hline"

latexHeader :: String
latexHeader = "\\documentclass[a4paper,10pt,landscape]{article}\n\\usepackage[utf8]{inputenc}\n"
              ++ "\\usepackage{german}\n\\usepackage{longtable}\n\\usepackage{eurosym}\n"
              ++ "\\usepackage{color}\n\\usepackage{colortbl}\n\\usepackage{geometry}"
              ++ "\n\\geometry{a4paper,left=0mm,right=0mm, top=0.5cm, bottom=0.75cm}"
              ++ "\n\n\\definecolor{dunkelgrau}{rgb}{0.6,0.6,0.6}\n\\definecolor{hellgrau}{rgb}{0.8,0.8,0.8}\n"
              ++ "\n\\begin{document}\n\\begin{longtable}{|l|p{3cm}|p{5cm}|l|l|p{2cm}|p{2cm}|p{2cm}|}\n\\hline"
              ++ "\n\\textbf{Login} & Guthaben & Club Mate (0,90 \\euro) & Cola \\slash\\ Brause (0,70 \\euro)"
              ++ "& Schokor. (0,50 \\euro) & 0,20 \\euro & 0,10 \\euro & 0,05 \\euro\\\\\n\\hline\n\\hline\n\\endhead\n"

latexFooter :: String
latexFooter =  concat (replicate 10 "& & & & & & & \\\\\n\\hline\n") ++ "\\end{longtable}\\bigskip"
               ++ "\n\\begin{center} \n Neue Trinker tragen sich bitte im Stil vom TechFak-Login ein.\\\\ \n"
               ++ "\n(1. Buchstabe des Vornamens + 7 Buchstaben des Nachnamens (oder voller Nachname)) \\bigskip \\\\ \n"
               ++ "\\textbf{Je mehr Geld in der Kasse, desto schneller gibt es neue Getränke!} \\\\ \n"
               ++ "\\textbf{Also seid so freundlich und übt bitte ein bisschen \\glqq peer pressure\\grqq\\ auf die Leute im Minus aus.}\n"
               ++ "\\end{center} \n \\end{document}"

-- Alles um Mails herum

processList :: Config -> [Trinker] -> Bool -> IO [Trinker]
processList c xs sh = do let fl = filterList c xs
                         case sh of
                              True  -> putStrLn "Ermittele alle Trinker mit mehr als 10 € Schulden:\n" >> showList fl 0
                              False -> putStrLn "Eingabe nicht erkannt. Ich wiederhole:"
                         putStrLn "\nBitte geben Sie ein, an wen alles böse E-Mails verschickt werden sollen."
                         putStr   "(Durch Kommata getrennte Liste von Nummern, \"none\" für keine oder \"all\" für alle)\nEingabe: "
                         line <- getLine
                         case line of
                              "none" -> putStrLn "--> Es werden keine bösen Mails verschickt." >> return []
                              "all"  -> putStrLn "--> Böse Mails werden an alle verschickt.\n" >> return fl
                              _      -> case reads ("[" ++ line ++ "]") of
                                             [(ys, "")] -> putStrLn "--> Böse Mails werden an ausgewählte Empfänger verschickt.\n" >> return (map (fl !!) ys)
                                             _          -> processList c xs False
      where
        showList :: [Trinker] -> Int -> IO ()
        showList []                              _ = return ()
        showList (t@(Trinker nm g mMail c f):xs) n = do putStrLn $ "    " ++ show n ++ ":  (" ++ showFarbe TRot (show g) ++ ")  " ++ showFarbe TBlau nm
                                                        showList xs (n+1)

filterList :: Config -> [Trinker] -> [Trinker]
filterList _    []     = []
filterList conf (t:xs) = let rl = filterList conf xs in if (unwrapGuthaben . guthaben) t < (grenze conf) then t:rl else rl

sendAllMails :: [Trinker] -> Config -> IO ()
sendAllMails xs c = do lst <- processList c xs True
                       mapM_ (sendEvilEmail c) lst
                       putStrLn "\nSendevorgang abgeschlossen."

sendEvilEmail :: Config -> Trinker -> IO ()
sendEvilEmail config t = case mailadr t of
                              Mty      -> putStrLn $ showFarbe TRot "    ->" ++ " Konnte keine böse E-Mail an " ++ showFarbe TBlau (name t) ++ " senden, da noch keine E-Mail-Adresse angegeben wurde."
                              NoAdress -> putStrLn $ showFarbe TRot "    ->" ++ " Konnte keine böse E-Mail an " ++ showFarbe TBlau (name t) ++ " senden, da keine E-Mail-Adresse eingetragen wurde."
                              mMail    -> do let from  = Address (Just "Fachschaft Technik") ((T.pack . absender) config)
                                             let to      = case mMail of
                                                                DefaultAdress -> (Address Nothing (T.pack ((name t) ++ '@':(stdDomain config))))
                                                                (Adress u d)  -> (Address Nothing (T.pack (u  ++ '@':d)))
                                             let cc      = [(Address (Just "Getränkefuzzi") ((T.pack . logcc) config))] -- Empty list if no logging emails are desired
                                             let bcc     = []
                                             let subject = "[Fachschaft Technik] Mate-Konto ausgleichen!"
                                             let body    = plainTextPart $ TL.pack $ composeEvilEmail (name t) (guthaben t)
                                             let mail    = simpleMail from [to] cc bcc subject [body]
                                             sendMail (stdHost config) mail
                                             putStrLn $ showFarbe TGruen "    ->" ++ " Böse E-Mail an " ++ showFarbe TBlau (name t) ++ " erfolgreich versendet."
   where
      composeEvilEmail :: Name -> Guthaben -> String
      composeEvilEmail nm g = "Hallo " ++ nm ++ "!\n\nWenn du diese Mail erhältst bedeutet das, dass du mit deinem Matekonto\n(eventuell sogar deutlich) im Minus bist."
                              ++ "\nGenauer gesagt ist dein Guthaben auf der Mateliste aktuell: EUR " ++ show g ++ "\n\n"
                              ++ "Es handelt sich hier generell um ein Prepaid-Konto und wenn zu viele\nLeute zu stark im Minus sind, bedeutet das, dass wir keine Mate"
                              ++ "\nbestellen können oder wir sie teurer verkaufen müssen. Ich würde dich\nalso bitten, fluchs wieder etwas einzuzahlen.\n\n"
                              ++ "Du kannst uns natürlich auch einfach etwas überweisen. Kontoverbindung:\n\n" ++ (kontodaten config) ++ "\n\n"
                              ++ "Bitte nicht vergessen, euren Login oder Namen in den Verwendungszweck\nzu packen, sodass man euch identifizieren kann. Inzwischen kann man\n"
                              ++ "auch in der Fachschaft Bargeld hinterlegen, wenn mal der Mate-Fuzzi\nnicht da ist. Bittet dazu einfach einen beliebigen Fachschaftler\n"
                              ++ "das Geld im entsprechenden Briefumschlag in der Protokollkasse zu\ndeponieren.\n\n"
                              ++ "Vergesst bitte auch nicht euch auf der Liste in der Fachschaft euer\nentsprechendes Plus unter \"Guthaben\" zu notieren, damit es nicht zu\n"
                              ++ "Missverständnissen kommt.\n\nVielen Dank!\n\nLiebe Grüße,\n  euer automatisiertes Matekonto-Benachrichtigungsprogramm\n   (i.A. für die Fachschaft Technik)"

-- Helferfunktionen und Trivialitäten:

unwrapGuthaben :: Guthaben -> Int
unwrapGuthaben (Guthaben g) = g

readInt :: NInterp -> String -> Maybe Int
readInt NNull    "" = Just 0
readInt NNothing "" = Nothing
readInt _        xs = case reads xs of [(n, "")] -> Just n
                                       _         -> Nothing

showFarbe :: TColor -> String -> String
showFarbe clr txt = case clr of TRot   -> "\x1b[31m" ++ txt ++ "\x1b[0m"
                                TGruen -> "\x1b[32m" ++ txt ++ "\x1b[0m"
                                TGelb  -> "\x1b[33m" ++ txt ++ "\x1b[0m"
                                TBlau  -> "\x1b[36m" ++ txt ++ "\x1b[0m"

showGuthaben :: Guthaben -> String
showGuthaben gld@(Guthaben betr)
    | betr < 0  = showFarbe TRot   $ show gld
    | otherwise = showFarbe TGruen $ show gld

showTrinkerInfo :: Trinker -> IO ()
showTrinkerInfo t = putStrLn $ "\nDer User " ++ showFarbe TBlau (name t) ++ inac ++ " hat derzeit einen Kontostand von " ++ showGuthaben (guthaben t) ++ "."
    where
      inac :: String
      inac = if (counter t) == 0 then "" else " (" ++ show (counter t) ++ " Mal inaktiv)"

cleanGuthaben :: String -> Maybe Int
cleanGuthaben s = case readInt NNull $ filter (\c -> (c /= '.') && (c /= ',') ) s
                       of {Just n -> Just n ; _ -> Nothing}

parseNumber:: NumberType -> String -> IO Int
parseNumber nmbt str = let retry = putStrLn "-- Eingabe ungültig!" >> parseNumber nmbt str
                       in  do putStr str ; x <- getLine
                              case nmbt of
                                   Money  -> let ps = splitOn "," x
                                             in  case length ps of
                                                      1 -> case readInt NNull x of  -- parse cents only
                                                                Nothing -> retry
                                                                Just n  -> if n == 0 then return 0
                                                                                     else do putStr $ "Eingabe unklar: " ++ show n ++ " (E)uro oder (C)ents? "
                                                                                             y <- getLine
                                                                                             case y of
                                                                                                 "E" -> return $ 100*n
                                                                                                 "C" -> return n
                                                                                                 _   -> retry
                                                      2 -> let  h = head ps ; t = last ps -- parse euros
                                                            in  case readInt NNull h of
                                                                     Nothing  -> retry
                                                                     Just eur -> case readInt NNothing t of
                                                                                      Nothing -> retry
                                                                                      Just ct -> case length t of
                                                                                                      1 -> return $ 100*eur + 10*ct
                                                                                                      2 -> return $ 100*eur + ct
                                                                                                      _ -> retry
                                                      _ -> retry -- more than one ',' fails
                                   Amount -> case readInt NNull x of {Just n  -> return n ; Nothing -> retry}

frage :: String -> IO Bool
frage fr = do putStr fr ; q <- getLine
              return (q == "ok")

ifM :: Monad m => m Bool -> m b -> m b -> m b
ifM p a b = do { p' <- p ; if p' then a else b }

parseConfig :: String -> String -> Config
parseConfig mconf kconf = Config (ls !! 0) (ls !! 1) (ls !! 2) (ls !! 3) threshold extra ks
  where
    ls = let content = (lines . clearConf) mconf
          in if length content == 6
                then content
                else error "Fehler in Konfigurationsdatei!"

    ks        = clearConf kconf
    extra     = case readInt NNothing (ls !! 5) of { Just n -> n ; Nothing -> 5000 }
    threshold = case readInt NNothing (ls !! 4) of { Just n -> n ; Nothing -> 0 }

    clearConf :: String -> String
    clearConf = unlines . (filter (\l -> not ((null l) || (head l == '#')))) . lines

-- Hauptprogrammlogik:

processTrinker :: Trinker -> [Int] -> IO Trinker
processTrinker t werte@[enzhlng, nnzg, sbzg, fnfzg, zwnzg, zhn, fnf]
               = return $ if all (==0) werte
                     then Trinker (name t) (guthaben t)                                                      (mailadr t) ((counter t) + 1) True
                     else Trinker (name t) (Guthaben ((unwrapGuthaben . guthaben) t + enzhlng - vertrunken)) (mailadr t) 0                 True
    where
      vertrunken = sum $ zipWith (*) [90, 70, 50, 20, 10, 5] (tail werte)

getAmounts :: Name -> IO [Int]
getAmounts nm = do a <- parseNumber Money ("-- Wie viel Geld hat " ++ nm ++ " eingezahlt? ")
                   b <- mapM (parseNumber Amount) $ map (strichFragen nm) ["90", "70", "50", "20", "10", " 5"]
                   return $ a:b
    where
      strichFragen :: Name -> String -> String
      strichFragen nm amnt = "-- Wie viele Striche hat " ++ nm ++ " in der Spalte für " ++ amnt ++ " Cent? "

askEmail :: Trinker -> IO Trinker
askEmail t@(Trinker nm gthb (Adress u d)  c f) = return t
askEmail t@(Trinker nm gthb DefaultAdress c f) = return t
askEmail t@(Trinker nm gthb NoAdress      c f) = return t
askEmail t@(Trinker nm gthb Mty           c f) = do putStrLn $ "\n     Für diesen Trinker wurde noch " ++ showFarbe TRot "keine E-Mail-Adresse" ++ " eingetragen."
                                                    putStr     "     Bitte geben Sie eine gültige Adresse ein (\"default\" für den Standard, \"none\" für keine): "
                                                    l <- getLine
                                                    case splitOn "@" l of
                                                      ["default"] -> return (Trinker nm gthb DefaultAdress c f)
                                                      ["none"]    -> return (Trinker nm gthb NoAdress      c f)
                                                      [""]        -> return (Trinker nm gthb Mty           c f)
                                                      [x,y]       -> return (Trinker nm gthb (Adress x y)  c f)
                                                      _           -> do putStrLn "Eingabe nicht verstanden. Ich wiederhole:\n"
                                                                        askEmail t

-- Backups current state of MateListe
backupData :: Bool -> Bool -> IO ()
backupData False False = putStrLn $ "Lege Sicherungskopie der aktuellen Daten an     ..." ++ (showFarbe TGelb "nicht möglich") ++ ", da keine Daten vorhanden."
backupData txt   pdf   = do putStr  "Lege Sicherungskopie der aktuellen Daten an     ..."
                            timestamp <- getCurrentTime
                            let name = show timestamp
                            createDirectoryIfMissing True ("./backups/" ++ name) -- will always be missing due to timestamp precision, but creates parents as well this way
                            if txt then copyFile "./mateliste.txt"    ("./backups/" ++ name ++ "/mateliste.txt") else return ()
                            if pdf then copyFile "./mateliste.pdf"    ("./backups/" ++ name ++ "/mateliste.pdf") else return ()
                            putStrLn $ showFarbe TGruen " OK" ++ "!"

clearPermissions :: Bool -> IO Bool
clearPermissions x = do ptxt <- getPermissions "./mateliste.txt"
                        if x then do ptex <- getPermissions "./mateliste.tex"
                                     return $ and [readable ptxt, readable ptex, writable ptxt, writable ptex]
                             else return $ and [readable ptxt, writable ptxt]

neuTrinker :: IO Trinker
neuTrinker = do putStrLn "Neuer Trinker wird erstellt."
                x <- askName
                y <- askKontostand
                z <- askMailAdress
                putStr $ "Bitte geben Sie \"ok\" zum Bestätigen ein: Trinker " ++ showFarbe TBlau x ++ " mit einem Kontostand von " ++ showGuthaben (Guthaben y) ++ "  "
                o <- getLine
                if o == "ok" then return $ Trinker x (Guthaben y) z 0 True else putStrLn "Bestätigung nicht erhalten. Neuer Versuch:\n" >> neuTrinker
                   where askName :: IO String
                         askName = do putStr "Bitte geben Sie einen Nicknamen ein: " ; n <- getLine
                                      case n of {"" -> askName ; x -> return x}

                         askKontostand :: IO Int
                         askKontostand = parseNumber Money "Bitte geben Sie einen validen Kontostand ein: "

                         askMailAdress :: IO MailAdress
                         askMailAdress = do putStr "Bitte geben Sie eine gültige E-Mail-Adresse ein (\"default\" für Standard, \"none\" für keine): " ; l <- getLine
                                            case splitOn "@" l of {[""] -> return Mty ; ["none"] -> return NoAdress  ; ["default"] -> return DefaultAdress ; [x,y] -> return (Adress x y) ; _ -> askMailAdress}

listLoop :: [Trinker] -> Config -> Int -> IO ()
listLoop xs conf i = do
                     if i >= length xs
                        then do putStrLn $ "\n!! Sie haben das " ++ showFarbe TGelb "Ende" ++ " der aktuellen Liste erreicht. !!"
                                putStr     "!! Bitte wählen sie aus: speichern/b(e)enden | (a)bbrechen | (n)euer Trinker | (z)urück : "
                                c <- getLine
                                case c of
                                     "e" -> ifM (frage "Wirklich beenden (bisherige Änderungen werden geschrieben)? Bitte geben Sie \"ok\" ein: ")
                                            (writeFiles xs conf) (putStrLn "Doch nicht? Okay, weiter geht's!" >> listLoop xs conf i)

                                     "a" -> ifM (frage "Wirklich abbrechen (bisherige Änderungen werden verworfen)? Bitte geben Sie \"ok\" ein: ")
                                            (putStrLn "Dann bis zum nächsten Mal! :)") (putStrLn "Doch nicht? Okay, weiter geht's!" >> listLoop xs conf i)

                                     "n" -> do neu <- neuTrinker ; listLoop (xs ++ [neu]) conf i

                                     "z" -> let z q = max (i-q) 0 in case (readInt NNothing . tail) c of {Nothing -> listLoop xs conf (z 1); Just n -> listLoop xs conf (z n)}

                                     _   -> putStrLn "Eingabe nicht verstanden. Ich wiederhole: " >> listLoop xs conf i

                        else do let tr = (head . drop i) xs
                                showTrinkerInfo tr
                                putStr "Bitte wählen Sie aus! (a)bbrechen | (b)earbeiten | b(e)enden | (l)öschen | übe(r)schreiben | (v)or | (z)urück : "
                                c <- getLine
                                case c of
                                     "a"    -> ifM (frage "Wirklich abbrechen (bisherige Änderungen werden verworfen)? Bitte geben Sie \"ok\" ein: ")
                                               (putStrLn "Dann bis zum nächsten Mal! :)") (putStrLn "Doch nicht? Okay, weiter geht's!" >> listLoop xs conf i)

                                     "e"    -> ifM (frage "Wirklich beenden (bisherige Änderungen werden gespeichert)? Bitte geben Sie \"ok\" ein: ")
                                               (writeFiles xs conf) (putStrLn "Doch nicht? Okay, weiter geht's!" >> listLoop xs conf i)

                                     "l"    -> do putStr $ "Bitte geben Sie \"ok\" ein um " ++ showFarbe TBlau ((\(Trinker nm _ _ _ _) -> nm) tr) ++ " aus der Liste entfernen: " ; q <- getLine
                                                  if q == "ok" then listLoop (take i xs ++ drop (i+1) xs) conf i else listLoop xs conf i

                                     "r"    -> do neu <- neuTrinker ; listLoop (take i xs ++ neu:drop (i+1) xs) conf i

                                     "b"    -> let foobar ti p = do putStr "Bitte geben Sie \"ok\" zum Bestätigen ein: " ; q <- getLine
                                                                    case q of "ok" -> do k <- askEmail p
                                                                                         listLoop (take i xs ++ k : drop (i+1) xs) conf (i+1)
                                                                              ""   -> foobar ti p
                                                                              _    -> putStr "Vorgang abgebrochen. Wiederhole:" >> listLoop xs conf i
                                                in do p <- (\(Trinker name gth mMail ctr f) -> (getAmounts name >>= processTrinker (Trinker name gth mMail ctr True))) tr
                                                      showTrinkerInfo p ; foobar tr p

                                     'v':bs -> let z q = min (i+q) (length xs) in case (readInt NNothing . tail) c of {Nothing -> listLoop xs conf (z 1); Just n -> listLoop xs conf (z n)}
                                     'z':bs -> let z q = max (i-q) 0           in case (readInt NNothing . tail) c of {Nothing -> listLoop xs conf (z 1); Just n -> listLoop xs conf (z n)}

                                     ""     -> listLoop xs conf (min (i+1) (length xs))
                                     _      -> putStr "Eingabe nicht verstanden. Ich wiederhole: " >> listLoop xs conf i

main :: IO ()
main = do hSetBuffering stdout NoBuffering

          putStrLn "++ LambdaList v. 1.1 ++ \n\nWillkommen, User!"
          putStrLn "Dies ist ein automatisches Matelistenprogramm. Bitte beantworten Sie die Fragen auf dem Schirm.\n"
          putStr   "Scanne Verzeichnis nach vorhandener Mateliste   ... "

          mc <- doesFileExist "./mail.conf"
          kc <- doesFileExist "./konto.conf"

          conf <- if not (mc && kc)
                     then putStrLn "Konfigurationsdateien nicht vorhanden, bite Getränkefuzzi alarmieren!\n\nProgramm wird nun beendet!" >> exitFailure
                     else do mcs <- readFile "./mail.conf"
                             kcs <- readFile "./konto.conf"
                             return $ parseConfig mcs kcs

          l <- doesFileExist "./mateliste.txt"
          t <- doesFileExist "./mateliste.tex"
          p <- doesFileExist "./mateliste.pdf"
          list <- case l of
                       True  -> do putStrLn ((showFarbe TGruen "OK") ++ "!")
                                   putStr "Überprüfe Berechtigungen auf relevanten Dateien ... "
                                   permsok <- if t then clearPermissions True  -- check tex
                                                   else clearPermissions False  -- don't check tex
                                   case permsok of
                                        True  -> putStrLn ((showFarbe TGruen "OK") ++ "!") >> parseListe "./mateliste.txt"
                                        False -> do putStrLn $ (showFarbe TRot "Fehlschlag") ++ "!\nBerechtigungen nicht vorhanden, bitte Getränkefuzzi alarmieren!\n\nProgramm wird nun beendet!"
                                                    exitFailure
                                                    return []
                       False -> putStrLn ((showFarbe TRot "Fehlschlag") ++ "! Beim Beenden wird eine neue Datei angelegt werden.") >> return []
          backupData l p
          listLoop list conf 0
