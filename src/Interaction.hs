module Interaction where

import Types

import System.Directory
import System.Exit

colourString :: Colour -> String -> String
colourString c str = undefined

confirm :: String -> LambdaList Bool
confirm str = undefined

question :: String -> IO Bool
question str = do putStr (str ++ " [j/n]: ")
                  inp <- getLine
                  case inp of
                       "n" -> return False ; "N" -> return False
                       "j" -> return True  ; "J" -> return True
                       _   -> question str

createEnvironment :: IO ()
createEnvironment = do putStrLn "\n    Sie können nun entweder eine neue Konfigurationsdatei live (hier im Programm)"
                       putStrLn "    angeleitet erstellen oder das Programm beenden und die Datei offline schreiben."
                       loop
 where
    loop :: IO ()
    loop = do putStr "\n    Wie wollen Sie fortfahren (\"erstellen\" oder \"beenden\")? "
              choice <- getLine
              case choice of
                   "erstellen" -> do putStrLn "\n    Erstellung der Konfigurationsdatei:\n"
                                     putStr "    Bitte geben Sie die Hostadresse des Mailservers an: "                      ; hst <- getLine
                                     putStr "    Bitte geben Sie die Absenderadresse für Mails an: "                        ; frm <- getLine
                                     putStr "    Bitte geben Sie eine CC-Adresse für Mails an (optional): "                 ; cca <- getLine
                                     putStr "    Bitte geben Sie eine Betreffzeile für Erinnerungsmails an: "               ; sbj <- getLine
                                     putStr "    Bitte geben Sie eine Grenze (in Tagen) für Inaktivitätserkennung an: "     ; ina <- getLine
                                     putStr "    Bitte geben Sie eine negative Grenze (in Cents) für rote Markierung an: "  ; tlw <- getLine
                                     putStr "    Bitte geben Sie eine negative Grenze (in Cents) für Erinnerungsmails an: " ; tlr <- getLine
                                     putStr "    Bitte geben Sie eine Grenze (in Cents) für positive Sonderbehandlung an: " ; tbn <- getLine
                                     createDirectoryIfMissing True "./conf"
                                     let configfile = unlines [hst,frm,cca,sbj,ina,tlw,tlr,tbn]
                                     writeFile "./conf/config.txt" configfile
                                     putStrLn "\n    Konfigurationsdateierstellung abgeschlossen!"
                   "beenden"   -> putStrLn "\n    Eingabe erkannt. Programm wird jetzt beendet." >> exitSuccess
                   _           -> putStrLn "    ~~> Eingabe nicht erkannt." >> loop

interactionLoop :: LambdaList ()
interactionLoop = undefined
