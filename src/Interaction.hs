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
createEnvironment = do putStrLn ""
                       putStrLn "(in this case please enter \"create\" at the prompt), or you can quit"
                       putStrLn "now and write one offline (for this, please enter \"quit\").\n"
                       loop
 where
    loop :: IO ()
    loop = do putStr "\nWie wollen Sie fortfahren? "
              choice <- getLine
              case choice of
                   "erstellen" -> do putStrLn "\nErstellung der Konfigurationsdatei:\n"
                                     putStr "Bitte geben Sie die Hostadresse des Mailservers an: "                      ; hst <- getLine
                                     putStr "Bitte geben Sie die Absenderadresse für Mails an: "                        ; frm <- getLine
                                     putStr "Bitte geben Sie eine CC-Adresse für Mails an (optional): "                 ; cca <- getLine
                                     putStr "Bitte geben Sie eine Betreffzeile für Erinnerungsmails an: "               ; sbj <- getLine
                                     putStr "Bitte geben Sie eine Grenze (in Tagen) für Inaktivitätserkennung an: "     ; ina <- getLine
                                     putStr "Bitte geben Sie eine negative Grenze (in Cents) für rote Markierung an: "  ; tlw <- getLine
                                     putStr "Bitte geben Sie eine negative Grenze (in Cents) für Erinnerungsmails an: " ; tlr <- getLine
                                     putStr "Bitte geben Sie eine Grenze (in Cents) für positive Sonderbehandlung an: " ; tbn <- getLine
                                     createDirectoryIfMissing True "./conf"
                                     let configfile = unlines [hst,frm,cca,sbj,ina,tlw,tlr,tbn]
                                     writeFile "./conf/config.txt" configfile
                                     putStrLn "\nKonfigurationsdateierstellung abgeschlossen!"
                   "beenden"   -> do putStrLn ""
                                     exitSuccess
                   _           -> putStrLn "\nEingabe nicht erkannt.\n" >> loop

interactionLoop :: LambdaList ()
interactionLoop = undefined
