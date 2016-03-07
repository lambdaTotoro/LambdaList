module Main where

import Interaction
import Latex
import Mail
import Parser
import Types

import Control.Monad.Trans.RWS.Lazy

import System.Directory
import System.Exit
import System.IO

main :: IO ()
main = do hSetBuffering stdout NoBuffering

          putStrLn "LambdaList version. 2.0\n=======================\n\nWillkommen, User!"
          putStrLn "Dies ist ein automatisches Matelistenprogramm. Bitte beantworten Sie die Fragen auf dem Schirm."
          putStr   "Aktuelles Verzeichnis wird nach vorhandenen Dateien durchsucht ... \n\n"

          conf_exists <- doesFileExist "./conf/config.txt"
          list_exists <- doesFileExist "./conf/mateliste.txt"
          mail_exists <- doesFileExist "./conf/mail.txt"
          temp_exists <- doesFileExist "./tmp/tmp_mateliste"

          if conf_exists then putStrLn "[+] Konfigurationsdatei gefunden!"
                         else do putStrLn "[-] Keine Konfigurationsdatei (config.txt) gefunden!"

          if list_exists then putStrLn "[+] Benutzerliste gefunden!"
                         else do putStrLn "[-] Keine Benutzerliste (mateliste.txt) gefunden!"

          if mail_exists then putStrLn "[+] Mailtext für Erinnerungsmails gefunden!"
                         else putStrLn "[-] Keinen Mailtext für Erinnerungsmails (mail.txt) gefunden!" >> exitFailure

          if temp_exists then do putStrLn "\n~~ Temporären Dateien gefunden! ~~"
                         else putStrLn "\nKeine temporäre Dateien gefunden; starte neue Auswertung!"

          let env = undefined :: Env
          let ini = undefined :: State
   
          final <- list . fst <$> execRWST interactionLoop env ini
          print final
