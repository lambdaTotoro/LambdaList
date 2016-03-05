module Parser where

import Types

import Control.Applicative ((<|>))

import Data.Attoparsec.Text
import Data.Time

parseUser :: Parser User
parseUser = do name <- many1 letter
               char ';'
               balance <- parseBalance
               char ';'
               mail <- many' letter
               let wrap = if mail == "" then Nothing else Just mail
               char ';'
               time <- parseUTC
               return $ User name wrap balance time

parseUTC :: Parser UTCTime
parseUTC = undefined

parseBalance :: Parser Balance
parseBalance = pb_sep '.' <|> pb_sep ',' <|> pb_cents
  where
    pb_cents :: Parser Balance
    pb_cents = decimal >>= (return . Balance)

    pb_sep :: Char -> Parser Balance
    pb_sep c = do euro <- decimal
                  char c
                  cent <- decimal
                  return $ Balance ((euro * 100) + cent)
