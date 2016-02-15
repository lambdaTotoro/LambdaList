module Parser where

import Types

import Control.Applicative ((<|>))

import Data.Attoparsec.Text
import Data.Time

parseUser :: Parser User
parseUser = do name <- many1 letter
               char ';'
               balance <- decimal
               char ';'
               mail <- many' letter
               let wrap = if mail == "" then Nothing else Just mail
               char ';'
               time <- parseUTC
               return $ User name wrap balance time

parseUTC :: Parser UTCTime
parseUTC = undefined
