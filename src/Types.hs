module Types where

import Control.Monad.Trans.RWS.Lazy

import Data.Time

newtype Balance = Balance Integer

data Env = Env { host         :: String       -- host for emails
               , from_address :: String       -- sender address for angry emails
               , cc_address   :: Maybe String -- CC address for angry emails
               , thresh_low   :: Integer      -- Threshold (negative) for marking in the list
               , thresh_lower :: Integer      -- Threshold (negative) for angry e-mails
               , thresh_bonus :: Integer      -- Threshold (positive) for bonus treatment
               } 

data User  = User  { name         :: String
                   , mailaddress  :: Maybe String
                   , balance      :: Balance
                   , last_changed :: UTCTime  
                   }

data State = State { list  :: [User]
                   , index :: Int
                   }

type LambdaList = RWST Env () State IO
