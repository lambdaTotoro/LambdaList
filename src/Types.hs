module Types where

import Control.Monad.Trans.RWS.Lazy

import Data.Time

newtype Balance = Balance Integer -- Balance is kept as an Integer in cents.
                  deriving Show

data Env = Env { host         :: String       -- host for e-mails
               , from_address :: String       -- sender address for e-mails
               , cc_address   :: Maybe String -- CC address for e-mails
               , subj_mail    :: String       -- Subject for angry e-mails
               , mailtext     :: String       -- Text for e-mails (allowed wildcards: $NAME, $BALANCE and $LASTCHANGED)
               , thresh_inac  :: Integer      -- Threshold (positive) in days after wich deletion will be suggested
               , thresh_low   :: Integer      -- Threshold (negative) in cents for red highlighting in the list
               , thresh_lower :: Integer      -- Threshold (negative) in cents for worse highlighting and angry e-mails
               , thresh_bonus :: Integer      -- Threshold (positive) in cents for bonus treatment
               } 

data User  = User  { name         :: String
                   , mailaddress  :: Maybe String
                   , balance      :: Balance
                   , last_changed :: UTCTime  
                   } deriving Show

data State = State { list  :: [User]
                   , index :: Int
                   }

type LambdaList = RWST Env () State IO -- Monad Stack with everything we need

data Colour = Red | Green | Blue | Yellow
