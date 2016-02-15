module Types where

import Control.Monad.Trans.RWS.Lazy

import Data.Time

data User = User { name         :: String
                 , mailaddress  :: Maybe String
                 , balance      :: Integer
                 , last_changed :: UTCTime  
                 }

type LambdaList = RWST () () () IO
