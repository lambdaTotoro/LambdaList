module Types where

import Control.Monad.Trans.RWS.Lazy

import Data.Time

data User = User { name         :: String
                 , mailaddress  :: Maybe String
                 , balance      :: Integer
                 , last_changed :: UTCTime  
                 }

data State = State { list  :: [User]
                   , index :: Int
                   }

type LambdaList = RWST () () State IO
