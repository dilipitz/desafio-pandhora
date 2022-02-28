{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import RIO
import RIO.Process

import LimitOrderBook.Types

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  , mockDatabase :: MVar LimitOrderDB
  }
instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

class HasMockedDatabase env where
  mockDatabaseL :: Lens' env (MVar LimitOrderDB)

instance HasMockedDatabase App where
  mockDatabaseL = lens mockDatabase (\x y -> x { mockDatabase = y })