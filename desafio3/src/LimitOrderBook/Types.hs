{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
module LimitOrderBook.Types where

import RIO
import Data.Aeson
import Data.Time (TimeOfDay)
import Data.Swagger (ToSchema)

data Direction = Buy | Sell deriving (Show, Eq, Generic)

data LimitOrder = LimitOrder 
    { orderID :: !OrderID
    , time :: !TimeOfDay
    , security_symbol :: !Text
    , direction :: !Direction
    , price :: !Double
    , size :: !Integer
    } deriving (Show, Eq, Generic)


type OrderID = Text
type BuyingOrders = Vector LimitOrder
type SellingOrders = Vector LimitOrder
type LimitOrderDB = (BuyingOrders, SellingOrders)

instance FromJSON Direction
instance ToJSON Direction

instance ToSchema Direction

instance FromJSON LimitOrder
instance ToJSON LimitOrder

instance ToSchema LimitOrder