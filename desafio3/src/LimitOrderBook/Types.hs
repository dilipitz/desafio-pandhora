{-# LANGUAGE NoImplicitPrelude #-}
module LimitOrderBook.Types where

import RIO
import Data.Time (TimeOfDay)

data Direction = Buy | Sell deriving (Show, Eq)

data LimitOrder = LimitOrder 
    { orderID :: !OrderID
    , time :: !TimeOfDay
    , security_symbol :: !Text
    , direction :: !Direction
    , price :: !Double
    , size :: !Integer
    } deriving (Show, Eq)

type OrderID = Text
type BuyingOrders = Vector LimitOrder
type SellingOrders = Vector LimitOrder
type LimitOrderDB = (BuyingOrders, SellingOrders)