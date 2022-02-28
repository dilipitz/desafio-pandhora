{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeApplications #-}
module Server.Api where

import Import hiding(Handler)
import Servant

import LimitOrderBook.Types
import LimitOrderBook.Database

type LimitOrderAPI = "limitOrder" 
    :> ( Get '[JSON] (Vector LimitOrder)
        :<|> ReqBody '[JSON] LimitOrder :> Post '[JSON] LimitOrder 
        :<|> "byId" :> Capture "id" Text :> 
            ( Get '[JSON] (Maybe LimitOrder)
         :<|> Delete '[JSON] NoContent
            )
        :<|> "createBulk" :> ReqBody '[JSON] [LimitOrder ] :> Post '[JSON] [LimitOrder]
        :<|> "Bids" :> Get '[JSON] (Vector LimitOrder)
        :<|> "Asks" :> Get '[JSON] (Vector LimitOrder)
       )

type MarketOrderAPI = "marketOrder" :> Capture "size" Integer 
    :> ( "buy"  :> Get '[JSON] () 
   :<|>  "sell" :> Get '[JSON] () 
       )


type API = LimitOrderAPI
    :<|> MarketOrderAPI

indexLimitOrdersHandler :: RIO App (Vector LimitOrder)
indexLimitOrdersHandler = indexOrders

addLimitOrderHandler :: LimitOrder -> RIO App LimitOrder
addLimitOrderHandler = createLimitOrder

addBulkLimitOrderHandler :: [LimitOrder] -> RIO App [LimitOrder]
addBulkLimitOrderHandler = createBulkLimitOrder

getByIdHandler :: Text -> RIO App (Maybe LimitOrder) 
getByIdHandler = findOrderById

getBidsHandler :: RIO App (Vector LimitOrder)
getBidsHandler = getBids 

getAsksHandler :: RIO App (Vector LimitOrder)
getAsksHandler = getAsks

marketOrderBuyHandler :: Integer ->  RIO App ()
marketOrderBuyHandler = marketOrderBuy

marketOrderSellHandler :: Integer -> RIO App ()
marketOrderSellHandler = marketOrderSell

cancelOrderHandler :: Text -> RIO App NoContent
cancelOrderHandler = undefined 

server :: ServerT API (RIO App)
server = limitOrderOps
    :<|> marketOrderOps
    where
        limitOrderOps = indexLimitOrdersHandler
            :<|> addLimitOrderHandler
            :<|> limitOrderByIdOps
            :<|> addBulkLimitOrderHandler
            :<|> getBidsHandler
            :<|> getAsksHandler
        
        limitOrderByIdOps oid =
           getByIdHandler oid :<|> cancelOrderHandler oid

        marketOrderOps s =
             marketOrderBuyHandler s :<|> marketOrderSellHandler s

api :: Proxy API
api = Proxy

app :: App -> Application
app env = serve api $ hoistServer api (runRIO env) server