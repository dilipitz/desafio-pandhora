{-# LANGUAGE NoImplicitPrelude #-}
module LimitOrderBook.Database where

import Import hiding ((++))
import RIO.Vector (find, (++))
import LimitOrderBook.Types as T
import qualified LimitOrderBook.Utils as T
import Servant.API (NoContent (NoContent))


-- | Helper to modify the persistant pseudo-datbase
withDatabase :: (MonadReader env m, HasMockedDatabase env, MonadIO m)
    => (LimitOrderDB -> LimitOrderDB) -> m ()
withDatabase fn = do
    env <- ask
    let ioref = view mockDatabaseL env
    modifyIORef' ioref fn

findOrderById :: (MonadReader env m, HasMockedDatabase env, MonadIO m)
    => OrderID -> m (Maybe LimitOrder)
findOrderById oid = do
    env <- ask
    let ioref = view mockDatabaseL env
    (l, r) <- readIORef ioref
    return $ find (\x -> orderID x == oid) (l ++ r)

indexOrders :: (MonadReader env m, HasMockedDatabase env, MonadIO m)
    => m (Vector LimitOrder)
indexOrders = do
    env <- ask
    let ioref = view mockDatabaseL env
    (l, r) <- readIORef ioref
    return $ l ++ r

createLimitOrder :: (MonadReader env m, HasMockedDatabase env, MonadIO m)
    => LimitOrder -> m LimitOrder
createLimitOrder order = do
    withDatabase (T.addLimitOrder order)
    return order

createBulkLimitOrder :: (MonadReader env m, HasMockedDatabase env, MonadIO m)
    => [LimitOrder] -> m [LimitOrder]
createBulkLimitOrder orders = do
    withDatabase (`T.addBulkLimitOrder` orders)
    return orders

getBids :: (MonadReader env m, HasMockedDatabase env, MonadIO m)
    => m (Vector LimitOrder)
getBids = do
    env <- ask
    let ioref = view mockDatabaseL env
    (l, _) <- readIORef ioref
    return l

getAsks :: (MonadReader env m, HasMockedDatabase env, MonadIO m)
    => m (Vector LimitOrder)
getAsks = do
    env <- ask
    let ioref = view mockDatabaseL env
    (_, r) <- readIORef ioref
    return r

marketOrderBuy :: (MonadReader env m, HasMockedDatabase env, MonadIO m)
    => Integer -> m ()
marketOrderBuy s = withDatabase $ T.addMarketOrder Buy s

marketOrderSell :: (MonadReader env m, HasMockedDatabase env, MonadIO m)
    => Integer -> m ()
marketOrderSell s = withDatabase $ T.addMarketOrder Sell s

deleteLimitOrder :: (MonadReader env m, HasMockedDatabase env, MonadIO m)
    => Text -> m NoContent
deleteLimitOrder oid = do
    orderM <- findOrderById oid
    case orderM of
        Nothing -> return NoContent
        Just LimitOrder {direction=d} -> do
            withDatabase $ T.cancelLimitOrder oid d
            return NoContent