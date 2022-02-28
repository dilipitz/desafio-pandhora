{-# LANGUAGE NoImplicitPrelude #-}

module LimitOrderBook.Utils where

import Import
import LimitOrderBook.Types
import RIO.Vector as V (modify, cons, filter, toList, fromList)
import Data.Vector.Algorithms.Intro (sortBy)

-- | Sorts a LimitOrder Vector by Bids rules
sortBids :: Vector LimitOrder -> Vector LimitOrder
sortBids = modify (sortBy (flip (comparing price) <> comparing time) )

-- | Sorts a LimitOrder Vector by Asks rules
sortAsks :: Vector LimitOrder -> Vector LimitOrder
sortAsks = modify (sortBy (comparing price <> comparing time) )

-- | Removes a LimitOrder from the pseudo-database
cancelLimitOrder :: OrderID -> Direction -> LimitOrderDB -> LimitOrderDB
cancelLimitOrder oid dir (buying, selling) = case dir of
    Buy  -> (filterOutOrder buying, selling)
    Sell -> (buying, filterOutOrder selling)
    where 
        filterOutOrder = V.filter (\o -> orderID o /= oid) 

-- | Adds a LimitOrder to the correct size of the pseudo-database depending on the LimitOrder direction
addLimitOrder :: LimitOrder -> LimitOrderDB  -> LimitOrderDB
addLimitOrder order (buying, selling) = case direction order of
    Buy ->  (sortBids $ cons order buying, selling)
    Sell -> (buying, sortAsks $ cons order selling)

-- | Helper to add LimitOrder in bulks
addBulkLimitOrder ::LimitOrderDB -> [LimitOrder] -> LimitOrderDB
addBulkLimitOrder = foldl' (flip addLimitOrder)

-- | Helper to modify the persistant pseudo-datbase
withDatabase :: (MonadReader env m, HasMockedDatabase env, MonadIO m) 
    => (LimitOrderDB -> LimitOrderDB) -> m ()
withDatabase fn = do
    env <- ask
    let mvar = view mockDatabaseL env
    db <- takeMVar mvar
    putMVar mvar $ fn db

-- | Helper to subtract LimitOrder's size depending on a given Size
subtractSizeFromOrders :: Integer -> Vector LimitOrder -> Vector LimitOrder
subtractSizeFromOrders sz vs = V.fromList $ f' sz (V.toList vs)
    where 
        f' 0 xs = xs
        f' _ [] = []
        f'  s (x@LimitOrder {size = s'}: xs) 
            | s' - s <= 0 = f' (abs (s' - s)) xs
            | otherwise = f' 0 (x{size = s' - s}:xs)

addMarketOrder :: Direction -> Integer -> LimitOrderDB -> LimitOrderDB
addMarketOrder d s (buying, selling) = case d of
    Buy  -> (buying, subtractSizeFromOrders s selling)
    Sell -> (subtractSizeFromOrders s buying, selling)