{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit
import Import hiding (mockDatabase)
import LimitOrderBook.Types
import Data.Time (TimeOfDay (..))
import Prelude (print)
import RIO.Vector (fromList)
import LimitOrderBook.Types
import LimitOrderBook.Utils
import LimitOrderBook.Database

data MockApp = MockApp
    { mockDatabase :: IORef LimitOrderDB }

instance HasMockedDatabase MockApp where
    mockDatabaseL = lens mockDatabase (\x y -> x { mockDatabase = y })

o1,o2, o3, o4, o5, o6 :: LimitOrder 
o1 = LimitOrder "O101" (TimeOfDay 12 03 18) "INTC" Buy 34.75  1
o2 = LimitOrder "0102" (TimeOfDay 12 00 00) "INTC" Buy 33.75  2
o3 = LimitOrder "0103" (TimeOfDay 12 02 00) "INTC" Buy 33.75  3
o4 = LimitOrder "O104" (TimeOfDay 12 03 18) "INTC" Sell 50.00 1
o5 = LimitOrder "0105" (TimeOfDay 12 00 00) "INTC" Sell 49.00 2
o6 = LimitOrder "0106" (TimeOfDay 12 02 00) "INTC" Sell 50.00 3

main :: IO ()
main = do
    defaultMain $ testGroup ""
        [ sortBidsTest
        , sortAsksTest
        , withDatabaseTest
        , cancelLimitOrderTest
        , addBulkLimitOrderTest
        , addMarketOrderTest
        , addLimitOrderTest
        ]

sortBidsTest :: TestTree 
sortBidsTest = testGroup "sortBids"
    [ testCase "should order by descending price" $ sortBids (fromList [o3, o1]) @?= fromList [o1, o3]
    , testCase "should order by time when price is equal" $ sortBids (fromList [o3, o2]) @?= fromList [o2, o3]
    ]

sortAsksTest :: TestTree
sortAsksTest = testGroup "sortAsks"
    [ testCase "should order by ascending price" $ sortAsks (fromList [o1, o3]) @?= fromList [o3, o1]
    , testCase "should order by time when price is equal" $ sortAsks (fromList [o3, o2]) @?= fromList [o2, o3]
    ]

withDatabaseTest :: TestTree
withDatabaseTest =
    testCase "withDatabase" $ do
        db <- newIORef (fromList [], fromList [])
        let env = MockApp { mockDatabase = db }
        runReaderT (withDatabase (addLimitOrder o1) ) env
        res <- readIORef $ view mockDatabaseL env
        res @?= (fromList [o1],fromList [])

cancelLimitOrderTest :: TestTree 
cancelLimitOrderTest = testCase "cancelLimitOrder" $ do
    let initial = (fromList [o1, o2, o3], fromList [o4, o5, o6])
        expected = (fromList [o2, o3], fromList [o4, o5, o6])
    cancelLimitOrder "O101" Buy initial @?= expected

addLimitOrderTest :: TestTree
addLimitOrderTest = testCase "addLimitOrder" $ do
    let initial = (fromList [], fromList [])
        expected = (fromList [o1], fromList [])
    addLimitOrder o1 initial @?= expected

addBulkLimitOrderTest :: TestTree
addBulkLimitOrderTest = testCase "addBulkLimitOrder" $ do
    let initial = (fromList [], fromList [])
        expected = (fromList [o2, o3], fromList [o6, o4])
    addBulkLimitOrder initial [o6, o2, o4, o3] @?= expected

addMarketOrderTest :: TestTree
addMarketOrderTest = testGroup "addMarketOrder" 
    [ testCase "Sell removes from Bids" $ do
        let initial = (fromList [o2, o3], fromList [])
            expected = (fromList[o3 {size = 1}], fromList [])
        addMarketOrder Sell 4 initial @?= expected
    , testCase "Buy removes from Asks" $ do
        let initial = (fromList [o2, o3], fromList [o5, o6])
            expected = (fromList[o2, o3], fromList [])
        addMarketOrder Buy 5 initial @?= expected

    ]