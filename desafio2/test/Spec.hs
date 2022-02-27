import Test.Tasty
import Test.Tasty.HUnit

import Data.Time (fromGregorian)
import qualified Data.Vector as V
import Numeric.Decimal

import Lib ( sortAscByDate
           , StockValue
           , StockDailyReturn
           , calcCumulativeReturn
           , CumulativeReturn
           , calcAllCumulativeReturns
           , calcCumulativeReturnFromDates
           , calcCumulativeWeeklyReturn
           , calcAllWeeklyCumulativeReturns
           )

main :: IO ()
main = do
    -- xml <- BL.readFile "../prices.xml"
    -- let prices = parseXML xml
    --     price = concatMap (findElements $ simpleName "Price") (onlyElems prices)
    --     date  = map (findElement $ simpleName "Date") price
    --     value = map (findElement $ simpleName "Value") price
    --     simpleName s = QName s Nothing Nothing
    -- print price
    -- print date
    -- print value
    -- csv <- BL.readFile "../prices.csv"
    -- case parseStockValueCsv csv of
    --     Left err -> print err
    --     Right xs  -> do
    --         -- print xs
    --         -- print $ V.length xs
    --         -- print $ (\ i v -> if i - 1 < 0 then Nothing else Just $ (snd v / snd (x V.! (i - 1))) - 1) `V.imap` x
    --         let getNext i = xs V.!? (i + 1)
    --         print $ (\ i (currDate, currPrice) -> 
    --             case getNext i of
    --                 Nothing -> Nothing
    --                 Just (nextDate, nextPrice) 
    --                     | (diffDays currDate nextDate) >= 0 -> Just $ calcCumulativeReturn currPrice nextPrice
    --                     | otherwise -> Just $ calcCumulativeReturn nextPrice currPrice
    --             ) `V.imap` xs
    print $ calcAllWeeklyCumulativeReturns dwv
    defaultMain $ testGroup "" 
        [ sortByDateTest
        , calcCumulativeReturnTest
        , calcAllCumulativeReturnTest
        , calcCumulativeReturnFromDatesTest
        , calcCumulativeWeeklyReturnTest
        ]
    -- where
    --     calcCumulativeReturn p1 p2 = p1 / p2 - 1

calcCumulativeReturnTest :: TestTree
calcCumulativeReturnTest = testGroup "calcCumulativeReturn" 
    [ testCase "division by zero" $ 
        calcCumulativeReturn 1 0 @?= Nothing
    , testCase "formula: x / y - 1 test" $
        calcCumulativeReturn 11.0 12.0 @?= ( fromScientificDecimal (-0.0833) :: Maybe CumulativeReturn)
    ]

dv1,dv2,dv3,dv4,dv5 :: StockValue  
dv1 = (fromGregorian 2022 01 01, 10)
dv2 = (fromGregorian 2022 01 02, 12)
dv3 = (fromGregorian 2022 01 04, 11)
dv4 = (fromGregorian 2022 01 05, 6)
dv5 = (fromGregorian 2022 01 06, 24)

dr1,dr2,dr3,dr4,dr5 :: StockDailyReturn  
dr1 = (fromGregorian 2022 01 01, 10, Nothing)
dr2 = (fromGregorian 2022 01 02, 12, fromScientificDecimal 0.2)
dr3 = (fromGregorian 2022 01 04, 11, fromScientificDecimal (-0.0833))
dr4 = (fromGregorian 2022 01 05, 6,  fromScientificDecimal (-0.4546))
dr5 = (fromGregorian 2022 01 06, 24, fromScientificDecimal 3)

sortByDateTest :: TestTree 
sortByDateTest = 
    let unorderedDatesV = V.fromList [dv5, dv2, dv3, dv4, dv1]
    in
        testCase "sortAscByDate" $ sortAscByDate unorderedDatesV @?= V.fromList [dv1, dv2, dv3, dv4, dv5]

calcAllCumulativeReturnTest :: TestTree
calcAllCumulativeReturnTest = 
    let vec_input = V.fromList [dv1, dv2, dv3, dv4, dv5]
        vec_expected = V.fromList [dr1, dr2, dr3, dr4, dr5]
    in
        testCase "calcAllCumulativeReturns" $ calcAllCumulativeReturns vec_input @?= vec_expected

calcCumulativeReturnFromDatesTest :: TestTree 
calcCumulativeReturnFromDatesTest = 
    let vec_input = V.fromList [dv1, dv2, dv3 ,dv4, dv5]
        initial_date = fromGregorian 2022 01 03
        term_date    = fromGregorian 2022 01 06
        calc_with_input = calcCumulativeReturnFromDates vec_input 
    in
        testGroup "calcCumulativeReturnFromDate"
            [ testGroup "any input order is valid"
                -- this test is the same as calculating dates 04/01/06 and 06/01/06
                [ testCase  "03/01/06 and 06/01/22" $ calc_with_input initial_date term_date @?= (fromScientificDecimal 1.1818 :: Maybe CumulativeReturn)
                , testCase  "06/01/22 and 03/01/22" $ calc_with_input term_date initial_date @?= (fromScientificDecimal 1.1818 :: Maybe CumulativeReturn)
                ]
            , testCase "equal dates returns nothing" $ calc_with_input initial_date initial_date @?= Nothing
            ]

dw1, dw2, dw3, dw4, dw5, dw6, dw7, dw8, dw9, dw10 :: StockValue
dw1 = (fromGregorian 2022 02 07, 10)
dw2 = (fromGregorian 2022 02 08, 11)
dw3 = (fromGregorian 2022 02 09, 12)
dw4 = (fromGregorian 2022 02 10, 13)
dw5 = (fromGregorian 2022 02 14, 14)
dw6 = (fromGregorian 2022 02 15, 15)
dw7 = (fromGregorian 2022 02 16, 16)
dw8 = (fromGregorian 2022 02 17, 17)
dw9 = (fromGregorian 2022 02 18, 18)
dw10 = (fromGregorian 2022 02 21, 19)

dwv :: V.Vector StockValue 
dwv = V.fromList [dw1, dw2, dw3, dw4, dw5, dw6, dw7, dw8, dw9, dw10]

calcCumulativeWeeklyReturnTest :: TestTree 
calcCumulativeWeeklyReturnTest = 
    testGroup "calcCumulativeWeeklyReturn" 
    [ testCase "14/02/2022" $ calcCumulativeWeeklyReturn dwv (fromGregorian 2022 02 14) @?= (fromScientificDecimal 0.4  :: Maybe CumulativeReturn)
    , testCase "10/02/2022" $ calcCumulativeWeeklyReturn dwv (fromGregorian 2022 02 10) @?= Nothing
    ]

-- calcAllWeeklyCumulativeReturnsTest :: TestTree 
-- calcAllWeeklyCumulativeReturnsTest =
--     let 
--     testCase "calcAllWeeklyCumulativeReturns" $ calcAllWeeklyCumulativeReturns dwv @?= 


-- findLastPriceOfPreviousWeekTest :: TestTree 
-- findLastPriceOfPreviousWeekTest = 
--     testCase "findLastPriceOfPreviousWeek" $ findLastPriceOfPreviousWeek dwv (fst dw10) @?= Just (fromScientificDecimal (-0.3214)) 
-- isNextDayTest :: TestTree 
-- isNextDayTest =
--     let day1 = fromGregorian 2022 02 01
--         day2 = fromGregorian 2022 02 02
--         day3 = fromGregorian 2022 01 02
--     in
--     testGroup "isNextDay"
--         [ testGroup "is following dates" 
--             [ testCase "day1 and day2" $ isNextDay day2 day1 @?= True
--             , testCase "day2 and day1" $ isNextDay day1 day2 @?= True
--             ]
--         , testCase "is not following dates" $ isNextDay day3 day1 @?= False
--         ]

-- parseDateTest :: TestTree 
-- parseDateTest = testCase "2022/02/26" $ parseDate "2022/02/26" @?= Just (fromGregorian 2022 02 26)

