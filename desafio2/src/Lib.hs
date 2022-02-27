{-# LANGUAGE DataKinds, FlexibleInstances #-}

module Lib where

import Data.Time( Day, parseTimeM, defaultTimeLocale, diffDays, addDays, fromGregorian)
import Data.Time.Calendar (showGregorian, DayOfWeek(..), dayOfWeek)
import Data.Csv
    ( decodeWith,
      defaultDecodeOptions,
      FromField(..),
      ToField(..),
      ToRecord,
      HasHeader(HasHeader) )
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Char8 (unpack, pack, ByteString)
import qualified Data.Vector as V
import Data.Ord (comparing)
import Data.Vector.Algorithms.Intro (sortBy)
import Numeric.Decimal
import Data.Scientific
import GHC.Base (join)

type CumulativeReturn = Decimal RoundHalfEven 4 Integer
-- ^ type for safe money arithmetic with precision up to 4 decimals

type StockValue = (Day, Scientific)
-- ^ type that represents the time series of a stock price 

type StockDailyReturn = (Day, Scientific , Maybe CumulativeReturn )
-- ^ type that represents the time series of a stock daily cumulative return

type StockWeeklyReturn = (Day, Scientific, Maybe CumulativeReturn, Maybe CumulativeReturn, Maybe CumulativeReturn)
-- ^ type that represents the time series of a stock weekly cumulative return

-- | Checks if inputed Days are sequential, without regarding input order
isNextDay :: Day -> Day -> Bool
isNextDay x y =  abs (diffDays x y) == 1

-- | Parse a stock csv with header included
parseStockValueCsv :: BL.ByteString  -> Either String (V.Vector StockValue)
parseStockValueCsv = decodeWith defaultDecodeOptions HasHeader

-- | Sort StockValue by date
sortAscByDate :: V.Vector StockValue -> V.Vector StockValue
sortAscByDate = V.modify (sortBy (comparing fst))

-- | Calculates the cumulative return of the stock with the formula day_price / previous_day_price - 1
calcCumulativeReturn :: Scientific -> Scientific -> Maybe CumulativeReturn
calcCumulativeReturn x y = do 
    p1 <- fromScientificDecimal x
    p2 <- fromScientificDecimal y
    arithMaybe $ divideDecimalWithRounding p1 p2 - 1

-- | Calculates all cumulative returns of the stock, this should be used with the parsed csv/xml
calcAllCumulativeReturns :: V.Vector StockValue -> V.Vector StockDailyReturn
calcAllCumulativeReturns xs = mapToStockReturns `V.imap` sortedByDate 
    where
        sortedByDate = sortAscByDate xs
        mapToStockReturns :: Int -> StockValue -> StockDailyReturn
        mapToStockReturns 0 (day, price) = (day, price, Nothing)
        mapToStockReturns i (day, price) = 
            let (prev_day, prev_price) = sortedByDate V.! (i - 1)
            in (day, price, calcCumulativeReturn price prev_price)

-- | Calculates cumulative returns of the stock based on two arbitrary dates
calcCumulativeReturnFromDates :: V.Vector StockValue -> Day -> Day -> Maybe CumulativeReturn
calcCumulativeReturnFromDates xs x y 
    | x == y = Nothing
    | otherwise = join $ calcCumulativeReturn <$> p_term <*> p_ini
    where
        findNext d = findFutureDate xs d
        (d1, d2) 
            | x > y = (y, x)
            | otherwise = (x, y)
        p_ini = snd <$> findNext d1
        p_term = snd <$> findNext d2

calcCumulativeWeeklyReturn :: V.Vector StockValue -> Day -> Maybe CumulativeReturn
calcCumulativeWeeklyReturn xs d = case date_diff of 
        Nothing -> Nothing 
        Just diff
            | diff < 7 -> Nothing
            | otherwise -> calc d =<< init_date
    where 
        calc = calcCumulativeReturnFromDates xs
        init_date = fst <$> findFutureDate xs (addDays (-7) d)
        date_diff = diffDays d <$> init_date

-- calcCumulativeReturnOnWeek :: V.Vector StockValue -> Day -> Maybe CumulativeReturn
-- calcCumulativeReturnOnWeek xs d = findLastPriceOfPreviousWeek xs d

diffToMonday :: DayOfWeek -> Int
diffToMonday wd = fromEnum wd - monday 
    where
        monday = fromEnum Monday  

getPreviousMondayDate :: Day -> Day
getPreviousMondayDate d = addDays (- toInteger (diffToMonday (dayOfWeek d)) - 7) d

getPreviousWeekDates :: V.Vector StockValue -> Day -> V.Vector StockValue
getPreviousWeekDates wds d =  V.filter (\(d', _) -> (addDays 6 d) >= d') wds

calcCumulativeReturnOnWeek :: V.Vector StockValue -> Day -> Maybe CumulativeReturn
calcCumulativeReturnOnWeek wds d =
    case prev_first_value of 
        Nothing -> Nothing
        Just init -> calc d (fst (value init)) 
    where
        calc = calcCumulativeReturnFromDates wds
        prev_monday = getPreviousMondayDate d
        prev_week_days = getPreviousWeekDates wds prev_monday
        prev_first_value = findFutureDate prev_week_days prev_monday
        value init = V.foldl (\x@(d1, _) y@(d2, _) -> if d1 > d2 then x else y ) init prev_week_days

findFutureDate :: V.Vector StockValue -> Day -> Maybe StockValue
findFutureDate xs d = V.find (\(d', _) -> diffDays d d' <= 0) sortedByDate
    where sortedByDate = sortAscByDate xs

calcAllWeeklyCumulativeReturns :: V.Vector StockValue -> V.Vector StockWeeklyReturn
calcAllWeeklyCumulativeReturns wds = (\(d, price, cumulative_returns) -> (d, price, cumulative_returns, calcCumulativeWeeklyReturn wds d,  calcCumulativeReturnOnWeek wds d)) <$> cumulativeReturns
    where
        cumulativeReturns = calcAllCumulativeReturns wds

instance Ord DayOfWeek 

instance FromField Day where
    parseField = parseTimeM True defaultTimeLocale "%Y/%m/%d" . unpack

-- instance ToRecord StockDailyReturn
instance ToField Day where
    toField = pack . showGregorian

instance ToField CumulativeReturn where
    toField = pack . show