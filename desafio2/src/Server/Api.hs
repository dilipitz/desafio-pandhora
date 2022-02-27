{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeApplications #-}

module Server.Api where

import Data.Csv
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import RIO.List (headMaybe)
import qualified RIO.Text as T 
import qualified Data.Vector as V 
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (unpack)
import Servant
import Servant.Multipart

import Lib
import RIO (throwM, MonadIO (liftIO))
import Servant.Server.Internal.ServerError (responseServerError)

type API = 
    ("calculate-daily-returns"
    :> MultipartForm Mem (MultipartData Mem) 
    :> Post '[PlainText] String
    :<|> "from-dates"
         :> MultipartForm Mem (MultipartData Mem)
         :> Post '[PlainText] String
    )
    :<|> "calculate-weekly-returns"
         :> MultipartForm Mem (MultipartData Mem)
         :> Post '[PlainText] String

handleCsvFile :: MultipartData Mem -> Either String (V.Vector StockValue)
handleCsvFile mpd = case headMaybe payloads of
    Nothing -> Left "No File Sent"
    Just content -> parseStockValueCsv content
    where
        payloads = fdPayload  <$> files mpd

calculateDailyReturnsHandler :: MultipartData Mem -> Handler String
calculateDailyReturnsHandler mpd = case handleCsvFile mpd of
    Left err -> return err
    Right x -> return . unpack . encode $ V.toList $ calcAllCumulativeReturns x

calcFromDateHandler :: MultipartData Mem -> Handler String
calcFromDateHandler mpd = case handleCsvFile mpd of
    Left err -> return err
    Right x -> return . show $ calcCumulativeReturnFromDates x <$> d1 <*> d2
    where
        d1:d2:_ = parseTime . T.unpack . iValue <$> filter (\i -> iName i == "day1" || iName i == "day2") (inputs mpd)
        parseTime d = parseTimeM True defaultTimeLocale "%d/%m/%Y" d :: Maybe Day

calcWeeklyReturnsHandler :: MultipartData Mem -> Handler String
calcWeeklyReturnsHandler mpd = case handleCsvFile mpd of
    Left err -> return err
    Right x -> return . unpack . encode $ V.toList $ calcAllWeeklyCumulativeReturns x

server :: Server API
server = (calculateDailyReturnsHandler
    :<|> calcFromDateHandler)
    :<|> calcWeeklyReturnsHandler

app :: Application 
app = serve (Proxy @API) server