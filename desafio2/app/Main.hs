{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Handler.Warp
import Server.Api


main :: IO ()
main = do
    run 8080 app 
    -- csv <- BL.readFile "../prices.csv"
    -- let result :: Either String (V.Vector (Day, Double))
    --     result = decodeWith defaultDecodeOptions HasHeader csv
    -- case result of
    --     Left err -> print err
    --     Right x  -> do
    --         print x
    --         print $ V.length x
    -- case decode HasHeader csv of
    --     Left err -> putStrLn err
    --     Right v  -> V.forM_ v $ \ Stock {price = p, date=d} -> do
    --         print p
    --         print d