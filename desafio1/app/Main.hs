module Main where

import Network.Wai.Handler.Warp (run)

import Server.Swagger (app)

main :: IO ()
main = do 
    run 8080 app
