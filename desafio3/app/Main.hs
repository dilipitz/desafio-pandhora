{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import RIO.Process
import RIO.Vector (fromList)
import Network.Wai.Handler.Warp
import Options.Applicative.Simple
import qualified Paths_desafio3

import qualified Server.Swagger as S 

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_desafio3.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
    )
    empty
  db <- newIORef (fromList [], fromList [])
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          , mockDatabase = db
          }
    --  in runRIO app run
    in run 8080 $ S.app app