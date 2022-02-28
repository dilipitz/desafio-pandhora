{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeApplications #-}

module Server.Swagger where

import Import
import Servant
import Servant.Swagger
import Servant.Swagger.UI

import qualified Server.Api as A

type API = SwaggerSchemaUI "swagger-ui" "swagger.json"
        :<|> A.API

server = swaggerSchemaUIServerT (toSwagger $ Proxy @A.API)
      :<|> A.server


api :: Proxy API
api = Proxy

app :: App -> Application
app env = serve api $ hoistServer api (runRIO env) server