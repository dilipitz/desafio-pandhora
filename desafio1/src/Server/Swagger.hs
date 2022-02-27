{-# LANGUAGE DataKinds, TypeOperators, TypeApplications #-}
module Server.Swagger where

import Servant
import Servant.Swagger
import Servant.Swagger.UI

import qualified Server.Api as A

type API = SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> A.API

server :: Server API
server = swaggerSchemaUIServer (toSwagger $ Proxy @A.API) 
    :<|> A.server

app = serve (Proxy @API) server