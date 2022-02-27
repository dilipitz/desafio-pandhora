{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Server.Api where

import Prelude hiding (gcd)
import GHC.Natural (Natural)
import Servant
import Servant.Swagger
import Servant.Swagger.UI

import Lib (fibonacci, gcd, isPrime, isPalindrome)

type API = 
        "fibonacci" 
                :> Summary "Binet's formula implementation to calculate the nth fibonacci number" 
                :> Capture "x" Natural 
                :> Get '[PlainText] String
        :<|> "gcd"
             :> Summary "Euclidian algorithm implementation to calculate the greatest common divisor"
             :> Capture "x" Natural
             :> Capture "y" Natural 
             :> Get '[PlainText] String
        :<|> "is-prime"
             :> Summary "Check whether a number is prime or not" 
             :> Capture "x" Natural 
             :> Get '[PlainText] String
        :<|> "is-palindrome"
             :> Summary "Check whether a word is a palindrome or not" 
             :> Capture "x" String 
             :> Get '[PlainText] String

fibonacciHandler :: Natural -> Handler String 
fibonacciHandler = return . show . fibonacci

gcdHandler :: Natural -> Natural -> Handler String
gcdHandler x = return . show . gcd x

isPrimeHandler :: Natural -> Handler String
isPrimeHandler = return . show . isPrime

isPalindromeHandler :: String -> Handler String
isPalindromeHandler = return . show . isPalindrome

server :: Server API
server = fibonacciHandler
       :<|> gcdHandler
       :<|> isPrimeHandler
       :<|> isPalindromeHandler

api :: Proxy API
api = Proxy

app :: Application 
app = serve api server