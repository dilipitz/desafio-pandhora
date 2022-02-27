{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Lib
    ( fibonacci
    , gcd
    , isPrime
    , isPalindrome
    ) where

import Prelude hiding (gcd)
import GHC.Natural ( Natural, naturalFromInteger )
import Data.Char (toLower)

-- | Binet's formula implementation to calculate the nth fibonacci number
fibonacci :: Natural -> Natural
fibonacci n = round $ phi ** fromIntegral n / sq5
  where
    sq5 = sqrt 5 :: Double
    phi = (1 + sq5) / 2

-- | Euclidian algorithm implementation to calculate the greatest common divisor
gcd :: Natural -> Natural -> Natural
gcd x 0 = x
gcd x y = gcd b (mod a b)
  where a = x
        b = y

-- | Check whether a number is prime or not
isPrime :: Natural -> Bool 
isPrime k
  | k > 1 = null [x | x <- [2.. floor . sqrt $ fromIntegral k], k `mod` (naturalFromInteger x) == 0]
  | otherwise = False


-- | Check whether a word is a palindrome or not
isPalindrome :: String -> Bool 
isPalindrome xs = s == reverse s
  where s = map toLower xs

