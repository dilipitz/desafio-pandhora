{-# LANGUAGE NoImplicitPrelude #-}
import Prelude hiding (gcd)
import Test.Tasty
import Test.Tasty.HUnit

import Lib (fibonacci, gcd, isPrime, isPalindrome)

main :: IO ()
main = defaultMain $ testGroup "" [ fibTest, gcdTest, isPrimeTest, isPalindromeTest]

fibTest :: TestTree
fibTest = testGroup "fibonacci" 
    [ testCase "0"  $ fibonacci 0 @?= 0
    , testCase "1"  $ fibonacci 1 @?= 1
    , testCase "2"  $ fibonacci 2 @?= 1
    , testCase "49" $ fibonacci 49 @?= 7778742049
    , testCase "50" $ fibonacci 50 @?= 12586269025
    ]

gcdTest :: TestTree 
gcdTest = testGroup "gcd" 
    [ testCase "7 8" $ gcd 7 8 @?= 1
    , testCase "875 455" $ gcd 875 455 @?= 35
    ]

isPrimeTest :: TestTree
isPrimeTest = testGroup "isPrime"
    [ testCase "0" $ isPrime 0 @?= False
    , testCase "1" $ isPrime 1 @?= False
    , testCase "7" $ isPrime 7 @?= True
    , testCase "9566833901" $ isPrime 9566833901 @?= True
    , testCase "9566833903" $ isPrime 9566833903 @?= False
    ]

isPalindromeTest :: TestTree 
isPalindromeTest = testGroup "isPalindrome"
    [ testCase "cac" $ isPalindrome "cac" @?= True
    , testCase "Emil peed deep lime" $ isPalindrome "Emil peed deep lime" @?= True
    , testCase "palindrome" $ isPalindrome "palindrome" @?= False
    ]