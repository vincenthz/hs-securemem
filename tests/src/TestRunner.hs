module Main where

import qualified Data.SecureMemTests
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "SecureMem Tests" [
                    Data.SecureMemTests.defaultTestGroup
                ]

