module Main (main) where

import Test.Tasty

import TestPalindroms (testPalindroms)

main :: IO ()
main = defaultMain $ testGroup "all tests" [testPalindroms]
