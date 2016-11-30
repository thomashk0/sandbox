module TestPalindroms (testPalindroms) where

import           Data.List             (nub, sort)

import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import           Palindroms

-- | Returns True if the list does not contain duplicates
isNub :: Eq a => [a] -> Bool
isNub x = nub x == x

genDecomposition :: Int -> QC.Gen [Int]
genDecomposition 0 = return []
genDecomposition n = do
  i <- QC.choose (1, n)
  rest <- genDecomposition (n - i)
  return $ i : rest

palinInput :: QC.Gen [(Int, Char)]
palinInput = do
  n <- QC.choose (1, 10)
  l <- genDecomposition n
  return $ zip l alphabet
  where
    alphabet = ['a' .. 'z']

testPalindroms :: TestTree
testPalindroms =
  testGroup
    "palindrom generation"
    [ QC.testProperty "there is no duplicates in generated palindroms" $
      QC.forAll palinInput (isNub . genPalindroms)
    , QC.testProperty "optimized version matches the bruteforce one" $
      QC.forAll
        palinInput
        (\s -> sort (bruteForcePalindroms s) == sort (genPalindroms s))
    ]
