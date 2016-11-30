module Main where

import Criterion.Main
import Palindroms

input0 :: [(Int, Char)]
input0 = [(1, 'a'), (3, 'b'), (1, 'c'), (1, 'd'), (1, 'e')]

input1 :: [(Int, Char)]
input1 = [(2, 'a'), (1, 'b'), (1, 'c'), (3, 'd'), (1, 'e'), (4, 'f')]

input2 :: [(Int, Char)]
input2 =
  [ (5, 'a')
  , (2, 'b')
  , (3, 'c')
  , (7, 'd')
  , (6, 'e')
  , (1, 'f')
  , (1, 'g')
  , (1, 'h')
  ]

-- input3 :: [(Int, Char)]
-- input3 =
--   [ (2, 'a')
--   , (3, 'b')
--   , (5, 'c')
--   , (3, 'd')
--   , (3, 'e')
--   , (2, 'f')
--   , (1, 'g')
--   , (2, 'h')
--   , (3, 'i')
--   , (4, 'j')
--   , (1, 'k')
--   , (3, 'l')
--   ]

-- Our benchmark harness.
main :: IO ()
main =
  defaultMain
    [ bgroup
        "palindroms"
        [ bench "input0" $ whnf genPalindroms input0
        , bench "input1" $ whnf genPalindroms input1
        , bench "input2" $ whnf genPalindroms input2
        -- , bench "input3" $ whnf genPalindroms input3
        ]
    ]
