module Palindroms
  ( isPalindrom
  , bruteForcePalindroms
  , genPalindroms
  )where

import           Data.List
import qualified Data.Set  as S

isPalindrom :: Eq a => [a] -> Bool
isPalindrom x = reverse x == x

-- | Drops the first half of a list (middle element is included, if list length
-- is odd) of a list
takeHalf :: [a] -> [a]
takeHalf l = drop (length l `div` 2) l

-- | Computes all subsets of size k from the given set, boring...
combinaisons :: Ord a => Int -> S.Set a -> [S.Set a]
combinaisons 0 _ = [S.empty]
combinaisons p s =
    case S.maxView s of
        Nothing -> []
        Just (val,sRest) ->
            fmap (S.insert val) (combinaisons (p - 1) sRest) ++
            combinaisons p sRest

insertAt :: Int -> [a] -> [a] -> [a]
insertAt n x l = h ++ x ++ t
  where (h, t) = splitAt n l

replicateAt :: Int -> a -> Int -> [a] -> [a]
replicateAt idx x n = insertAt idx (replicate n x)

sublists :: [(Int, a)] -> [[a]]
sublists [] = [[]]
sublists ((n, x):xs) = do
  l <- sublists xs
  i <- [0..n]
  return $ replicate i x ++ l

bruteForcePalindroms :: Eq a => [(Int, a)] -> [[a]]
bruteForcePalindroms l =
    nub $ concatMap (nub . filter isPalindrom . permutations) (sublists l)

-- | Computes all possible decompositions of the given number as a sum of
-- **ascending** numbers
sumDecomp :: Int -> [[Int]]
sumDecomp x = boundedDecomp x x
  where
    -- | Sum decomposition, with an upper bound
    boundedDecomp :: Int -> Int -> [[Int]]
    boundedDecomp 0 _ = [[]]
    boundedDecomp n nmax = do
        i <- [1 .. (min n nmax)]
        xs <- boundedDecomp (n - i) i
        return $ i : xs

-- | Generate all associations from elements of the given list to the given set
--
-- * The input list must be sorted
--
-- IMPORTANT: it do not generate duplicated mapping
strictAssociations :: (Ord b, Eq a) => [a] -> S.Set b -> [[(a, b)]]
strictAssociations [] _ = [[]]
strictAssociations l@(x:_) s = do
    sub <- combinaisons (length h) s
    rest <- strictAssociations t (S.difference s sub)
    return $ zip h (S.toList sub) ++ rest
  where
    (h,t) = span (== x) l

-- | Generates all possible associations for inserting p times a value into a
-- list of size n
insertions :: Int -> Int -> [[(Int, Int)]]
insertions p n = do
    ps <- sumDecomp p
    xs <- combinaisons (length ps) $ S.fromList [0 .. n]
    map (sortOn snd) $ strictAssociations ps xs -- Sort really needed there ?

-- | Apply an association to a list with a specified element
-- NOTE: must be sorted by ascending insertion indexes
insertAll :: Foldable t => a -> [a] -> t (Int, Int) -> [a]
insertAll x = foldr (\(cnt, idx) pl -> replicateAt idx x cnt pl)

insertExactly :: Int -> a -> [a] -> [[a]]
insertExactly n x l = map (insertAll x l) (insertions n (length l))

allSubinsertions :: Int -> a -> [a] -> [[a]]
allSubinsertions n x l = concatMap (\i -> insertExactly i x l) [0..n]

genOddPalins :: Int -> a -> [[a]] -> [[a]]
genOddPalins n x l = do
    -- TODO: would a recursive call to genEvenPalins do the job ?
    p <- takeHalf <$> filter ((== 0) . (`mod` 2) . length) l
    halfPalin <- allSubinsertions (n `div` 2) x p
    return $ halfPalin ++ [x] ++ reverse halfPalin

genEvenPalins :: Int -> a -> [[a]] -> [[a]]
genEvenPalins n x l = do
    p <- l
    let pHalf = takeHalf p
    if length p `mod` 2 == 0
        then do
            halfPalin <- allSubinsertions (n `div` 2) x pHalf
            return $ halfPalin ++ reverse halfPalin
        else do
            halfPalin <- allSubinsertions (n `div` 2) x (tail pHalf)
            return $ halfPalin ++ [head pHalf] ++ reverse halfPalin

genPalindroms :: [(Int, a)] -> [[a]]
genPalindroms [] = [[]]
genPalindroms ((n,x):xs) =
    genOddPalins (n - 1) x subPalindroms ++ genEvenPalins n x subPalindroms
  where
    subPalindroms = genPalindroms xs
