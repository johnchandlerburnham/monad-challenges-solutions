{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

-- 1: Generating combinations

allPairs :: [a] -> [b] -> [(a, b)]
allPairs (x:xs) ys = map ((,) x) ys ++ allPairs xs ys
allPairs _ _ = [] 

-- 2: Poker hands

data Card = Card (Int, String)

instance Show Card where
  show (Card (n, s)) = show n ++ s

allCards :: [Int] -> [String] -> [Card]
allCards (n:ns) ss = map (Card . ((,) n)) ss ++ allCards ns ss
allCards _ _ = [] 

-- 3: Generalizing pairs and cards

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f (a:as) bs = map (f a) bs ++ allCombs f as bs
allCombs _ _ _ = []

allPairs' :: [a] -> [b] -> [(a, b)]
allPairs' = allCombs (,)

allCards' :: [Int] -> [String] -> [Card]
allCards' = allCombs (\ x y -> Card (x, y)) 

-- 4: Combinations of three things

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f as bs cs = allCombs id (allCombs f as bs) cs

-- 5: Combinations of more things

combStep :: [a -> b] -> [a] -> [b]
combStep fs as = allCombs id fs as

-- Or with combStep as the fundamental function

combStep' :: [a -> b] -> [a] -> [b]
combStep' (f:fs) as  = map f as ++ combStep' fs as
combStep' _ [] = []

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f as bs = combStep (map f as) bs

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f as bs cs = combStep (combStep (map f as) bs) cs
