{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

-- 1: Random Number Generation
fiveRands :: [Integer]
fiveRands = go 5 (mkSeed 1) [] 
  where
    go 0 _ rs = rs
    go n s rs = go (n - 1) (snd $ rand s) ((fst $ rand s):rs)

fiveRandsProduct :: Integer
fiveRandsProduct = 8681089573064486461641871805074254223660

fiveRandsCorrect :: Bool
fiveRandsCorrect = foldr (*) 1 fiveRands == fiveRandsProduct 


-- 2: Random Character Generation
randLetter' :: Seed -> (Char, Seed)
randLetter' s = let (r, s') = rand s in (toLetter r, s')

randString3 :: String
randString3 = 
  map fst $ take 3 $ iterate (randLetter . snd) (randLetter (mkSeed 1))

-- 3: More generators

type Gen a = Seed -> (a, Seed)

randLetter :: Gen Char
randLetter s = let (r, s') = rand s in (toLetter r, s')

randEven :: Gen Integer
randEven s = let (r, s') = rand s in (2 * r, s')

randOdd :: Gen Integer
randOdd s = let (r, s') = randEven s in (r + 1, s')

randTen :: Gen Integer
randTen s = let (r, s') = rand s in (10 * r, s')

generalA' :: (Integer -> b) -> Gen b
generalA' f s = let (r, s') = rand s in (f r, s')

generalA :: (a -> b) -> Gen a -> Gen b
generalA f ga = \ s -> let (r, s') = ga s in (f r, s')

randsProduct = 189908109902700

randsCorrect = foldr (*) 1 rands == randsProduct
  where
   rands = map fst [randEven (mkSeed 1), randOdd (mkSeed 1), randTen (mkSeed 1)]

-- 4: Generalizing random pairs

randPair :: Gen (Char, Integer)
randPair = \ s -> 
  let (rChr, s')  = randLetter s
      (rInt, s'') = rand s'
  in  ((rChr, rInt), s'')

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb = \ s -> 
  let (ra, s')  = ga s
      (rb, s'') = gb s'
  in  ((ra, rb), s'')

pairTest = randPair (mkSeed 1) == generalPair randLetter rand (mkSeed 1)

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f ga gb = \ s -> 
  let (ra, s')  = ga s
      (rb, s'') = gb s'
  in  (f ra rb, s'')

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (,)
                               
pairTest2 = randPair (mkSeed 1) == generalPair2 randLetter rand (mkSeed 1)

-- 5: Generalizing Lists of Generators
repRandom1 :: [Gen a] -> Gen [a]
repRandom1 gs = \ s -> go s gs []
  where
    go st [] as = (as, st)
    go st (g:gs) as = let (a, st') = g st in go st' gs (as ++ [a])

repRandom2 :: [Gen a] -> Gen [a]
repRandom2 [] s = ([], s)
repRandom2 (g:gs) s = 
  let (r, s') = g s 
      (rs, s'') = repRandom2 gs s'
  in  (r : rs, s'')

repRandom :: [Gen a] -> Gen [a]
repRandom [] = \ s -> ([], s)
repRandom (g:gs) = generalB (:) g (repRandom gs)

repRandomTest = 
  fst (repRandom (replicate 3 randLetter) (mkSeed 1)) == randString3

-- 6: Threading the random number state

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo ga fgb = \ s -> let (ra, s') = ga s in fgb ra s'

mkGen :: a -> Gen a
mkGen a = \ s -> (a, s)
