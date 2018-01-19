{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set5 where

import MCPrelude

-- 1: Do Notation

liftM :: Monad m => (a -> b) -> a -> m b
liftM f = return . f

-- 2: Do Notation - operators

class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a

-- 3: Do Notation - Set 1

newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

instance Monad Gen where
  (>>=) g f = Gen $ \ s -> let (ra, s') = runGen g s in runGen (f ra) s'
  return a = Gen $ \ s -> (a, s)

evalGen :: Gen a -> Seed -> a
evalGen ga s = let (a, s') = runGen ga s in a

makeRandom :: Gen Integer
makeRandom = Gen rand

fiveRands :: Gen [Integer]
fiveRands = do
  a <- makeRandom
  b <- makeRandom
  c <- makeRandom
  d <- makeRandom
  e <- makeRandom
  return [a, b, c, d, e]

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = do
  a <- ma
  b <- mb
  return $ f a b

rands :: Integer -> Gen [Integer]
rands 0 = return []
rands n = do
  a <- makeRandom
  liftM2 (:) (return a) $ rands (n - 1)

-- randLetter :: Gen Char
-- randLetter = makeRandom >>= (return . toLetter)

randLetter :: Gen Char
randLetter = do
  a <- makeRandom
  return $ toLetter a

randString :: Integer -> Gen String
randString 0 = return []
randString n = do
  a <- randLetter
  liftM2 (:) (return a) $ randString (n - 1)

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb = do
  a <- ga
  b <- gb
  return $ (a, b)

-- 3: Do Notation - Set 2

-- don't want to import, self contained is better here
data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just a) = "Just " ++ (show a)

instance Monad Maybe where
  (>>=) (Just a) f = f a
  (>>=) Nothing f = Nothing
  return a = Just a

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (a:as) = Just a

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (a:as) = Just as

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay a ((x, y):xys) = if a == x then Just y else lookupMay a xys

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay x 0 = Nothing
divMay x y = Just $ (/) x y

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = Just $ go x xs
  where
    go m [] = m
    go m (x:xs) = if x > m then go x xs else go m xs

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (x:xs) = Just $ go x xs
  where
    go m [] = m
    go m (x:xs) = if x < m then go x xs else go m xs

queryGreek :: GreekData -> String -> Maybe Double
queryGreek greekData string = do
  value <- lookupMay string greekData
  maxValue <- tailMay value >>= maximumMay >>= liftM fromIntegral
  headValue <- headMay value >>= liftM fromIntegral
  divMay maxValue headValue
   
addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries sals a b = do
  aSal <- lookupMay a sals
  bSal <- lookupMay b sals
  return $ aSal + bSal

tailProd :: Num a => [a] -> Maybe a
tailProd a = do
  tail <- tailMay a
  return $ product tail 

tailSum :: Num a => [a] -> Maybe a
tailSum a = do
  tail <- tailMay a
  return $ sum tail 

tailMax :: Ord a => [a] -> Maybe a
tailMax a = do
  tail <- tailMay a
  maximumMay tail 

-- 5: Do Notation - Set 3

data Card = Card (Int, String)

instance Show Card where
  show (Card (n, s)) = show n ++ s

instance Monad [] where
  return a = [a]
  (>>=) (a:as) f = (f a) ++ (>>=) as f
  (>>=) [] _ = []

allPairs :: [a] -> [b] -> [(a, b)]
allPairs as bs = do
  a <- as
  b <- bs
  return $ (a, b)

allCards :: [Int] -> [String] -> [Card]
allCards ns ss = do
  n <- ns
  s <- ss
  return $ Card (n, s)


allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f as bs cs = do
  a <- as
  b <- bs
  c <- cs
  return $ f a b c







