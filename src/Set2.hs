{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

-- 1: The Maybe Type
data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just a) = "Just " ++ (show a)

-- 2: Build a library of things that can fail

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

-- 3: Chains of Failing Computations

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gData str = bindM id $ apM (fmapM divMay maxv) headv
  where 
    v = lookupMay str gData
    maxv = fmapM fromIntegral $ bindM maximumMay $ bindM tailMay v
    headv = fmapM fromIntegral $ bindM headMay v
    fmapM :: (a -> b) -> Maybe a -> Maybe b
    fmapM f m = case m of
      Just x -> Just $ f x
      Nothing -> Nothing
    apM :: Maybe (a -> b) -> Maybe a -> Maybe b
    apM mf ma = case mf of
      Just f -> fmapM f ma
      Nothing -> Nothing
    bindM :: (a -> Maybe b) -> Maybe a -> Maybe b
    bindM f ma = case (fmapM f ma) of
      Just (Just x) -> Just x
      _ -> Nothing

--- 4: Generalizing chains of failures

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f (Just a) = f a
chain f Nothing = Nothing

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link ma f = chain f ma

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gData str = chain (\ x -> chain (divMay x) headv) maxv
  where 
  v = lookupMay str gData
  maxv = chain (Just . fromIntegral) $ chain maximumMay $ chain tailMay v
  headv = chain (Just . fromIntegral) $ chain headMay v

-- 5: Chaining variations
addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries sals a b = 
  chain (\ x -> chain (Just . (+x)) (lookupMay b sals)) (lookupMay a sals)
    
yLink' :: (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
yLink' f a b = chain ( \ x -> chain (f x) b) a

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f a b = chain ( \ x -> chain (Just . (f x)) b) a

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 sals a b = yLink (+) (lookupMay a sals) (lookupMay b sals)

mkMaybe :: a -> Maybe a
mkMaybe a = Just a

-- 6: Tailprod

tailProd :: Num a => [a] -> Maybe a
tailProd [] = Nothing
tailProd (x:xs) = Just $ product xs

tailSum :: Num a => [a] -> Maybe a
tailSum [] = Nothing
tailSum (x:xs) = Just $ sum xs

transMaybe' :: ([a] -> b) -> [a] -> Maybe b
transMaybe' _ [] = Nothing
transMaybe' f xs = Just $ f xs

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe _ Nothing = Nothing
transMaybe f (Just x) = Just $ f x

tailProd' :: Num a => [a] -> Maybe a
tailProd' x = transMaybe product $ tailMay x

tailSum' :: Num a => [a] -> Maybe a
tailSum' x = transMaybe sum $ tailMay x

tailMax :: Ord a => [a] -> Maybe a
tailMax x = chain maximumMay (tailMay x) 

tailMin :: Ord a => [a] -> Maybe a
tailMin x = chain minimumMay (tailMay x) 

combine :: Maybe (Maybe a) -> Maybe a
combine x = chain id x



