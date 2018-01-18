{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude

-- Skipping some exercises

-- 3: Formalizing the Pattern

class Monad m where
  return :: a -> m a
  bind :: m a -> (a -> m b) -> m b

-- 4: Creating Instances

data Maybe a = Just a | Nothing

instance Monad Maybe where
  return a = Just a
  bind Nothing _ = Nothing
  bind (Just a) f = f a

{-
newtype Gen a = Gen (Seed -> (a, Seed))

instance Monad Gen where
  return a = Gen (\ s -> (a, s))
  bind (Gen ga) fgb = Gen $ \ s -> 
    let (ra, s') = ga s 
        (Gen gb) = fgb ra
    in gb s'
-}

instance Monad [] where
  return a = [a]
  bind (a:as) f = (f a) ++ (bind as f)
  bind [] _ = []

newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

instance Monad Gen where
  return a = Gen $ \ s -> (a, s)
  bind g f = Gen $ \ s -> let (ra, s') = runGen g s in runGen (f ra) s'

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = bind ma $ \ a -> bind mb $ \ b -> return $ f a b 

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = 
  bind ma $ \ a -> bind mb $ \ b -> bind mc $ \ c -> return $ f a b c

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = liftM2 id mf ma 

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (m:ms) = liftM2 (:) m (sequence ms)

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind

join :: Monad m => m (m a) -> m a
join ma = bind ma id

-- 6: Using the abstraction
-- skipping


