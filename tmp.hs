import Control.Monad (liftM2, liftM3)

unless :: Bool -> a -> a -> a
unless True _ y = y
unless False x _ = x

unlessM :: (Monad m) => m Bool -> m a -> m a -> m a
unlessM = liftM3 unless 

concatM :: (Monad m) => m [a] -> m [a] -> m [a]
concatM = liftM2 (++)


