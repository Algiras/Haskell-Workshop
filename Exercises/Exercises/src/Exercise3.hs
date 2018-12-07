{-# LANGUAGE RankNTypes #-}
module Exercise3 where

newtype Reader a b = Reader{run :: a -> b}

instance Functor (Reader a) where
    fmap fn (Reader run) = Reader $ fn . run

instance Applicative (Reader a) where
    pure a = Reader $ const a
    (Reader fn) <*> (Reader run) = Reader $ \v -> fn v (run v)

instance Monad (Reader a) where
    (Reader fna) >>= fn = Reader $ \w -> run (fn (fna w)) w
-----------------------------------------------------------------

data Writer a b = Result a b deriving (Eq, Show)

instance Functor (Writer a) where
    fmap fn (Result a b) = Result a (fn b)

instance Monoid a => Applicative (Writer a) where
    pure = Result mempty
    (Result acm fn) <*> (Result acm2 el) = Result (acm <> acm2) (fn el)

instance Monoid a => Monad (Writer a) where
    (Result acc v) >>= fn = case fn v of
        Result acc2 v2 -> Result (acc <> acc2) v2

------------------------------------------------------------------
newtype State s a = State{runState :: s -> (a, s)}

instance Functor (State s) where
    fmap fn (State runState) = State $ \s -> let (v, ns) = runState s
        in (fn v, ns)

instance Applicative (State s) where
    pure v = State $ \s -> (v, s)
    (State s1Run) <*> (State s2Run) = State $ \s -> let 
        (fn, s1) = s1Run s
        (v1, s2) = s2Run s1
        in (fn v1, s2)

instance Monad (State s) where
    (State sRun) >>= fn = State $ \v -> let
        (v1, s1) = sRun v
        newState = fn v1
        in runState newState v