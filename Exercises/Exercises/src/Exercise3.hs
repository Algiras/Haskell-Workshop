{-# LANGUAGE RankNTypes #-}
module Exercise3 where

-- when completed uncomment Spec line 92

newtype Reader a b = Reader{run :: a -> b}

instance Functor (Reader a) where

instance Applicative (Reader a) where

instance Monad (Reader a) where
-----------------------------------------------------------------
-- when completed uncomment Spec line 96
data Writer a b = Result a b deriving (Eq, Show)

instance Functor (Writer a) where

instance Monoid a => Applicative (Writer a) where

instance Monoid a => Monad (Writer a) where

------------------------------------------------------------------
-- when completed uncomment Spec line 108 and update line 110 to be `it` instead of `xit`
newtype State s a = State{runState :: s -> (a, s)}

instance Functor (State s) where

instance Applicative (State s) where

instance Monad (State s) where