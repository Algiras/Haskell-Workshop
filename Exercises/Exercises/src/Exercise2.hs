module Exercise2 where

import Control.Monad
import Data.Maybe
import Data.Map.Strict as Map

-- Write a type Option a such that you can mimic how Maybe works
data Option a = Some a | None deriving (Show, Eq)

-- write a Functor instance
instance Functor Option where

-- wirte a Applicatve instance | when done navigate to Spec.hs and uncomment line 63
instance Applicative Option where

-- write a Monad instance
instance Monad Option where

-- replace  Maybe with your type to check if your code works as expected using the REPL
data Person = Person { 
      recId :: Integer
    , name :: String} deriving (Show)

database :: Map Integer Person
database = fromList $ fmap (\v -> (recId v, v)) [
    Person 0 "Algimantas",
    Person 1 "Vaidas",
    Person 2 "Lukas",
    Person 3 "Edvardas"]

    
-- division
lookupDB :: Monad m => (Integer -> m Person) -> Integer -> m Person
lookupDB handleError identifier = if member identifier database
    then pure $ database ! identifier
    else handleError identifier

-- divide list of doubles
listLookupDB :: Monad m => (Integer -> m Person) -> [Integer] -> m [Person]
listLookupDB handleError xs = mapM (lookupDB handleError) xs
