module Exercise1 where

-- given any number, return "Yes" if value is larger then 5
-- and return "No" if smaller (try using if/else)
moreThan5 :: Integer -> String
moreThan5 nr = if nr > 5 
    then "Yes" 
    else "No"

-- given a color print it's name, (try using case expressions)
data Colour = Red | Green | Blue

colorName :: Colour -> String
colorName color = case color of
    Red -> "Red"
    Green -> "Green"
    Blue -> "Blue"
 
-- reverse a list, by not using default `reverse` method
reverse :: [Integer] -> [Integer]
reverse = foldl (flip (:)) []

-- write map/filter/foldl without using them

-- map a list without using fmap
mMap :: (a -> b) -> [a] -> [b]
mMap fn [] = []
mMap fn (x : xs) = fn x : mMap fn xs

-- filter ali st of values without using filter
mFilter :: (a -> Bool) -> [a] -> [a]
mFilter fn [] = []
mFilter fn (x:xs) = if fn x then x : mFilter fn xs else mFilter fn xs

-- fold a list of values without using foldl/foldr
mFoldl :: (b -> a -> b) -> b -> [a]  -> b
mFoldl fn v [] = v
mFoldl fn v (x:xs) = mFoldl fn (fn v x) xs
