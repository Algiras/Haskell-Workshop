module Exercise1 where

-- given any number, return "Yes" if value is larger then 5
-- and return "No" if smaller (try using if/else)
moreThan5 :: Integer -> String
moreThan5 nr = undefined

-- given a color print it's name, (try using case expressions)
data Colour = Red | Green | Blue

colorName :: Colour -> String
colorName color = undefined
 
-- reverse a list, by not using default `reverse` method
reverse :: [Integer] -> [Integer]
reverse = undefined

-- write map/filter/foldl without using them

-- map a list without using fmap
mMap :: (a -> b) -> [a] -> [b]
mMap fn xs = undefined

-- filter ali st of values without using filter
mFilter :: (a -> Bool) -> [a] -> [a]
mFilter fn x = undefined

-- fold a list of values without using foldl/foldr
mFoldl :: (b -> a -> b) -> b -> [a]  -> b
mFoldl fn v xs = undefined
