{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith) 
import Test.QuickCheck
import Test.Hspec.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Control.Monad
import Exercise1 (moreThan5, colorName, Colour(..), mMap, mFilter, mFoldl)
import qualified Exercise1 (reverse)
import Exercise2 (Option(..))
import Data.Typeable
import Data.List
import Test.QuickCheck.Gen.Unsafe
import System.Random
import Control.Applicative

main :: IO ()
main = hspecWith defaultConfig {configFastFail = False} specs

data Description a b = Description String (a -> b -> String) [(a, b)] | SelfDescription String [(a, b, String)]

buildTest :: forall a b. (Show b, Show a, Eq b) => (a -> b) -> Description a b -> SpecWith ()
buildTest fn (Description name describeFn cases) = describe name $ let testCase _ (val, answer) = it (describeFn val answer) $ fn val `shouldBe` answer 
    in foldM testCase () cases
buildTest fn (SelfDescription name cases) = describe name $ let testCase _ (val, answer, desc) = it desc $ fn val `shouldBe` answer 
    in foldM testCase () cases

instance Show Colour where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"
instance Arbitrary a => Arbitrary (Option a) where
    arbitrary = Some <$> arbitrary

instance Eq a => EqProp (Option a) where (=-=) = eq

specs :: Spec
specs = describe "Tests" $ do
            describe "Exercise1" $ do
                buildTest 
                    moreThan5 
                    (Description "moreThan5" (\v r -> "returns " ++ show v ++ " for " ++ r) 
                        [(5, "No")
                        ,(0, "No") 
                        ,(10, "Yes")])
                buildTest 
                    colorName 
                    (Description "colorName" (\v r -> "returns " ++ show v ++ " for " ++ r) 
                        [(Red, "Red")
                        ,(Green, "Green")
                        ,(Blue, "Blue")])
                describe "mMap" $ it "should map list" $ property $ \xs fn -> mMap fn xs == map (fn :: Integer -> String) (xs :: [Integer])
                describe "mFilter" $ it "should filter list" $ property $ \xs fn -> mFilter fn xs == filter (fn :: Integer -> Bool) (xs :: [Integer])
                describe "mFoldl" $ it "should reduce list" $ property $ \xs y fn -> mFoldl fn y xs == foldl (fn :: String -> Integer -> String) (y :: String) (xs :: [Integer])
                describe "reverse" $ it "should reverse list" $ property $ \xs -> (Exercise1.reverse . Exercise1.reverse) xs == (xs :: [Integer])
            describe "Exercise 2 > Option" $ do
                testBatch $ functor (undefined :: Option (Int, String, Integer))
                testBatch $ monad (undefined :: Option (Int, String, Integer))
                -- testBatch $ applicative (undefined :: Option (Int, String, Integer))