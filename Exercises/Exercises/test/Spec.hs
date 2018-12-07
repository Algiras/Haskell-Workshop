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
import Exercise3 (Reader(..), Writer(..), State(..))
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

instance (Typeable a, Typeable b) => Show (Reader a b) where
    show fn = show $ typeOf fn

instance (Typeable a, Typeable b) => Show (State a b) where
    show fn = show $ typeOf fn

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Reader a b) where
    arbitrary = Reader <$> promote (`coarbitrary` arbitrary)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (State a b) where
    arbitrary = (\a -> State (\b -> let res = a b in (res, b))) <$> promote (`coarbitrary` arbitrary)

instance (Monoid a, Arbitrary b) => Arbitrary (Writer a b) where
        arbitrary = Result mempty <$> arbitrary

instance Arbitrary a => Arbitrary (Option a) where
    arbitrary = Some <$> arbitrary

instance Eq a => EqProp (Option a) where (=-=) = eq

instance (Show a, Arbitrary a, EqProp b) => EqProp (Reader a b) where 
    (Reader l) =-= (Reader r) = property (liftA2 (=-=) l r)

instance (Show a, Arbitrary a, EqProp a, EqProp b) => EqProp (State a b) where 
    (State l) =-= (State r) =  property (liftA2 (=-=) l r)

instance (Eq a, Eq b) => EqProp (Writer a b) where (=-=) = eq

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
            xdescribe "Exercise 2 > Option" $ do
                testBatch $ functor (undefined :: Option (Int, String, Integer))
                testBatch $ applicative (undefined :: Option (Int, String, Integer))
                testBatch $ monad (undefined :: Option (Int, String, Integer))
            describe "Exercise 3" $ do 
                        describe "Reader" $ do
                            testBatch $ functor (undefined :: Reader Int (String, Int, Double))
                            testBatch $ applicative (undefined :: Reader Int (String, Int, Double))
                            testBatch $ monad (undefined :: Reader Int (String, Int, Double))
                        describe "Writer" $ do
                            testBatch $ functor (undefined :: Writer String (String, Int, Double))
                            testBatch $ applicative (undefined :: Writer String (String, Int, Double))
                            testBatch $ monad (undefined :: Writer String (String, Int, Double))
                            it "collector should be Monadic" $ property $ \x y a b -> let 
                                testCase :: Integer -> Integer -> String -> String -> Writer String Integer
                                testCase x y a b = do 
                                    xV <- Result a x
                                    yV <- Result b y
                                    pure $ xV + yV
                                testCaseResult = Result (a <> b) (x + y)
                                in testCase x y a b == testCaseResult
                        describe "State" $ do
                            testBatch $ functor (undefined :: State Int (String, Int, Double))
                            testBatch $ applicative (undefined :: State Int (String, Int, Double))
                            testBatch $ monad (undefined :: State Int (String, Int, Double))
                            it "should matter in what order you apply state" $ property $ \t -> let
                                testState :: State Int Int
                                testState = ((State $ \v -> ((-) 1, v * 2)) <*> (State $ \b -> (b, b + 2)))
                                in runState testState (t :: Int) == (- (t * 2) + 1, t * 2 + 2)
                        