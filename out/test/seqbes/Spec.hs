import Test.Hspec
import Test.QuickCheck
import Test.HUnit

main :: IO ()
main = hspec $ do
  describe "Testing QuickCheck" $
    do it "prop_test" $ property prop_test
       it "prop_test" $ property prop_test 
  describe "Testing Something else" $
    do it "prop_test" $ property prop_test
       it "prop_test" $ property prop_test

prop_test :: Int -> Bool
prop_test x = True