import Exponents
import Sorting
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
      [ testGroup "Exponents"
          [ testProperty "pow" pow_t
          , testProperty "pow'" pow_t']
      ]

pow_t, pow_t' :: Integer -> Positive Integer -> Bool
pow_t x (Positive n) = pow x n  == x ^ n
pow_t' x (Positive n) = pow x n  == x ^ n
