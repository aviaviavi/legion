import           Lib
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup "Unit tests" [testCase "valid chain" testValidChain]

testValidChain :: IO ()
testValidChain  = do
  print "running test!"
  b <- initialBlock
  assertBool "block eq" $ b == b
  assertBool "empty chains are valid" $ isValidChain b []
  assertBool "base chain is valid" $ isValidChain b [b]
  assertBool "two init blocks are invalid" $ not $ isValidChain  b [b, b]
  goodBlock <- mineBlockFrom b "asdfasdf"
  assertBool "actually good chain" $ isValidChain b [b, goodBlock]
