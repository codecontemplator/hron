import Test.HUnit
import HRON

test1 = TestCase (assertEqual "my assert" "123" "123")

tests = TestList [TestLabel "test1" test1]
