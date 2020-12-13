import Lib

import Test.HUnit (assertEqual)
import Test.Framework (defaultMain, Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Data.Monoid
import Control.Monad
-- import Utils

testGroups :: [Test]
testGroups = [ testFoo
             ]

--------------------------------------------------------------------------------
testFoo :: Test
testFoo = testGroup "Foo"
    [ testCase "Foo test case" $ do
        assertEqual "should not be possible" 1 1
    ]

main :: IO ()
main = defaultMain testGroups
