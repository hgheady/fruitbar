{-# LANGUAGE OverloadedStrings   #-}

import Lib

import Test.HUnit (assertEqual)
import Test.Framework (defaultMain, Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Time (UTCTime(..), Day, fromGregorianValid, utctDay)
import Control.Monad
-- import Utils

testGroups :: [Test]
testGroups = [ testFoo
             ]

--------------------------------------------------------------------------------
testFoo :: Test
testFoo = testGroup "Foo"
    [
      testCase "Test streak" $ do
        let s = streak 3 testPairs
        assertEqual (show s) streakHappy s
    , testCase "Test no streak" $ do
        let s = streak 3 testNoMore
        assertEqual (show s) [] s
    , testCase "Test streaks" $ do
        let s = streaks streaksPairs
        assertEqual (show s) testHappy s
    ]

main :: IO ()
main = defaultMain testGroups

testNoMore = [((utctDay $ testDay 1), 2)
             ,((utctDay $ testDay 1), 1)
             ]

streakHappy = take 3 testPairs

testPairs = [((utctDay $ testDay 1), 4)
            ,((utctDay $ testDay 1), 5)
            ,((utctDay $ testDay 1), 6)
            ,((utctDay $ testDay 1), 1)
            ]

testHappy = [[((utctDay $ testDay 3), 1)
             ,((utctDay $ testDay 4), 2)
             ,((utctDay $ testDay 5), 3)
             ,((utctDay $ testDay 6), 4)
            ]]

streaksPairs = perDay testStreak

testStreak = concat [ getBaseline        1  3
                    , getDayConsumptions 4  2
                    , getDayConsumptions 5  3
                    , getDayConsumptions 6  4
                    , getBaseline        7  3
                    ]

getDayConsumptions day consumptions = getResponses day day consumptions

getBaseline start length = getResponses start (start+1) length

getResponses i m n = take n $ map (ConsumptionRes "Anon" "any") (map testDay [i,m..])

testDay n = concat [testOct, testNov]!!(n-1)

testOct = testMonth 10 31
testNov = testMonth 11 30

testMonth m numDays = map (\d -> testTime m d) [1..numDays]

testTime m d = UTCTime (fromJust (fromGregorianValid 2020 m d)) 86399

