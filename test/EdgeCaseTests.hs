{-# LANGUAGE PatternSynonyms #-}

-- | Edge-case tests for the RGA.
module EdgeCaseTests (edgeCaseTests) where

import Helpers
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, (@?=))

edgeCaseTests :: TestTree
edgeCaseTests =
    testGroup
        "edge cases"
        [ testCase "empty patch leaves state unchanged" $ do
            let state = mkState [5, 3, 1]
                result = applyRaw state []
            getIds result @?= [5, 3, 1]
        , testCase "single vertex state with append" $ do
            let state = mkState [5]
                result = applyRaw state (mkInsertAfter 5 [3, 1])
            getIds result @?= [5, 3, 1]
        , testCase "patch with nonexistent parent is not applied" $ do
            -- Parent 99 doesn't exist in state; patch goes into unapplied.
            let state = mkState [5, 3, 1]
                result = applyRaw state (mkInsertAfter 99 [4, 2])
            getIds result @?= [5, 3, 1]
        , testCase "mixed inserts and removals in same batch" $ do
            -- Insert D(4) after A(5), and tombstone B(3) in the same batch.
            let state = mkState [5, 3, 1]
                ops = mkInsertAfter 5 [4] ++ [mkRemoval 20 3]
                result = applyRaw state ops
            -- D(4) inserted; B(3) tombstoned but still in list
            getIds result @?= [5, 4, 3, 1]
            let refs = getIdsWithRef result
            assertEqual "B tombstoned" (3, 20) (refs !! 2)
        ]
