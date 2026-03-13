{-# LANGUAGE PatternSynonyms #-}

-- | Tests for RGA removal (tombstone) operations.
module RemovalTests (removalTests) where

import Helpers
import RON.Data (Reducible (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, (@?=))

removalTests :: TestTree
removalTests =
    testGroup
        "removal operations"
        [ testCase "tombstone a vertex" $ do
            -- State: A(5) -> B(3) -> C(1), all alive
            -- Remove B(3) with tombstone event 20
            let state = mkState [5, 3, 1]
                result = applyRaw state [mkRemoval 20 3]
            -- Linked list order unchanged
            getIds result @?= [5, 3, 1]
            -- B(3) should now be tombstoned (refId = 20)
            let refs = getIdsWithRef result
            assertEqual "A alive" (5, 0) (refs !! 0)
            assertEqual "B tombstoned" (3, 20) (refs !! 1)
            assertEqual "C alive" (1, 0) (refs !! 2)
        , testCase "tombstone preserves vertex count" $ do
            -- Tombstoned vertices remain in the linked list (not removed)
            let state = mkState [5, 3, 1]
                result = applyRaw state [mkRemoval 20 3]
            length (stateToChunk result) @?= 3
        , testCase "larger tombstone event wins" $ do
            -- Two tombstone events for the same vertex; max wins
            let state = mkState [5, 3, 1]
                result = applyRaw state [mkRemoval 10 3, mkRemoval 20 3]
                refs = getIdsWithRef result
            assertEqual "B tombstoned with max event" (3, 20) (refs !! 1)
        , testCase "tombstone on already-tombstoned vertex takes max" $ do
            -- Apply one tombstone, then another larger one
            let state = mkState [5, 3, 1]
                step1 = applyRaw state [mkRemoval 10 3]
                step2 = applyRaw step1 [mkRemoval 20 3]
                refs = getIdsWithRef step2
            assertEqual "max tombstone wins" (3, 20) (refs !! 1)
        ]
