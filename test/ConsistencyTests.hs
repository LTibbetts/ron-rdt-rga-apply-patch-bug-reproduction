{-# LANGUAGE PatternSynonyms #-}

-- | Consistency checks: roundtrips, Semigroup agreement, commutativity.
module ConsistencyTests (consistencyTests) where

import Helpers
import RON.Data (Reducible (..))
import RON.Data.RGA (RgaRep)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

consistencyTests :: TestTree
consistencyTests =
    testGroup
        "consistency"
        [ testCase "applyPatches agrees with Semigroup merge order" $ do
            -- Both paths should produce the same vertex ID ordering.
            -- applyPatches uses the internal HashMap-based applyPatch;
            -- Semigroup merge uses flat-list merge (no HashMap bias).
            let state = mkState [5, 3, 1]
                -- applyPatches path
                applyResult = applyRaw state (mkInsertAfter 5 [4, 2])
                -- Semigroup merge path (using clean state ops)
                mergeResult = state <> mkState [4, 2]
            assertEqual
                "same traversal order"
                (getIds mergeResult)
                (getIds applyResult)
        , testCase "stateFromChunk . stateToChunk roundtrip" $ do
            let vids = [10, 8, 6, 4, 2]
                state = mkState vids
                roundtrip = stateFromChunk (stateToChunk state) :: RgaRep
            getIds roundtrip @?= vids
        , testCase "patch vertices are alive (refId = Zero)" $ do
            -- patchSetFromRawOp resets refId to Zero for inserted vertices.
            let state = mkState [5, 3, 1]
                result = applyRaw state (mkInsertAfter 5 [4, 2])
                refs = getIdsWithRef result
            assertBool
                "all vertices alive (refId = 0)"
                (all (\(_, r) -> r == 0) refs)
        , testCase "Semigroup merge is commutative on vertex order" $ do
            let a = mkState [5, 3, 1]
                b = mkState [4, 2]
            assertEqual "a <> b == b <> a" (getIds (a <> b)) (getIds (b <> a))
        ]
