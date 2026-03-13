{-# LANGUAGE PatternSynonyms #-}

{- | Tests for applyPatch correctness.
Tests marked [BUG] fail when the HashMap.union bias bug is present;
the rest should always pass.
-}
module ApplyPatchTests (applyPatchTests) where

import Helpers
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

applyPatchTests :: TestTree
applyPatchTests =
    testGroup
        "applyPatch correctness"
        [ testCase "[BUG] interleaving two-vertex patch (canonical case)" $ do
            -- State:    A(5) -> B(3) -> C(1)
            -- Patch:    D(4), E(2) after A
            -- Expected: A(5) -> D(4) -> B(3) -> E(2) -> C(1)
            --
            -- With bug: E(2) orphaned because B(3).next stays C(1) instead
            --           of updating to E(2).
            let state = mkState [5, 3, 1]
                result = applyRaw state (mkInsertAfter 5 [4, 2])
            getIds result @?= [5, 4, 3, 2, 1]
        , testCase "[BUG] single interleaving vertex" $ do
            -- State:    A(5) -> B(3) -> C(1)
            -- Patch:    D(2) after A
            -- Expected: A(5) -> B(3) -> D(2) -> C(1)
            --
            -- With bug: D(2) orphaned because B(3).next stays C(1).
            let state = mkState [5, 3, 1]
                result = applyRaw state (mkInsertAfter 5 [2])
            getIds result @?= [5, 3, 2, 1]
        , testCase "[BUG] all-older patch (every pointer needs updating)" $ do
            -- State:    A(10) -> B(8) -> C(6)
            -- Patch:    D(3), E(1) after A
            -- Expected: A(10) -> B(8) -> C(6) -> D(3) -> E(1)
            --
            -- With bug: C(6).next stays Nothing (tail), D(3) and E(1) orphaned.
            let state = mkState [10, 8, 6]
                result = applyRaw state (mkInsertAfter 10 [3, 1])
            getIds result @?= [10, 8, 6, 3, 1]
        , testCase "[BUG] deep interleaving: 5 vertices into 5-vertex chain" $ do
            -- State:    A(20) -> B(16) -> C(12) -> D(8) -> E(4)
            -- Patch:    F(18), G(14), H(10), I(6), J(2) after A
            -- Perfect interleaving: F,B,G,C,H,D,I,E,J
            --
            -- With bug: B(16).next stays C(12) instead of G(14);
            --           G,H,I,J all orphaned. Only 6 of 10 reachable.
            let state = mkState [20, 16, 12, 8, 4]
                result = applyRaw state (mkInsertAfter 20 [18, 14, 10, 6, 2])
            getIds result @?= [20, 18, 16, 14, 12, 10, 8, 6, 4, 2]
        , testCase "[BUG] multiple patches from different parents" $ do
            -- State:    A(10) -> B(8) -> C(6) -> D(4) -> E(2)
            -- Patch 1:  F(5) after A   -- interleaves between C(6) and D(4)
            -- Patch 2:  G(3) after C   -- interleaves between D(4) and E(2)
            -- Expected: A(10) -> B(8) -> C(6) -> F(5) -> D(4) -> G(3) -> E(2)
            --
            -- With bug: both F and G orphaned.
            let state = mkState [10, 8, 6, 4, 2]
                ops = mkInsertAfter 10 [5] ++ mkInsertAfter 6 [3]
                result = applyRaw state ops
            getIds result @?= [10, 8, 6, 5, 4, 3, 2]
        , testCase "all-newer patch (no interleaving)" $ do
            -- All patch vertices newer than existing suffix, so no interleaving.
            -- This works correctly even without the fix.
            let state = mkState [5, 3, 1]
                result = applyRaw state (mkInsertAfter 5 [8, 7])
            getIds result @?= [5, 8, 7, 3, 1]
        , testCase "append after tail vertex (no existing suffix)" $ do
            -- C has no next, so the patch is used directly (no merge needed).
            -- This works correctly even without the fix.
            let state = mkState [5, 3, 1]
                result = applyRaw state (mkInsertAfter 1 [8, 7])
            getIds result @?= [5, 3, 1, 8, 7]
        ]
