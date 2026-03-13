{- | Tests for applyPatch correctness.
Tests marked [BUG] fail when the HashMap.union bias bug is present;
the rest should always pass.

Each test models a realistic distributed scenario: deviceA created the base
state over time, deviceB was offline and made concurrent edits. When B's
patches arrive at A during resync, the interleaving triggers the bug.
-}
module ApplyPatchTests (applyPatchTests) where

import Helpers
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

-- | Two replicas, as in the real system (different devices with different MACs).
deviceA, deviceB :: ReplicaId
deviceA = mkReplicaId 0xAAAA
deviceB = mkReplicaId 0xBBBB

applyPatchTests :: TestTree
applyPatchTests =
    testGroup
        "applyPatch correctness"
        [ testCase "[BUG] interleaving two-vertex patch (canonical case)" $ do
            -- Device A's state (created over time):  t=500 → t=300 → t=100
            -- Device B's patch (concurrent edits):   t=400, t=200 after t=500
            -- Expected after resync:                 500 → 400 → 300 → 200 → 100
            --
            -- With bug: B's t=200 is orphaned because t=300's next pointer
            --           stays at t=100 instead of updating to t=200.
            let state = mkState (eventsAt deviceA [500, 300, 100])
                result = applyRaw state (mkInsertAfter (eventAt deviceA 500) (eventsAt deviceB [400, 200]))
            assertRgaOrder [500, 300, 100] "B inserts [t=400, t=200] after A's t=500" [500, 400, 300, 200, 100] result
        , testCase "[BUG] single interleaving vertex" $ do
            -- State (A):  t=500 → t=300 → t=100
            -- Patch (B):  t=200 after t=500 → falls between t=300 and t=100
            --
            -- With bug: B's t=200 orphaned because t=300's next stays at t=100.
            let state = mkState (eventsAt deviceA [500, 300, 100])
                result = applyRaw state (mkInsertAfter (eventAt deviceA 500) (eventsAt deviceB [200]))
            assertRgaOrder [500, 300, 100] "B inserts [t=200] after A's t=500" [500, 300, 200, 100] result
        , testCase "[BUG] all-older patch (every pointer needs updating)" $ do
            -- State (A):  t=1000 → t=800 → t=600
            -- Patch (B):  t=300, t=100 after t=1000 → both after entire chain
            --
            -- With bug: t=600's next stays Nothing (tail), B's t=300 and t=100 orphaned.
            let state = mkState (eventsAt deviceA [1000, 800, 600])
                result = applyRaw state (mkInsertAfter (eventAt deviceA 1000) (eventsAt deviceB [300, 100]))
            assertRgaOrder [1000, 800, 600] "B inserts [t=300, t=100] after A's t=1000" [1000, 800, 600, 300, 100] result
        , testCase "[BUG] deep interleaving: 5 vertices into 5-vertex chain" $ do
            -- State (A):  t=2000 → t=1600 → t=1200 → t=800 → t=400
            -- Patch (B):  t=1800, t=1400, t=1000, t=600, t=200 after t=2000
            -- Perfect interleaving: every B vertex slots between two A vertices.
            --
            -- With bug: t=1600's next stays at t=1200 instead of t=1400;
            --           t=1400, t=1000, t=600, t=200 all orphaned.
            let state = mkState (eventsAt deviceA [2000, 1600, 1200, 800, 400])
                result = applyRaw state (mkInsertAfter (eventAt deviceA 2000) (eventsAt deviceB [1800, 1400, 1000, 600, 200]))
            assertRgaOrder
                [2000, 1600, 1200, 800, 400]
                "B inserts [t=1800..t=200] after A's t=2000"
                [2000, 1800, 1600, 1400, 1200, 1000, 800, 600, 400, 200]
                result
        , testCase "[BUG] multiple patches from different parents" $ do
            -- State (A):  t=1000 → t=800 → t=600 → t=400 → t=200
            -- Patch 1:    B inserts t=500 after A's t=1000 → between t=600 and t=400
            -- Patch 2:    B inserts t=300 after A's t=600  → between t=400 and t=200
            --
            -- With bug: both B's t=500 and t=300 orphaned.
            let state = mkState (eventsAt deviceA [1000, 800, 600, 400, 200])
                ops =
                    mkInsertAfter (eventAt deviceA 1000) (eventsAt deviceB [500])
                        ++ mkInsertAfter (eventAt deviceA 600) (eventsAt deviceB [300])
                result = applyRaw state ops
            assertRgaOrder
                [1000, 800, 600, 400, 200]
                "B inserts [t=500] after A's t=1000; B inserts [t=300] after A's t=600"
                [1000, 800, 600, 500, 400, 300, 200]
                result
        , testCase "all-newer patch (no interleaving)" $ do
            -- Patch vertices newer than all existing suffix → no interleaving.
            -- Works correctly even without the fix.
            let state = mkState (eventsAt deviceA [500, 300, 100])
                result = applyRaw state (mkInsertAfter (eventAt deviceA 500) (eventsAt deviceB [800, 700]))
            assertRgaOrder [500, 300, 100] "B inserts [t=800, t=700] after A's t=500 (all newer)" [500, 800, 700, 300, 100] result
        , testCase "append after tail vertex (no existing suffix)" $ do
            -- Tail has no next, so the patch is appended directly (no merge needed).
            -- Works correctly even without the fix.
            let state = mkState (eventsAt deviceA [500, 300, 100])
                result = applyRaw state (mkInsertAfter (eventAt deviceA 100) (eventsAt deviceB [800, 700]))
            assertRgaOrder [500, 300, 100] "B inserts [t=800, t=700] after tail t=100" [500, 300, 100, 800, 700] result
        ]
