-- | Edge-case tests for the RGA.
module EdgeCaseTests (edgeCaseTests) where

import Helpers
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

-- | Two replicas for edge-case tests.
deviceA, deviceB :: ReplicaId
deviceA = mkReplicaId 0xAAAA
deviceB = mkReplicaId 0xBBBB

edgeCaseTests :: TestTree
edgeCaseTests =
    testGroup
        "edge cases"
        [ testCase "empty patch leaves state unchanged" $ do
            let state = mkState (eventsAt deviceA [500, 300, 100])
                result = applyRaw state []
            assertRgaOrder [500, 300, 100] "empty patch (no ops)" [500, 300, 100] result
        , testCase "single vertex state with append" $ do
            let state = mkState (eventsAt deviceA [500])
                result = applyRaw state (mkInsertAfter (eventAt deviceA 500) (eventsAt deviceB [300, 100]))
            assertRgaOrder [500] "B inserts [t=300, t=100] after A's t=500" [500, 300, 100] result
        , testCase "patch with nonexistent parent is not applied" $ do
            -- Parent t=9900 doesn't exist in state; patch goes into unapplied.
            let state = mkState (eventsAt deviceA [500, 300, 100])
                result = applyRaw state (mkInsertAfter (eventAt deviceB 9900) (eventsAt deviceB [400, 200]))
            assertRgaOrder
                [500, 300, 100]
                "B inserts [t=400, t=200] after nonexistent t=9900 (should be ignored)"
                [500, 300, 100]
                result
        , testCase "mixed inserts and removals in same batch" $ do
            -- Insert t=400 after t=500, and tombstone t=300 in the same batch.
            let state = mkState (eventsAt deviceA [500, 300, 100])
                ops =
                    mkInsertAfter (eventAt deviceA 500) (eventsAt deviceB [400])
                        ++ [mkRemoval (eventAt deviceA 2000) (eventAt deviceA 300)]
                result = applyRaw state ops
            assertRgaOrder
                [500, 300, 100]
                "B inserts [t=400] after A's t=500; tombstone t=300 (event t=2000)"
                [500, 400, 300, 100]
                result
            assertTombstones
                [500, 300, 100]
                "B inserts [t=400] after A's t=500; tombstone t=300 (event t=2000)"
                [(500, 0), (400, 0), (300, 2000), (100, 0)]
                result
        ]
