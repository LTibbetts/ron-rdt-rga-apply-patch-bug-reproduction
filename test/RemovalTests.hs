-- | Tests for RGA removal (tombstone) operations.
module RemovalTests (removalTests) where

import Helpers
import RON.Data (Reducible (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

-- | Single device replica for removal tests.
device :: ReplicaId
device = mkReplicaId 0xAAAA

removalTests :: TestTree
removalTests =
    testGroup
        "removal operations"
        [ testCase "tombstone a vertex" $ do
            -- State: t=500 → t=300 → t=100, all alive
            -- Remove t=300 with tombstone event t=2000
            let state = mkState (eventsAt device [500, 300, 100])
                result = applyRaw state [mkRemoval (eventAt device 2000) (eventAt device 300)]
            -- Linked list order unchanged, t=300 now tombstoned
            assertRgaOrder [500, 300, 100] "tombstone t=300 (event t=2000)" [500, 300, 100] result
            assertTombstones
                [500, 300, 100]
                "tombstone t=300 (event t=2000)"
                [(500, 0), (300, 2000), (100, 0)]
                result
        , testCase "tombstone preserves vertex count" $ do
            -- Tombstoned vertices remain in the linked list (not removed)
            let state = mkState (eventsAt device [500, 300, 100])
                result = applyRaw state [mkRemoval (eventAt device 2000) (eventAt device 300)]
                actual = length (stateToChunk result)
            assertEqual
                ( unlines
                    [ ""
                    , "  Input:    times = [500, 300, 100]"
                    , "  Applied:  tombstone t=300 (event t=2000)"
                    , "  Expected: 3 vertices (tombstoned vertices remain in list)"
                    , "  Received: " ++ show actual ++ " vertices"
                    ]
                )
                3
                actual
        , testCase "larger tombstone event wins" $ do
            -- Two tombstone events for the same vertex; max wins
            let state = mkState (eventsAt device [500, 300, 100])
                result =
                    applyRaw
                        state
                        [ mkRemoval (eventAt device 1000) (eventAt device 300)
                        , mkRemoval (eventAt device 2000) (eventAt device 300)
                        ]
            assertTombstones
                [500, 300, 100]
                "tombstone t=300 (events t=1000 and t=2000; max should win)"
                [(500, 0), (300, 2000), (100, 0)]
                result
        , testCase "tombstone on already-tombstoned vertex takes max" $ do
            -- Apply one tombstone, then another larger one
            let state = mkState (eventsAt device [500, 300, 100])
                step1 = applyRaw state [mkRemoval (eventAt device 1000) (eventAt device 300)]
                step2 = applyRaw step1 [mkRemoval (eventAt device 2000) (eventAt device 300)]
            assertTombstones
                [500, 300, 100]
                "tombstone t=300 (event t=1000), then tombstone again (event t=2000)"
                [(500, 0), (300, 2000), (100, 0)]
                step2
        ]
