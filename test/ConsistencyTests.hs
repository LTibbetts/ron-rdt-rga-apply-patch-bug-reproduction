-- | Consistency checks: roundtrips, Semigroup agreement, commutativity.
module ConsistencyTests (consistencyTests) where

import Helpers
import RON.Data (Reducible (..))
import RON.Data.RGA (RgaRep)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

-- | Two replicas for consistency tests.
deviceA, deviceB :: ReplicaId
deviceA = mkReplicaId 0xAAAA
deviceB = mkReplicaId 0xBBBB

consistencyTests :: TestTree
consistencyTests =
    testGroup
        "consistency"
        [ testCase "applyPatches agrees with Semigroup merge order" $ do
            -- Both paths should produce the same vertex ordering.
            -- applyPatches uses the internal HashMap-based applyPatch;
            -- Semigroup merge uses flat-list merge (no HashMap bias).
            let state = mkState (eventsAt deviceA [500, 300, 100])
                applyResult = applyRaw state (mkInsertAfter (eventAt deviceA 500) (eventsAt deviceB [400, 200]))
                mergeResult = state <> mkState (eventsAt deviceB [400, 200])
                applyTimes = getEventTimes applyResult
                mergeTimes = getEventTimes mergeResult
            assertEqual
                ( unlines
                    [ ""
                    , "  Input:    state times = [500, 300, 100] (device A)"
                    , "  Path A:   applyPatches with B's [t=400, t=200] after A's t=500"
                    , "  Path B:   Semigroup merge (state <> mkState [t=400, t=200])"
                    , "  Expected: both paths produce the same event time order"
                    , "  Path A result: " ++ show applyTimes
                    , "  Path B result: " ++ show mergeTimes
                    ]
                )
                mergeTimes
                applyTimes
        , testCase "stateFromChunk . stateToChunk roundtrip" $ do
            let times = [1000, 800, 600, 400, 200]
                state = mkState (eventsAt deviceA times)
                roundtrip = stateFromChunk (stateToChunk state) :: RgaRep
                roundtripTimes = getEventTimes roundtrip
            assertEqual
                ( unlines
                    [ ""
                    , "  Input:    times = " ++ show times
                    , "  Applied:  stateFromChunk (stateToChunk state)"
                    , "  Expected: " ++ show times
                    , "  Received: " ++ show roundtripTimes
                    ]
                )
                times
                roundtripTimes
        , testCase "patch vertices are alive (refId = Zero)" $ do
            -- patchSetFromRawOp resets refId to Zero for inserted vertices.
            let state = mkState (eventsAt deviceA [500, 300, 100])
                result = applyRaw state (mkInsertAfter (eventAt deviceA 500) (eventsAt deviceB [400, 200]))
                refs = getTimesWithRef result
                allAlive = all (\(_, r) -> r == 0) refs
            assertBool
                ( unlines
                    [ ""
                    , "  Input:    times = [500, 300, 100] (device A)"
                    , "  Applied:  B inserts [t=400, t=200] after A's t=500"
                    , "  Expected: all vertices have refTime = 0 (alive)"
                    , "  Received: " ++ show refs
                    ]
                )
                allAlive
        , testCase "Semigroup merge is commutative on vertex order" $ do
            let a = mkState (eventsAt deviceA [500, 300, 100])
                b = mkState (eventsAt deviceB [400, 200])
                abTimes = getEventTimes (a <> b)
                baTimes = getEventTimes (b <> a)
            assertEqual
                ( unlines
                    [ ""
                    , "  Input A:  times = [500, 300, 100] (device A)"
                    , "  Input B:  times = [400, 200] (device B)"
                    , "  Expected: A <> B == B <> A (commutative event time order)"
                    , "  A <> B:   " ++ show abTimes
                    , "  B <> A:   " ++ show baTimes
                    ]
                )
                abTimes
                baTimes
        ]
