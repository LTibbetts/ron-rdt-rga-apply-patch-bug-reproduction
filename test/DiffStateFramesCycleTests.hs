{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

{- | Tests for the diffStateFrames -> vertexListFromOps cycle bug.

This module reproduces the CYCLE variant of the RGA bug, distinct from the
data-loss variant already covered in ApplyPatchTests.

Three bugs chain together to crash the production system:

  Bug 3 (trigger): cardith's diffStateFrames sends the ENTIRE RGA state body
  when ANY new ops exist, instead of just the new ops.
  Source: cardith-core/src/RON/Extra.hs:289-302

  Bug 1 (mechanism): vertexListFromOps uses HashMap.insert, which OVERWRITES
  existing entries when duplicate opIds appear, corrupting the linked-list's
  itemNext pointers into a cycle.
  Source: ron-rdt/lib/RON/Data/RGA.hs:135-144

  Bug 2 (amplifier): applyPatch's left-biased HashMap.union silently drops
  ops, causing data loss that compounds across sync rounds.
  Source: ron-rdt/lib/RON/Data/RGA.hs:284-298

Production crash: "Cannot find vertex id <UUID> in array"
Observed cycle: aI9Q -> ayoB -> __dN -> _RlY -> _TDO -> aI9Q

The cycle path:
  1. diffStateFrames(sender, receiver) for RGA:
       filtered = filter (not . member oldSet) (stateBody new)
       if null filtered then [] else stateBody new  -- BUG: entire body
  2. Receiver gets [A,B,C,D,E] (full body) when it already has [A,B,C,D]
  3. Combined ops fed to vertexListFromOps -> HashMap.insert overwrites
     a vertex's next pointer, creating a cycle -> traversal crashes
-}
module DiffStateFramesCycleTests (diffStateFramesCycleTests) where

import Control.Exception (SomeException, evaluate, try)
import Data.Word (Word64)
import Helpers
import RON.Data (Reducible (..))
import RON.Data.RGA (RgaRep)
import RON.Types (Atom (..), Op (..), UUID)
import RON.UUID (pattern Zero)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

-- | Two replicas with distinct MAC-derived replica IDs.
deviceA, deviceB :: ReplicaId
deviceA = mkReplicaId 0xAAAA
deviceB = mkReplicaId 0xBBBB

-- | Build an alive vertex op (refId = Zero = not tombstoned).
mkAliveOp :: UUID -> [Atom] -> Op
mkAliveOp uuid atoms = Op{opId = uuid, refId = Zero, payload = atoms}

{- | Simulate diffStateFrames for a single RGA chunk.

Real code (cardith-core/src/RON/Extra.hs:289-302):
  diff new old = Just new {stateBody = stateBody'}
    where
      stateBody'
        | stateType new == RGA.rgaType =
            if null filtered
              then []
              else stateBody new   -- FIXME: sends ENTIRE body
        | otherwise = filtered
        where
          filtered = filter (\x -> not (HS.member x oldSet)) (stateBody new)
      oldSet = HS.fromList (stateBody old)

Returns (buggyResult, correctResult):
  buggyResult   = what the buggy diffStateFrames actually produces
  correctResult = what it SHOULD produce (only new ops)
-}
simulateDiffStateFrames :: [Op] -> [Op] -> ([Op], [Op])
simulateDiffStateFrames senderBody receiverBody =
    let filtered = filter (`notElem` receiverBody) senderBody
        buggyResult
            | null filtered = []
            | otherwise = senderBody -- Bug 3: sends entire body for RGA
        correctResult = filtered
     in (buggyResult, correctResult)

diffStateFramesCycleTests :: TestTree
diffStateFramesCycleTests =
    testGroup
        "diffStateFrames cycle bug (Bug 1 + Bug 3)"
        [ testCase "[BUG] vertexListFromOps creates cycle from duplicate opIds (minimal)" $ do
            -- Direct reproduction of Bug 1: the MECHANISM.
            --
            -- vertexListFromOps (called by stateFromChunk) builds a HashMap-backed
            -- linked list via foldr. When duplicate opIds appear, HashMap.insert
            -- OVERWRITES the existing entry, corrupting itemNext pointers.
            --
            -- Input: [A@100, B@200, A'@100]  (A' shares opId with A)
            --
            -- foldr processes RIGHT to LEFT: A'@100 -> B@200 -> A@100
            --
            --   Step 1 (A'@100): items = {100: next=Nothing}
            --   Step 2 (B@200):  items = {100: next=Nothing, 200: next=100}
            --   Step 3 (A@100):  HashMap.insert 100 (next=200) OVERWRITES 100!
            --                    items = {100: next=200, 200: next=100}
            --                    CYCLE: 100 -> 200 -> 100 -> 200 -> ...
            --
            -- vertexListToOps traversal:
            --   Visit 100, delete from map, follow next to 200
            --   Visit 200, delete from map, follow next to 100
            --   100 already deleted -> "Cannot find vertex id"
            --
            -- With fix (nubBy dedup in vertexListFromOps):
            --   Duplicate A' is dropped, result is [A@100, B@200] — no cycle.
            let uuidA = eventAt deviceA 100
                uuidB = eventAt deviceA 200
                opsWithDupe =
                    [ mkAliveOp uuidA [AInteger 1] -- A (first occurrence)
                    , mkAliveOp uuidB [AInteger 2] -- B
                    , mkAliveOp uuidA [AInteger 99] -- A' (duplicate opId, different payload)
                    ]
                -- stateFromChunk -> vertexListFromOps -> creates cycle (Bug 1)
                cycleState = stateFromChunk opsWithDupe :: RgaRep
            -- stateToChunk -> vertexListToOps -> follows cycle -> crash
            result <-
                try (evaluate (length (stateToChunk cycleState))) ::
                    IO (Either SomeException Int)
            case result of
                Left e ->
                    assertBool
                        ( unlines
                            [ ""
                            , "  Bug 1: vertexListFromOps cycle from duplicate opIds"
                            , "  Input:    [A@100, B@200, A'@100] (A' = dup opId)"
                            , "  Expected: no crash (duplicates deduplicated)"
                            , "  Got:      CRASH: " ++ show e
                            ]
                        )
                        False
                Right _ ->
                    pure ()
        , testCase "[BUG] diffStateFrames sends entire RGA body, not just new ops" $ do
            -- Demonstrates Bug 3: the TRIGGER.
            --
            -- cardith's diffStateFrames computes what to send from sender to receiver.
            -- For non-RGA types, it correctly sends only new (filtered) ops.
            -- For RGA, the FIXME shortcut sends the ENTIRE stateBody when any
            -- new ops exist. This means the receiver gets ops it already has.
            --
            -- Scenario:
            --   Receiver: [A@400, B@300, C@200, D@100]
            --   Sender:   [A@400, B@300, C@200, D@100, E@50]
            --   New ops:  [E@50]  (the only actual difference)
            --
            --   Correct: send [E@50]             (1 op)
            --   Buggy:   send [A,B,C,D,E]        (5 ops = entire body)
            let receiverBody =
                    [ mkAliveOp (eventAt deviceA 400) [AInteger 400]
                    , mkAliveOp (eventAt deviceA 300) [AInteger 300]
                    , mkAliveOp (eventAt deviceA 200) [AInteger 200]
                    , mkAliveOp (eventAt deviceA 100) [AInteger 100]
                    ]
                senderBody =
                    [ mkAliveOp (eventAt deviceA 400) [AInteger 400]
                    , mkAliveOp (eventAt deviceA 300) [AInteger 300]
                    , mkAliveOp (eventAt deviceA 200) [AInteger 200]
                    , mkAliveOp (eventAt deviceA 100) [AInteger 100]
                    , mkAliveOp (eventAt deviceB 50) [AInteger 50]
                    ]
                (buggyDiff, correctDiff) = simulateDiffStateFrames senderBody receiverBody
            -- Buggy: sends all 5 ops (4 redundant + 1 new)
            assertEqual
                ( unlines
                    [ ""
                    , "  Bug 3: diffStateFrames RGA special case"
                    , "  Sender:   5 ops [A@400, B@300, C@200, D@100, E@50]"
                    , "  Receiver: 4 ops [A@400, B@300, C@200, D@100]"
                    , "  Expected buggy diff: 5 ops (entire body)"
                    ]
                )
                5
                (length buggyDiff)
            -- Correct fix: send only the 1 new op
            assertEqual
                ( unlines
                    [ ""
                    , "  Fix: diffStateFrames should send only new ops for RGA too"
                    , "  Expected correct diff: 1 op (only E@50)"
                    ]
                )
                1
                (length correctDiff)
        , testCase "[BUG] full-body diff + existing state creates cycle (Bug 3 -> Bug 1)" $ do
            -- Demonstrates the CHAIN: Bug 3 triggers Bug 1.
            --
            -- When the buggy diffStateFrames sends [A,B,C,D,E] (full body)
            -- and the receiver already has [A,B,C,D], combining both op lists
            -- feeds duplicate opIds into vertexListFromOps, creating a cycle.
            --
            -- This models the cardith subscriber flow:
            --   handleSync -> doUpdate -> patchDocument -> ... -> vertexListFromOps
            --
            -- Combined ops: [A@400, B@300, C@200, D@100, A@400, B@300, C@200, D@100, E@50]
            --
            -- foldr processes RIGHT to LEFT:
            --   senderOps build clean chain: A -> B -> C -> D -> E
            --   receiverOps overwrite via HashMap.insert:
            --     D@100: insert d (next=head=A) OVERWRITES d (was next=E)
            --            -> D->A->B->C->D  CYCLE  (E orphaned)
            --     C@200, B@300, A@400: no structural change (same next pointers)
            --
            -- Final linked list: A -> B -> C -> D -> A -> (A already deleted) -> CRASH
            let receiverBody =
                    [ mkAliveOp (eventAt deviceA 400) [AInteger 400]
                    , mkAliveOp (eventAt deviceA 300) [AInteger 300]
                    , mkAliveOp (eventAt deviceA 200) [AInteger 200]
                    , mkAliveOp (eventAt deviceA 100) [AInteger 100]
                    ]
                senderBody =
                    [ mkAliveOp (eventAt deviceA 400) [AInteger 400]
                    , mkAliveOp (eventAt deviceA 300) [AInteger 300]
                    , mkAliveOp (eventAt deviceA 200) [AInteger 200]
                    , mkAliveOp (eventAt deviceA 100) [AInteger 100]
                    , mkAliveOp (eventAt deviceB 50) [AInteger 50]
                    ]
                (buggyDiff, _) = simulateDiffStateFrames senderBody receiverBody
                -- Receiver combines existing state body with incoming diff body.
                -- This is the code path that creates duplicate opIds.
                combinedOps = receiverBody ++ buggyDiff
                cycleState = stateFromChunk combinedOps :: RgaRep
            result <-
                try (evaluate (length (stateToChunk cycleState))) ::
                    IO (Either SomeException Int)
            case result of
                Left e ->
                    assertBool
                        ( unlines
                            [ ""
                            , "  Chain: Bug 3 (diffStateFrames) -> Bug 1 (vertexListFromOps)"
                            , "  Receiver:  [A@400, B@300, C@200, D@100]"
                            , "  Diff:      [A@400, B@300, C@200, D@100, E@50] (full body)"
                            , "  Combined:  9 ops, 4 duplicate opIds"
                            , "  Expected:  no crash (duplicates deduplicated)"
                            , "  Got:       CRASH: " ++ show e
                            ]
                        )
                        False
                Right _ ->
                    pure ()
        , testCase "[BUG] production-like 5-vertex cycle (models aI9Q -> ayoB -> ... -> aI9Q)" $ do
            -- Models the production crash observed in las-server:
            --   aI9Q -> ayoB -> __dN -> _RlY -> _TDO -> aI9Q  <- CYCLE
            --
            -- Simplified: 5 vertices from device A, full body arrives via
            -- diffStateFrames, the receiver combines its state body with the
            -- incoming full body, creating 5 duplicate opIds.
            --
            -- The first duplicate processed by foldr (the one corresponding to
            -- the vertex just before the new op in the sender's state) gets its
            -- next pointer overwritten to point back toward the head, closing
            -- the cycle. All vertices after the overwrite point are orphaned.
            let times = [500, 400, 300, 200, 100] :: [Word64]
                originalOps =
                    [ mkAliveOp (eventAt deviceA t) [AInteger (fromIntegral t)]
                    | t <- times
                    ]
                -- Sender has original + one new op (the trigger for Bug 3)
                newOp = mkAliveOp (eventAt deviceB 50) [AInteger 50]
                senderFullBody = originalOps ++ [newOp]
                -- diffStateFrames sends senderFullBody (Bug 3: entire body)
                -- Receiver combines existing body with incoming full body
                combinedOps = originalOps ++ senderFullBody
                cycleState = stateFromChunk combinedOps :: RgaRep
            result <-
                try (evaluate (length (stateToChunk cycleState))) ::
                    IO (Either SomeException Int)
            case result of
                Left e ->
                    assertBool
                        ( unlines
                            [ ""
                            , "  Production-like 5-vertex cycle"
                            , "  State:     [t=500, t=400, t=300, t=200, t=100]"
                            , "  Diff:      [t=500, t=400, t=300, t=200, t=100, t=50]"
                            , "  Combined:  11 ops, 5 duplicates"
                            , "  Expected:  no crash (duplicates deduplicated)"
                            , "  Got:       CRASH: " ++ show e
                            ]
                        )
                        False
                Right _ ->
                    pure ()
        , testCase "safe merge via Semigroup correctly handles shared ops (no cycle)" $ do
            -- CONTRAST: The Semigroup merge path (<>) is SAFE.
            --
            -- When two RgaRep states are merged via <>, the internal merge'
            -- function compares opIds. For equal opIds (EQ case), it emits
            -- ONE merged vertex, correctly deduplicating.
            --
            -- This is the path used by reduceStateFrame in RON.Data.
            -- The bug is that some cardith code paths bypass this safe merge,
            -- allowing duplicate opIds to reach vertexListFromOps directly.
            let receiverState = mkState (eventsAt deviceA [400, 300, 200, 100])
                -- Sender has the same 4 ops + 1 new op
                senderState =
                    mkState (eventsAt deviceA [400, 300, 200, 100] ++ eventsAt deviceB [50])
                -- Safe merge via Semigroup
                mergedState = receiverState <> senderState
                mergedTimes = getEventTimes mergedState
            assertEqual
                ( unlines
                    [ ""
                    , "  Safe path: Semigroup merge (<>) deduplicates via merge'"
                    , "  Receiver: [400, 300, 200, 100]"
                    , "  Sender:   [400, 300, 200, 100, 50]"
                    , "  Expected: [400, 300, 200, 100, 50] (5 unique, no cycle)"
                    ]
                )
                [400, 300, 200, 100, 50]
                mergedTimes
        , testCase "correct diff (new ops only) avoids cycle entirely" $ do
            -- If diffStateFrames is FIXED to send only new ops (not entire body),
            -- no duplicates reach vertexListFromOps and no cycle forms.
            let receiverBody =
                    [ mkAliveOp (eventAt deviceA 400) [AInteger 400]
                    , mkAliveOp (eventAt deviceA 300) [AInteger 300]
                    , mkAliveOp (eventAt deviceA 200) [AInteger 200]
                    , mkAliveOp (eventAt deviceA 100) [AInteger 100]
                    ]
                senderBody =
                    [ mkAliveOp (eventAt deviceA 400) [AInteger 400]
                    , mkAliveOp (eventAt deviceA 300) [AInteger 300]
                    , mkAliveOp (eventAt deviceA 200) [AInteger 200]
                    , mkAliveOp (eventAt deviceA 100) [AInteger 100]
                    , mkAliveOp (eventAt deviceB 50) [AInteger 50]
                    ]
                (_, correctDiff) = simulateDiffStateFrames senderBody receiverBody
                -- With the fix, receiver combines its state with ONLY the new ops
                combinedOps = receiverBody ++ correctDiff
                fixedState = stateFromChunk combinedOps :: RgaRep
            -- No crash: the only new op has a unique opId
            result <-
                try (evaluate (length (stateToChunk fixedState))) ::
                    IO (Either SomeException Int)
            case result of
                Left e ->
                    assertBool
                        ( unlines
                            [ ""
                            , "  With fix: correct diff sends only new ops"
                            , "  Combined: [A@400, B@300, C@200, D@100, E@50]"
                            , "  Expected: no crash (no duplicate opIds)"
                            , "  Got:      crash: " ++ show e
                            ]
                        )
                        False
                Right _ -> do
                    -- Verify correct vertex count (5 unique ops)
                    let times = getEventTimes fixedState
                    assertEqual
                        ( unlines
                            [ ""
                            , "  With fix: 5 unique vertices, correct order"
                            ]
                        )
                        [400, 300, 200, 100, 50]
                        times
        , testCase "[BUG] cascading: applyPatch corruption + diffStateFrames -> cycle" $ do
            -- The full cascade that causes the production crash:
            --
            -- Round 1: B's patch interleaves with A's state (Bug 2: data loss)
            --   State:  [1000, 800, 600, 400] (device A)
            --   Patch:  B inserts [700, 500] after A's t=1000
            --   Bug 2:  applyPatch's left-biased union orphans B's vertices
            --   Roundtrip: stateToChunk drops orphans (permanent data loss)
            --
            -- Round 2: C diffs against the CORRUPTED state
            --   C has the original state [1000, 800, 600, 400]
            --   A has corrupted/truncated state from round 1
            --   diffStateFrames (C_state, A_state) sends C's full body
            --   A combines this with its own body -> duplicate opIds -> CYCLE
            let state = mkState (eventsAt deviceA [1000, 800, 600, 400])
                -- Round 1: B's interleaving patch (triggers Bug 2)
                round1 =
                    applyRaw
                        state
                        (mkInsertAfter (eventAt deviceA 1000) (eventsAt deviceB [700, 500]))
                -- Storage roundtrip: serialize then deserialize
                -- stateToChunk only emits REACHABLE vertices (drops orphans from Bug 2)
                round1Body = stateToChunk round1
                round1State = stateFromChunk round1Body :: RgaRep
                round1Times = getEventTimes round1State

            -- Round 2: C has the original state, diffs against A's corrupted state
            let originalBody =
                    [ mkAliveOp (eventAt deviceA t) [AInteger (fromIntegral t)]
                    | t <- [1000, 800, 600, 400] :: [Word64]
                    ]
                -- Simulate: C sends full body via diffStateFrames (Bug 3)
                -- A combines its corrupted body with C's full body
                combinedOps = round1Body ++ originalBody
                cycleOrCorrupt = stateFromChunk combinedOps :: RgaRep
            result <-
                try (evaluate (length (stateToChunk cycleOrCorrupt))) ::
                    IO (Either SomeException Int)
            case result of
                Left e ->
                    assertBool
                        ( unlines
                            [ ""
                            , "  Cascade: Bug 2 (applyPatch) + Bug 3 (diffStateFrames) -> Bug 1 (cycle)"
                            , "  Round 1: A=[1000,800,600,400], B inserts [700,500]"
                            , "  Round 1 result times: " ++ show round1Times
                            , "  Round 1 body length:  " ++ show (length round1Body)
                            , "  Round 2: C sends full original body [1000,800,600,400]"
                            , "  Combined ops: " ++ show (length combinedOps)
                            , "  Expected: no crash (all 3 bugs fixed)"
                            , "  Got:      CRASH: " ++ show e
                            ]
                        )
                        False
                Right _ ->
                    pure ()
        ]
