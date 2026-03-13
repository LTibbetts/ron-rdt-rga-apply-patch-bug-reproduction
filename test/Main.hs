{-
  RON RGA applyPatch Bug Demonstration
  =====================================

  This test uses the ACTUAL ron and ron-rdt libraries from Hackage to reproduce
  a bug in RON.Data.RGA's internal applyPatch function.

  Bug: applyPatch uses left-biased HashMap.union (<>) when combining the merge
  result (newItems) with the existing state (targetItems). When a patch inserts
  vertices that interleave with existing vertices, the merge correctly updates
  itemNext pointers — but the left-biased union then DISCARDS those updated
  pointers, keeping the stale ones from targetItems. This causes interleaved
  patch vertices to become orphaned (present in the HashMap but unreachable
  via the linked list).

  Source: https://github.com/ff-notes/ron/blob/master/ron-rdt/lib/RON/Data/RGA.hs
  Buggy line (approx L298):
    pure $ HashMap.insert parent item' targetItems <> newItems
  Fix:
    pure $ HashMap.insert parent item' $ HashMap.union newItems targetItems
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Data.Word (Word64)
import RON.Data (Reducible (..))
import RON.Data.RGA (RgaRep)
import RON.Types (Atom (..), Op (..), UUID (..))
import RON.UUID (pattern Zero)
import System.Exit (exitFailure, exitSuccess)

{- | Build a simple UUID from an integer for testing.
Uses the raw Word64 pair directly; these compare naturally by value.
-}
mkUuid :: Word64 -> UUID
mkUuid n = UUID n 0

-- ============================================================================
-- TEST
-- ============================================================================

main :: IO ()
main = do
    putStrLn "=================================================================="
    putStrLn "  RON RGA applyPatch Bug Demonstration"
    putStrLn "  Using actual ron-rdt library from Hackage"
    putStrLn "  https://github.com/ff-notes/ron  (ron-rdt/lib/RON/Data/RGA.hs)"
    putStrLn "=================================================================="
    putStrLn ""

    ---------------------------------------------------------------------------
    -- Set up initial state: A(5) → B(3) → C(1)
    --
    -- stateFromChunk builds a VertexList via foldr, preserving input order.
    -- Vertices are identified by opId (UUID). refId=Zero means alive.
    -- Payload is non-empty (required to distinguish from tombstone ops).
    ---------------------------------------------------------------------------

    let stateOps =
            [ Op{opId = mkUuid 5, refId = Zero, payload = [AInteger 65]} -- A
            , Op{opId = mkUuid 3, refId = Zero, payload = [AInteger 66]} -- B
            , Op{opId = mkUuid 1, refId = Zero, payload = [AInteger 67]} -- C
            ]

    let initialState :: RgaRep
        initialState = stateFromChunk stateOps

    putStrLn "Setup:"
    putStrLn "  Initial state: A(5) -> B(3) -> C(1)  [3 vertices]"
    putStrLn ""

    ---------------------------------------------------------------------------
    -- Patch: insert D(4) and E(2) after parent A(5)
    --
    -- These are raw ops processed by patchSetFromRawOp internally:
    --   - refId = parent vertex (mkUuid 5 = A)
    --   - Non-empty payload = insert (not remove)
    --
    -- patchSetFromRawOp creates single-vertex VertexLists keyed by refId.
    -- Since both ops share the same parent (A), foldMap merges them via
    -- VertexList's Semigroup (= merge), producing D(4)→E(2) as the patch.
    --
    -- The merge of the existing suffix [B(3), C(1)] with patch [D(4), E(2)]
    -- should interleave by descending opId:
    --   D(4) → B(3) → E(2) → C(1)
    --
    -- So the expected final state is:
    --   A(5) → D(4) → B(3) → E(2) → C(1)  [5 vertices]
    ---------------------------------------------------------------------------

    let patchOps =
            [ Op{opId = mkUuid 4, refId = mkUuid 5, payload = [AInteger 68]} -- D after A
            , Op{opId = mkUuid 2, refId = mkUuid 5, payload = [AInteger 69]} -- E after A
            ]

    putStrLn "  Patch: insert D(4), E(2) after parent A(5)"
    putStrLn ""
    putStrLn "  Expected: A(5) -> D(4) -> B(3) -> E(2) -> C(1)  [5 vertices]"
    putStrLn ""

    ---------------------------------------------------------------------------
    -- Test 1: applyPatches (Reducible typeclass method)
    --
    -- This exercises the internal applyPatch function where the bug lives.
    -- We pass the patch ops as raw ops (second component of Unapplied tuple).
    -- The first component (ReducedChunks) is empty.
    ---------------------------------------------------------------------------

    putStrLn "-- Test 1: applyPatches (Reducible method) -----------------------"
    putStrLn "   Exercises internal applyPatch -> HashMap.union bug path"
    putStrLn ""

    let (patchResult, _unapplied) = applyPatches initialState ([], patchOps)
    let patchResultOps = stateToChunk patchResult
    let patchResultIds = [x | Op{opId = UUID x _} <- patchResultOps]

    putStrLn $ "  Vertices reachable: " ++ show (length patchResultOps)
    putStrLn $ "  Vertex IDs (linked list order): " ++ show patchResultIds
    putStrLn ""

    ---------------------------------------------------------------------------
    -- Test 2: Semigroup merge (<>) for comparison
    --
    -- The Semigroup instance uses merge/merge' which operates on flat lists
    -- (no HashMap bias). This should produce the correct result.
    --
    -- We build a clean "state" version of the patch vertices (refId=Zero)
    -- since stateFromChunk doesn't distinguish patch vs state ops.
    ---------------------------------------------------------------------------

    putStrLn "-- Test 2: Semigroup (<>) merge (for comparison) -----------------"
    putStrLn "   Uses flat-list merge -- no HashMap bias issue"
    putStrLn ""

    let patchAsState =
            [ Op{opId = mkUuid 4, refId = Zero, payload = [AInteger 68]} -- D
            , Op{opId = mkUuid 2, refId = Zero, payload = [AInteger 69]} -- E
            ]
    let mergeResult = initialState <> stateFromChunk patchAsState
    let mergeResultOps = stateToChunk mergeResult
    let mergeResultIds = [x | Op{opId = UUID x _} <- mergeResultOps]

    putStrLn $ "  Vertices reachable: " ++ show (length mergeResultOps)
    putStrLn $ "  Vertex IDs (linked list order): " ++ show mergeResultIds
    putStrLn ""

    ---------------------------------------------------------------------------
    -- Verdict
    ---------------------------------------------------------------------------

    putStrLn "-- Verdict ------------------------------------------------------"

    let expectedIds = [5, 4, 3, 2, 1] :: [Word64]

    if patchResultIds /= expectedIds
        && mergeResultIds == expectedIds
        then do
            let missing = filter (`notElem` patchResultIds) expectedIds
            putStrLn ""
            putStrLn "  BUG CONFIRMED."
            putStrLn ""
            putStrLn $
                "  applyPatches: only "
                    ++ show (length patchResultOps)
                    ++ " of 5 vertices reachable"
            putStrLn $
                "    Result:   " ++ renderChain patchResultIds
            putStrLn $
                "    Missing:  vertex ID(s) " ++ show missing ++ " -- orphaned!"
            putStrLn ""
            putStrLn $
                "  Semigroup merge: all "
                    ++ show (length mergeResultOps)
                    ++ " vertices reachable"
            putStrLn $
                "    Result:   " ++ renderChain mergeResultIds
            putStrLn ""
            putStrLn "  Root cause: In RON.Data.RGA.applyPatch (line ~298):"
            putStrLn "    pure $ HashMap.insert parent item' targetItems <> newItems"
            putStrLn ""
            putStrLn "  HashMap's (<>) = union is LEFT-BIASED. targetItems has STALE"
            putStrLn "  itemNext pointers that override the CORRECT pointers from"
            putStrLn "  newItems (the merge result). Interleaved patch vertices"
            putStrLn "  become orphaned: present in the HashMap but unreachable"
            putStrLn "  via the linked list."
            putStrLn ""
            putStrLn "  Fix: swap operands so newItems (correct) takes precedence:"
            putStrLn "    pure $ HashMap.insert parent item' $"
            putStrLn "      HashMap.union newItems targetItems"
            putStrLn ""
            exitSuccess
        else
            if patchResultIds == expectedIds
                then do
                    putStrLn ""
                    putStrLn "  Bug NOT triggered (all 5 vertices present)."
                    putStrLn "  The bug may have been fixed in this version."
                    exitSuccess
                else do
                    putStrLn ""
                    putStrLn "  UNEXPECTED: test expectations not met"
                    putStrLn $ "    applyPatches IDs: " ++ show patchResultIds
                    putStrLn $ "    merge IDs:        " ++ show mergeResultIds
                    putStrLn $ "    expected:         " ++ show expectedIds
                    exitFailure

-- | Render a list of vertex IDs as a chain: "5 → 4 → 3 → 2 → 1"
renderChain :: [Word64] -> String
renderChain [] = "(empty)"
renderChain xs = go xs
  where
    go [] = ""
    go [x] = show x
    go (x : rest) = show x ++ " -> " ++ go rest
