{-
  RON RGA Test Suite
  ==================

  Tests the RGA (Replicated Growable Array) implementation from the ron-rdt
  package, focusing on the applyPatch bug and general correctness.

  Bug: In RON.Data.RGA.applyPatch (line ~298), the expression
    HashMap.insert parent item' targetItems <> newItems
  uses left-biased HashMap.union. targetItems has STALE itemNext pointers
  that override the CORRECT pointers from newItems (the merge result).
  This causes interleaved patch vertices to become orphaned.

  Fix: swap operands so newItems takes precedence:
    HashMap.insert parent item' $ HashMap.union newItems targetItems

  Source: https://github.com/ff-notes/ron/blob/master/ron-rdt/lib/RON/Data/RGA.hs
-}
module Main (main) where

import ApplyPatchTests (applyPatchTests)
import ConsistencyTests (consistencyTests)
import DeviceConnectionTests (deviceConnectionTests)
import DiffStateFramesCycleTests (diffStateFramesCycleTests)
import EdgeCaseTests (edgeCaseTests)
import RemovalTests (removalTests)
import SwarmResyncTests (swarmResyncTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
        testGroup
            "RON RGA"
            [ applyPatchTests
            , removalTests
            , consistencyTests
            , edgeCaseTests
            , deviceConnectionTests
            , swarmResyncTests
            , diffStateFramesCycleTests
            ]
