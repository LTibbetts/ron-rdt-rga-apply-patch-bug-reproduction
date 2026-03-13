{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

{- | Swarm resync: multi-entity bidirectional sync.

Models the withSwarmDB / resync pattern from RON.Swarm.Internal.DB.
A SimpleSwarmDB holds two entity types (profiles + tags), each backed by
an independent RGA. Two replicas start from the same base state, make
concurrent local changes, then resync by exchanging patches.

Key properties tested:
  1. After bidirectional resync, both replicas converge to the same state.
  2. All entities in the DB are independently affected by the bug.
  3. Patch application order doesn't matter for final convergence.
-}
module SwarmResyncTests (swarmResyncTests) where

import Helpers
import RON.Data (Reducible (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

swarmResyncTests :: TestTree
swarmResyncTests =
    testGroup
        "swarm resync (withSwarmDB / resync pattern)"
        [ testCase "[BUG] bidirectional resync: both replicas converge" $ do
            -- Models the real flow:
            --   1. withSwarmDB initializes both replicas with same base state
            --   2. Each replica makes concurrent local edits
            --   3. resync (triggered by MQTT reconnect) exchanges patches
            --   4. Both replicas should converge to identical state
            --
            -- Base state:
            --   profiles: Eth1(100) → WiFiC2(80) → WiFiH3(60)
            --   tags:     "outdoor"(100) → "primary"(80) → "backup"(60)
            let baseDB =
                    SimpleSwarmDB
                        { dbProfiles =
                            stateFromChunk
                                [ profileStateOp 100 (SimpleProfile "eth-1" Wired Confirmed)
                                , profileStateOp 80 (SimpleProfile "wifi-c-2" WirelessClient Confirmed)
                                , profileStateOp 60 (SimpleProfile "wifi-h-3" WirelessHost Confirmed)
                                ]
                        , dbTags =
                            stateFromChunk
                                [ tagStateOp 100 "outdoor"
                                , tagStateOp 80 "primary"
                                , tagStateOp 60 "backup"
                                ]
                        }

            -- Replica A: add a WiFi client profile + "mobile" tag
            let aProfs = [profileOp 70 100 (SimpleProfile "wifi-c-4" WirelessClient Unconfirmed)]
                aTags = [tagInsertOp 70 100 "mobile"]

            -- Replica B: add an Ethernet profile + "temporary" tag
            let bProfs = [profileOp 50 80 (SimpleProfile "eth-5" Wired Unconfirmed)]
                bTags = [tagInsertOp 50 80 "temporary"]

            let (replicaA, replicaB) = bidirectionalResync baseDB (aProfs, aTags) (bProfs, bTags)

            -- After resync, both replicas should have identical states
            assertEqual
                "profiles converge"
                (liveProfiles (dbProfiles replicaA))
                (liveProfiles (dbProfiles replicaB))
            assertEqual
                "tags converge"
                (liveTags (dbTags replicaA))
                (liveTags (dbTags replicaB))

            -- And all 5 profiles + 5 tags should be present
            assertEqual "5 profiles after resync" 5 (length (liveProfiles (dbProfiles replicaA)))
            assertEqual "5 tags after resync" 5 (length (liveTags (dbTags replicaA)))
        , testCase "[BUG] resync preserves all profiles across entities" $ do
            -- Focuses on the profiles entity: after resync, no profiles
            -- should be silently dropped due to the HashMap bias bug.
            let baseDB =
                    SimpleSwarmDB
                        { dbProfiles =
                            stateFromChunk
                                [ profileStateOp 100 (SimpleProfile "eth-1" Wired Confirmed)
                                , profileStateOp 80 (SimpleProfile "wifi-c-2" WirelessClient Confirmed)
                                , profileStateOp 60 (SimpleProfile "wifi-h-3" WirelessHost Confirmed)
                                ]
                        , dbTags = stateFromChunk []
                        }

            let aProfs = [profileOp 70 100 (SimpleProfile "wifi-c-4" WirelessClient Unconfirmed)]
                bProfs = [profileOp 50 80 (SimpleProfile "eth-5" Wired Unconfirmed)]
            let (replicaA, _) = bidirectionalResync baseDB (aProfs, []) (bProfs, [])

            -- Expected order: eth-1(100) → wifi-c-2(80) → wifi-c-4(70) → wifi-h-3(60) → eth-5(50)
            let expectedIds = ["eth-1", "wifi-c-2", "wifi-c-4", "wifi-h-3", "eth-5"]
            assertEqual
                "all profiles in correct order after resync"
                expectedIds
                (map profileId (liveProfiles (dbProfiles replicaA)))
        , testCase "[BUG] resync preserves all tags across entities" $ do
            -- Same test for the tags entity independently.
            let baseDB =
                    SimpleSwarmDB
                        { dbProfiles = stateFromChunk []
                        , dbTags =
                            stateFromChunk
                                [ tagStateOp 100 "outdoor"
                                , tagStateOp 80 "primary"
                                , tagStateOp 60 "backup"
                                ]
                        }

            let aTags = [tagInsertOp 70 100 "mobile"]
                bTags = [tagInsertOp 50 80 "temporary"]
            let (replicaA, _) = bidirectionalResync baseDB ([], aTags) ([], bTags)

            -- Expected: outdoor(100) → primary(80) → mobile(70) → backup(60) → temporary(50)
            assertEqual
                "all tags in correct order after resync"
                ["outdoor", "primary", "mobile", "backup", "temporary"]
                (liveTags (dbTags replicaA))
        , testCase "resync with non-interleaving patches always works" $ do
            -- When patch vertices are newer than all existing vertices,
            -- no interleaving occurs and the bug is not triggered.
            -- This test passes with or without the fix.
            let baseDB =
                    SimpleSwarmDB
                        { dbProfiles =
                            stateFromChunk
                                [ profileStateOp 100 (SimpleProfile "eth-1" Wired Confirmed)
                                ]
                        , dbTags =
                            stateFromChunk
                                [ tagStateOp 100 "outdoor"
                                ]
                        }

            -- Both replicas insert newer-than-existing vertices at head
            let aProfs = [profileOp 200 100 (SimpleProfile "wifi-c-2" WirelessClient Confirmed)]
                bProfs = [profileOp 300 100 (SimpleProfile "wifi-h-3" WirelessHost Confirmed)]
            let (replicaA, replicaB) = bidirectionalResync baseDB (aProfs, []) (bProfs, [])

            assertEqual
                "profiles converge (no interleaving)"
                (liveProfiles (dbProfiles replicaA))
                (liveProfiles (dbProfiles replicaB))
            assertEqual "3 profiles present" 3 (length (liveProfiles (dbProfiles replicaA)))
        , testCase "resync with removals converges" $ do
            -- Replica A adds a profile, Replica B removes a different one.
            -- After bidirectional resync, both should see the same live set.
            let baseDB =
                    SimpleSwarmDB
                        { dbProfiles =
                            stateFromChunk
                                [ profileStateOp 100 (SimpleProfile "eth-1" Wired Confirmed)
                                , profileStateOp 80 (SimpleProfile "wifi-c-2" WirelessClient Confirmed)
                                , profileStateOp 60 (SimpleProfile "wifi-h-3" WirelessHost Confirmed)
                                ]
                        , dbTags = stateFromChunk []
                        }

            -- Replica A: add wifi-c-4 after eth-1
            let aProfs = [profileOp 200 100 (SimpleProfile "wifi-c-4" WirelessClient Unconfirmed)]
            -- Replica B: remove wifi-h-3
            let bProfs = [mkRemoval 300 60]
            let (replicaA, replicaB) = bidirectionalResync baseDB (aProfs, []) (bProfs, [])

            assertEqual
                "profiles converge after add+remove"
                (liveProfiles (dbProfiles replicaA))
                (liveProfiles (dbProfiles replicaB))
            -- wifi-h-3 should be gone, wifi-c-4 should be present
            let profileIds = map profileId (liveProfiles (dbProfiles replicaA))
            assertBool "wifi-c-4 present" ("wifi-c-4" `elem` profileIds)
            assertBool "wifi-h-3 removed" ("wifi-h-3" `notElem` profileIds)
        , testCase "[BUG] resync order independence (A then B == B then A)" $ do
            -- The real resync uses traverseDBConcurrently_ — patches can
            -- arrive in any order. Final state must be the same regardless.
            let base =
                    stateFromChunk
                        [ profileStateOp 100 (SimpleProfile "eth-1" Wired Confirmed)
                        , profileStateOp 80 (SimpleProfile "wifi-c-2" WirelessClient Confirmed)
                        , profileStateOp 60 (SimpleProfile "wifi-h-3" WirelessHost Confirmed)
                        ]
                aOps = [profileOp 70 100 (SimpleProfile "wifi-c-4" WirelessClient Unconfirmed)]
                bOps = [profileOp 50 80 (SimpleProfile "eth-5" Wired Unconfirmed)]

            -- Apply A then B
            let resultAB = applyRaw (applyRaw base aOps) bOps
            -- Apply B then A
            let resultBA = applyRaw (applyRaw base bOps) aOps

            assertEqual
                "same profiles regardless of patch order"
                (liveProfiles resultAB)
                (liveProfiles resultBA)
            assertEqual "all 5 profiles present (AB)" 5 (length (liveProfiles resultAB))
        ]
