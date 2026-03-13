{-# LANGUAGE OverloadedStrings #-}

{- | Swarm resync: multi-entity bidirectional sync.

Models the withSwarmDB / resync pattern from RON.Swarm.Internal.DB.
A SimpleSwarmDB holds two entity types (profiles + tags), each backed by
an independent RGA. Two replicas start from the same base state, make
concurrent local changes, then resync by exchanging patches.

UUIDs use the real event encoding: each device has a ReplicaId (from MAC)
and operations happen at specific epoch times. The interleaving bug triggers
when patches from device B contain events whose times fall between device A's
existing events in the RGA linked list.

Key properties tested:
  1. After bidirectional resync, both replicas converge to the same state.
  2. All entities in the DB are independently affected by the bug.
  3. Patch application order doesn't matter for final convergence.
-}
module SwarmResyncTests (swarmResyncTests) where

import qualified Data.Text as Text
import Helpers
import RON.Data (Reducible (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

-- | Two devices with MAC-derived replica IDs.
deviceA, deviceB :: ReplicaId
deviceA = mkReplicaId 0xAABBCCDDEE01
deviceB = mkReplicaId 0x112233445566

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
            -- Base state (from device A at times 1000, 800, 600):
            --   profiles: Eth1@1000 → WiFiC2@800 → WiFiH3@600
            --   tags:     "outdoor"@1000 → "primary"@800 → "backup"@600
            let eth1 = eventAt deviceA 1000
                wifiC2 = eventAt deviceA 800
                wifiH3 = eventAt deviceA 600
                tOutdoor = eventAt deviceA 1000
                tPrimary = eventAt deviceA 800
                tBackup = eventAt deviceA 600
                baseDB =
                    SimpleSwarmDB
                        { dbProfiles =
                            stateFromChunk
                                [ profileStateOp eth1 (SimpleProfile "eth-1" Wired Confirmed)
                                , profileStateOp wifiC2 (SimpleProfile "wifi-c-2" WirelessClient Confirmed)
                                , profileStateOp wifiH3 (SimpleProfile "wifi-h-3" WirelessHost Confirmed)
                                ]
                        , dbTags =
                            stateFromChunk
                                [ tagStateOp tOutdoor "outdoor"
                                , tagStateOp tPrimary "primary"
                                , tagStateOp tBackup "backup"
                                ]
                        }

            -- Device A: add WiFi client profile@t=700 + "mobile" tag@t=700
            let wifiC4 = eventAt deviceA 700
                tMobile = eventAt deviceA 700
                aProfs = [profileOp wifiC4 eth1 (SimpleProfile "wifi-c-4" WirelessClient Unconfirmed)]
                aTags = [tagInsertOp tMobile tOutdoor "mobile"]

            -- Device B: add Ethernet profile@t=500 + "temporary" tag@t=500
            let eth5 = eventAt deviceB 500
                tTemp = eventAt deviceB 500
                bProfs = [profileOp eth5 wifiC2 (SimpleProfile "eth-5" Wired Unconfirmed)]
                bTags = [tagInsertOp tTemp tPrimary "temporary"]

            let (replicaA, replicaB) = bidirectionalResync baseDB (aProfs, aTags) (bProfs, bTags)

            -- After resync, both replicas should have identical states
            assertConverge
                "profiles"
                (map show . liveProfiles)
                (dbProfiles replicaA)
                (dbProfiles replicaB)
            assertConverge
                "tags"
                (map Text.unpack . liveTags)
                (dbTags replicaA)
                (dbTags replicaB)

            -- And all 5 profiles + 5 tags should be present
            assertProfileCount
                "bidirectional resync: base=[eth-1, wifi-c-2, wifi-h-3]; A adds wifi-c-4@t=700; B adds eth-5@t=500"
                5
                (dbProfiles replicaA)
            let tagCount = length (liveTags (dbTags replicaA))
            assertEqual
                ( unlines
                    [ ""
                    , "  Scenario:       bidirectional resync: base=[outdoor, primary, backup]; A adds mobile@t=700; B adds temporary@t=500"
                    , "  Expected count: 5"
                    , "  Received count: " ++ show tagCount
                    , "  Received tags:  " ++ show (map Text.unpack (liveTags (dbTags replicaA)))
                    ]
                )
                5
                tagCount
        , testCase "[BUG] resync preserves all profiles across entities" $ do
            -- Focuses on the profiles entity: after resync, no profiles
            -- should be silently dropped due to the HashMap bias bug.
            let eth1 = eventAt deviceA 1000
                wifiC2 = eventAt deviceA 800
                wifiH3 = eventAt deviceA 600
                baseDB =
                    SimpleSwarmDB
                        { dbProfiles =
                            stateFromChunk
                                [ profileStateOp eth1 (SimpleProfile "eth-1" Wired Confirmed)
                                , profileStateOp wifiC2 (SimpleProfile "wifi-c-2" WirelessClient Confirmed)
                                , profileStateOp wifiH3 (SimpleProfile "wifi-h-3" WirelessHost Confirmed)
                                ]
                        , dbTags = stateFromChunk []
                        }

            let wifiC4 = eventAt deviceA 700
                eth5 = eventAt deviceB 500
                aProfs = [profileOp wifiC4 eth1 (SimpleProfile "wifi-c-4" WirelessClient Unconfirmed)]
                bProfs = [profileOp eth5 wifiC2 (SimpleProfile "eth-5" Wired Unconfirmed)]
            let (replicaA, _) = bidirectionalResync baseDB (aProfs, []) (bProfs, [])

            -- Expected: eth-1@1000 → wifi-c-2@800 → wifi-c-4@700 → wifi-h-3@600 → eth-5@500
            assertProfileIds
                "resync profiles: base=[eth-1@t=1000, wifi-c-2@t=800, wifi-h-3@t=600]; A adds wifi-c-4@t=700; B adds eth-5@t=500"
                ["eth-1", "wifi-c-2", "wifi-c-4", "wifi-h-3", "eth-5"]
                (dbProfiles replicaA)
        , testCase "[BUG] resync preserves all tags across entities" $ do
            -- Same test for the tags entity independently.
            let tOutdoor = eventAt deviceA 1000
                tPrimary = eventAt deviceA 800
                tBackup = eventAt deviceA 600
                baseDB =
                    SimpleSwarmDB
                        { dbProfiles = stateFromChunk []
                        , dbTags =
                            stateFromChunk
                                [ tagStateOp tOutdoor "outdoor"
                                , tagStateOp tPrimary "primary"
                                , tagStateOp tBackup "backup"
                                ]
                        }

            let tMobile = eventAt deviceA 700
                tTemp = eventAt deviceB 500
                aTags = [tagInsertOp tMobile tOutdoor "mobile"]
                bTags = [tagInsertOp tTemp tPrimary "temporary"]
            let (replicaA, _) = bidirectionalResync baseDB ([], aTags) ([], bTags)

            -- Expected: outdoor@1000 → primary@800 → mobile@700 → backup@600 → temporary@500
            assertTagOrder
                "resync tags: base=[outdoor@t=1000, primary@t=800, backup@t=600]; A adds mobile@t=700; B adds temporary@t=500"
                ["outdoor", "primary", "mobile", "backup", "temporary"]
                (dbTags replicaA)
        , testCase "resync with non-interleaving patches always works" $ do
            -- When only one replica makes changes and the other is passive,
            -- resync simply appends the patch — no merge with a competing
            -- suffix, so the HashMap bias bug cannot trigger.
            -- This test passes with or without the fix.
            let eth1 = eventAt deviceA 1000
                wifiC2 = eventAt deviceA 800
                tOutdoor = eventAt deviceA 1000
                tPrimary = eventAt deviceA 800
                baseDB =
                    SimpleSwarmDB
                        { dbProfiles =
                            stateFromChunk
                                [ profileStateOp eth1 (SimpleProfile "eth-1" Wired Confirmed)
                                , profileStateOp wifiC2 (SimpleProfile "wifi-c-2" WirelessClient Confirmed)
                                ]
                        , dbTags =
                            stateFromChunk
                                [ tagStateOp tOutdoor "outdoor"
                                , tagStateOp tPrimary "primary"
                                ]
                        }

            -- Only device A makes changes; device B is passive (no ops)
            let wifiH3 = eventAt deviceA 2000
                aProfs = [profileOp wifiH3 eth1 (SimpleProfile "wifi-h-3" WirelessHost Confirmed)]
                aTags = [tagInsertOp (eventAt deviceA 2000) tOutdoor "mobile"]
            let (replicaA, replicaB) = bidirectionalResync baseDB (aProfs, aTags) ([], [])

            assertConverge
                "profiles"
                (map show . liveProfiles)
                (dbProfiles replicaA)
                (dbProfiles replicaB)
            assertProfileCount
                "non-interleaving resync: base=[eth-1@t=1000, wifi-c-2@t=800]; A adds wifi-h-3@t=2000 (B passive)"
                3
                (dbProfiles replicaA)
        , testCase "resync with removals converges" $ do
            -- Device A adds a profile, Device B removes a different one.
            -- After bidirectional resync, both should see the same live set.
            let eth1 = eventAt deviceA 1000
                wifiC2 = eventAt deviceA 800
                wifiH3 = eventAt deviceA 600
                baseDB =
                    SimpleSwarmDB
                        { dbProfiles =
                            stateFromChunk
                                [ profileStateOp eth1 (SimpleProfile "eth-1" Wired Confirmed)
                                , profileStateOp wifiC2 (SimpleProfile "wifi-c-2" WirelessClient Confirmed)
                                , profileStateOp wifiH3 (SimpleProfile "wifi-h-3" WirelessHost Confirmed)
                                ]
                        , dbTags = stateFromChunk []
                        }

            -- Device A: add wifi-c-4 after eth-1
            let wifiC4 = eventAt deviceA 2000
                aProfs = [profileOp wifiC4 eth1 (SimpleProfile "wifi-c-4" WirelessClient Unconfirmed)]
            -- Device B: remove wifi-h-3
            let tombstone = eventAt deviceB 3000
                bProfs = [mkRemoval tombstone wifiH3]
            let (replicaA, replicaB) = bidirectionalResync baseDB (aProfs, []) (bProfs, [])

            assertConverge
                "profiles"
                (map show . liveProfiles)
                (dbProfiles replicaA)
                (dbProfiles replicaB)
            -- wifi-h-3 should be gone, wifi-c-4 should be present
            let profileIds = map profileId (liveProfiles (dbProfiles replicaA))
            assertBool
                ( unlines
                    [ ""
                    , "  Scenario:  resync with add+remove: A adds wifi-c-4@t=2000, B removes wifi-h-3@t=600"
                    , "  Expected:  wifi-c-4 present in live profiles"
                    , "  Received:  " ++ show (map Text.unpack profileIds)
                    ]
                )
                ("wifi-c-4" `elem` profileIds)
            assertBool
                ( unlines
                    [ ""
                    , "  Scenario:  resync with add+remove: A adds wifi-c-4@t=2000, B removes wifi-h-3@t=600"
                    , "  Expected:  wifi-h-3 NOT in live profiles (tombstoned)"
                    , "  Received:  " ++ show (map Text.unpack profileIds)
                    ]
                )
                ("wifi-h-3" `notElem` profileIds)
        , testCase "[BUG] resync order independence (A then B == B then A)" $ do
            -- The real resync uses traverseDBConcurrently_ — patches can
            -- arrive in any order. Final state must be the same regardless.
            let eth1 = eventAt deviceA 1000
                wifiC2 = eventAt deviceA 800
                wifiH3 = eventAt deviceA 600
                base =
                    stateFromChunk
                        [ profileStateOp eth1 (SimpleProfile "eth-1" Wired Confirmed)
                        , profileStateOp wifiC2 (SimpleProfile "wifi-c-2" WirelessClient Confirmed)
                        , profileStateOp wifiH3 (SimpleProfile "wifi-h-3" WirelessHost Confirmed)
                        ]
                wifiC4 = eventAt deviceA 700
                eth5 = eventAt deviceB 500
                aOps = [profileOp wifiC4 eth1 (SimpleProfile "wifi-c-4" WirelessClient Unconfirmed)]
                bOps = [profileOp eth5 wifiC2 (SimpleProfile "eth-5" Wired Unconfirmed)]

            -- Apply A then B
            let resultAB = applyRaw (applyRaw base aOps) bOps
            -- Apply B then A
            let resultBA = applyRaw (applyRaw base bOps) aOps

            assertConverge
                "profiles (order independence: A→B vs B→A)"
                (map show . liveProfiles)
                resultAB
                resultBA
            assertProfileCount
                "order independence: base=[eth-1@t=1000, wifi-c-2@t=800, wifi-h-3@t=600]; A adds wifi-c-4@t=700; B adds eth-5@t=500"
                5
                resultAB
        ]
