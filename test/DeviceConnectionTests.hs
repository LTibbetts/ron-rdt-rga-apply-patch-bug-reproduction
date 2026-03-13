{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

{- | Real-world scenario: device connection profiles.

Inspired by LumberjackConnections in las-types, which holds up to three
typed connection profiles (Wired, WirelessHost, WirelessClient) backed by
RON CRDTs so multiple replicas can update them concurrently.

Here we model two network-device replicas that each independently add
connection profiles into an RGA, then merge via applyPatches.  The bug
causes some concurrently-inserted profiles to become orphaned (silently
dropped from the linked list).
-}
module DeviceConnectionTests (deviceConnectionTests) where

import Helpers
import RON.Data (Reducible (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

deviceConnectionTests :: TestTree
deviceConnectionTests =
    testGroup
        "real-world: device connection profiles"
        [ testCase "[BUG] concurrent profile insertions from two replicas" $ do
            --
            -- Base state shared by both replicas:
            --   Eth1 (100) → WiFiClient2 (80) → WiFiHost3 (60)
            --
            -- State ops use refId=Zero (alive); order in list = linked-list order.
            let eth1 = SimpleProfile "eth-uuid-1" Wired Confirmed
                wifiC2 = SimpleProfile "wifi-c-uuid-2" WirelessClient Confirmed
                wifiH3 = SimpleProfile "wifi-h-uuid-3" WirelessHost Confirmed
                baseOps =
                    [ profileStateOp 100 eth1
                    , profileStateOp 80 wifiC2
                    , profileStateOp 60 wifiH3
                    ]
                baseState = stateFromChunk baseOps

            -- Replica A: insert WiFiClient4 (vid=70) after Eth1 (vid=100)
            --   70 < 80, so it interleaves: WiFiC2(80) wins, then WiFiC4(70) sits
            --   between WiFiC2 and WiFiH3.
            let wifiC4 = SimpleProfile "wifi-c-uuid-4" WirelessClient Unconfirmed
                replicaAOps = [profileOp 70 100 wifiC4]

            -- Replica B: insert Eth5 (vid=50) after WiFiClient2 (vid=80)
            --   50 < 60, so it interleaves after WiFiH3(60) from WiFiH3's
            --   perspective — Eth5 sits at the tail after WiFiH3.
            let eth5 = SimpleProfile "eth-uuid-5" Wired Unconfirmed
                replicaBOps = [profileOp 50 80 eth5]

            -- Merge both replica patches into the base state
            let merged = applyRaw (applyRaw baseState replicaAOps) replicaBOps

            -- All five profiles should be reachable
            let profiles = liveProfiles merged
            assertEqual "all five profiles present" 5 (length profiles)

            -- Order should reflect RGA timestamp ordering:
            --   Eth1(100) → WiFiC2(80) → WiFiC4(70) → WiFiH3(60) → Eth5(50)
            let expectedIds = ["eth-uuid-1", "wifi-c-uuid-2", "wifi-c-uuid-4", "wifi-h-uuid-3", "eth-uuid-5"]
            assertEqual "profiles in correct RGA order" expectedIds (map profileId profiles)
        , testCase "[BUG] merged profiles preserve type and status metadata" $ do
            -- Same scenario as above; additionally verify type/status are intact.
            let baseOps =
                    [ profileStateOp 100 (SimpleProfile "eth-1" Wired Confirmed)
                    , profileStateOp 80 (SimpleProfile "wifi-c-2" WirelessClient Confirmed)
                    , profileStateOp 60 (SimpleProfile "wifi-h-3" WirelessHost Confirmed)
                    ]
                baseState = stateFromChunk baseOps
                replicaAOps = [profileOp 70 100 (SimpleProfile "wifi-c-4" WirelessClient Unconfirmed)]
                replicaBOps = [profileOp 50 80 (SimpleProfile "eth-5" Wired Unconfirmed)]
                merged = applyRaw (applyRaw baseState replicaAOps) replicaBOps
                profiles = liveProfiles merged

            -- Type and status checks on specific positions
            assertEqual "pos 0: Wired/Confirmed" (Wired, Confirmed) (profileType (profiles !! 0), profileStatus (profiles !! 0))
            assertEqual "pos 1: WirelessClient/Confirmed" (WirelessClient, Confirmed) (profileType (profiles !! 1), profileStatus (profiles !! 1))
            assertEqual "pos 2: WirelessClient/Unconfirmed" (WirelessClient, Unconfirmed) (profileType (profiles !! 2), profileStatus (profiles !! 2))
            assertEqual "pos 3: WirelessHost/Confirmed" (WirelessHost, Confirmed) (profileType (profiles !! 3), profileStatus (profiles !! 3))
            assertEqual "pos 4: Wired/Unconfirmed" (Wired, Unconfirmed) (profileType (profiles !! 4), profileStatus (profiles !! 4))
        , testCase "removing a profile tombstones it (profile no longer live)" $ do
            -- Build: Eth1(100) → WiFiC2(80) → WiFiH3(60)
            -- Tombstone WiFiC2 (vid=80).  liveProfiles should return only 2.
            let baseOps =
                    [ profileStateOp 100 (SimpleProfile "eth-1" Wired Confirmed)
                    , profileStateOp 80 (SimpleProfile "wifi-c-2" WirelessClient Confirmed)
                    , profileStateOp 60 (SimpleProfile "wifi-h-3" WirelessHost Confirmed)
                    ]
                baseState = stateFromChunk baseOps
                result = applyRaw baseState [mkRemoval 200 80]
            let profiles = liveProfiles result
            assertEqual "two profiles alive after removal" 2 (length profiles)
            assertEqual "surviving ids" ["eth-1", "wifi-h-3"] (map profileId profiles)
        , testCase "add then remove a profile leaves correct live set" $ do
            -- Start with Eth1 only, add WiFiC2 via patch, then remove WiFiC2.
            let baseState = stateFromChunk [profileStateOp 100 (SimpleProfile "eth-1" Wired Confirmed)]
                step1 = applyRaw baseState [profileOp 80 100 (SimpleProfile "wifi-c-2" WirelessClient Confirmed)]
                step2 = applyRaw step1 [mkRemoval 200 80]
            let profiles = liveProfiles step2
            assertEqual "only eth-1 remains" ["eth-1"] (map profileId profiles)
        ]
