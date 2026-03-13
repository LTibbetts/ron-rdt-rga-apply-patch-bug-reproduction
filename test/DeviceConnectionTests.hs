{-# LANGUAGE OverloadedStrings #-}

{- | Real-world scenario: device connection profiles.

Inspired by ncmli profiles, which holds up to three
typed connection profiles (Wired, WirelessHost, WirelessClient) backed by
RON CRDTs so multiple replicas can update them concurrently.

UUIDs are now realistic event UUIDs: each device has a ReplicaId (from MAC)
and profiles are created at specific epoch times. The interleaving bug
triggers when device B's profile UUIDs (from B's clock) fall between
device A's existing profile UUIDs (from A's clock).
-}
module DeviceConnectionTests (deviceConnectionTests) where

import Helpers
import RON.Data (Reducible (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

-- | Two devices, each with a MAC-derived replica ID.
deviceA, deviceB :: ReplicaId
deviceA = mkReplicaId 0xAABBCCDDEE01 -- e.g., device A's MAC
deviceB = mkReplicaId 0x112233445566 -- e.g., device B's MAC

deviceConnectionTests :: TestTree
deviceConnectionTests =
    testGroup
        "real-world: device connection profiles"
        [ testCase "[BUG] concurrent profile insertions from two replicas" $ do
            -- Device A created 3 profiles over time (t=1000, t=800, t=600)
            let eth1 = eventAt deviceA 1000
                wifiC2 = eventAt deviceA 800
                wifiH3 = eventAt deviceA 600
                baseState =
                    stateFromChunk
                        [ profileStateOp eth1 (SimpleProfile "eth-uuid-1" Wired Confirmed)
                        , profileStateOp wifiC2 (SimpleProfile "wifi-c-uuid-2" WirelessClient Confirmed)
                        , profileStateOp wifiH3 (SimpleProfile "wifi-h-uuid-3" WirelessHost Confirmed)
                        ]
                -- Device A adds WiFiClient4 (t=700) after Eth1 → between WiFiC2 and WiFiH3
                wifiC4 = eventAt deviceA 700
                replicaAOps = [profileOp wifiC4 eth1 (SimpleProfile "wifi-c-uuid-4" WirelessClient Unconfirmed)]
                -- Device B adds Eth5 (t=500) after WiFiC2 → after WiFiH3
                eth5 = eventAt deviceB 500
                replicaBOps = [profileOp eth5 wifiC2 (SimpleProfile "eth-uuid-5" Wired Unconfirmed)]
                merged = applyRaw (applyRaw baseState replicaAOps) replicaBOps

            assertProfileCount
                "base=[eth-1, wifi-c-2, wifi-h-3]; A inserts wifi-c-4 after eth-1; B inserts eth-5 after wifi-c-2"
                5
                merged
            assertProfileIds
                "base=[eth-1@t=1000, wifi-c-2@t=800, wifi-h-3@t=600]; A inserts wifi-c-4@t=700 after eth-1; B inserts eth-5@t=500 after wifi-c-2"
                ["eth-uuid-1", "wifi-c-uuid-2", "wifi-c-uuid-4", "wifi-h-uuid-3", "eth-uuid-5"]
                merged
        , testCase "[BUG] merged profiles preserve type and status metadata" $ do
            let eth1 = eventAt deviceA 1000
                wifiC2 = eventAt deviceA 800
                wifiH3 = eventAt deviceA 600
                baseState =
                    stateFromChunk
                        [ profileStateOp eth1 (SimpleProfile "eth-1" Wired Confirmed)
                        , profileStateOp wifiC2 (SimpleProfile "wifi-c-2" WirelessClient Confirmed)
                        , profileStateOp wifiH3 (SimpleProfile "wifi-h-3" WirelessHost Confirmed)
                        ]
                wifiC4 = eventAt deviceA 700
                eth5 = eventAt deviceB 500
                merged =
                    applyRaw
                        (applyRaw baseState [profileOp wifiC4 eth1 (SimpleProfile "wifi-c-4" WirelessClient Unconfirmed)])
                        [profileOp eth5 wifiC2 (SimpleProfile "eth-5" Wired Unconfirmed)]
                profiles = liveProfiles merged
                scenario =
                    "base=[eth-1(W/C), wifi-c-2(WC/C), wifi-h-3(WH/C)]; "
                        ++ "A inserts wifi-c-4(WC/U)@t=700; B inserts eth-5(W/U)@t=500"

            let check pos expectedType expectedStatus = do
                    let p = profiles !! pos
                        actual = (profileType p, profileStatus p)
                        expected = (expectedType, expectedStatus)
                    assertEqual
                        ( unlines
                            [ ""
                            , "  Scenario:   " ++ scenario
                            , "  Position:   " ++ show pos ++ " (" ++ show (profileId p) ++ ")"
                            , "  Expected:   " ++ show expected
                            , "  Received:   " ++ show actual
                            ]
                        )
                        expected
                        actual

            assertProfileCount scenario 5 merged
            check 0 Wired Confirmed
            check 1 WirelessClient Confirmed
            check 2 WirelessClient Unconfirmed
            check 3 WirelessHost Confirmed
            check 4 Wired Unconfirmed
        , testCase "removing a profile tombstones it (profile no longer live)" $ do
            let eth1 = eventAt deviceA 1000
                wifiC2 = eventAt deviceA 800
                wifiH3 = eventAt deviceA 600
                baseState =
                    stateFromChunk
                        [ profileStateOp eth1 (SimpleProfile "eth-1" Wired Confirmed)
                        , profileStateOp wifiC2 (SimpleProfile "wifi-c-2" WirelessClient Confirmed)
                        , profileStateOp wifiH3 (SimpleProfile "wifi-h-3" WirelessHost Confirmed)
                        ]
                tombstone = eventAt deviceA 2000
                result = applyRaw baseState [mkRemoval tombstone wifiC2]

            assertProfileCount
                "base=[eth-1, wifi-c-2, wifi-h-3]; tombstone wifi-c-2@t=800"
                2
                result
            assertProfileIds
                "base=[eth-1, wifi-c-2, wifi-h-3]; tombstone wifi-c-2@t=800"
                ["eth-1", "wifi-h-3"]
                result
        , testCase "add then remove a profile leaves correct live set" $ do
            let eth1 = eventAt deviceA 1000
                baseState = stateFromChunk [profileStateOp eth1 (SimpleProfile "eth-1" Wired Confirmed)]
                wifiC2 = eventAt deviceA 800
                step1 = applyRaw baseState [profileOp wifiC2 eth1 (SimpleProfile "wifi-c-2" WirelessClient Confirmed)]
                tombstone = eventAt deviceA 2000
                step2 = applyRaw step1 [mkRemoval tombstone wifiC2]

            assertProfileIds
                "base=[eth-1]; add wifi-c-2@t=800 after eth-1; tombstone wifi-c-2"
                ["eth-1"]
                step2
        ]
