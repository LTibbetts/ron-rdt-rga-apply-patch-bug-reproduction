{-
  Shared helpers, types, and simplified domain models for the RON RGA test suite.

  Bug under test: In RON.Data.RGA.applyPatch (line ~298), the expression
    HashMap.insert parent item' targetItems <> newItems
  uses left-biased HashMap.union. targetItems has STALE itemNext pointers
  that override the CORRECT pointers from newItems (the merge result).
  This causes interleaved patch vertices to become orphaned.

  Fix: swap operands so newItems takes precedence:
    HashMap.insert parent item' $ HashMap.union newItems targetItems

  Source: https://github.com/ff-notes/ron/blob/master/ron-rdt/lib/RON/Data/RGA.hs
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Helpers (
    -- * RGA helpers
    mkUuid,
    mkState,
    mkInsertAfter,
    mkRemoval,
    applyRaw,
    getIds,
    getIdsWithRef,

    -- * Connection-profile types (inspired by LumberjackConnections)
    ConnType (..),
    ConnStatus (..),
    SimpleProfile (..),
    encodeProfile,
    decodeProfile,
    profileOp,
    profileStateOp,
    liveProfiles,

    -- * Tag helpers
    tagStateOp,
    tagInsertOp,
    liveTags,

    -- * SwarmDB model (inspired by RON.Swarm.Internal.DB)
    SimpleSwarmDB (..),
    resyncDB,
    bidirectionalResync,
) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word64)
import RON.Data (Reducible (..))
import RON.Data.RGA (RgaRep)
import RON.Types (Atom (..), Op (..), UUID (..))
import RON.UUID (pattern Zero)

-- ============================================================================
-- RGA helpers
-- ============================================================================

-- | Build a UUID from an integer. Compares naturally by value.
mkUuid :: Word64 -> UUID
mkUuid n = UUID n 0

{- | Build RgaRep state from vertex IDs in desired linked-list order.
Each vertex is alive (refId = Zero) with a payload carrying its ID.
-}
mkState :: [Word64] -> RgaRep
mkState vids =
    stateFromChunk
        [ Op{opId = mkUuid vid, refId = Zero, payload = [AInteger (fromIntegral vid)]}
        | vid <- vids
        ]

-- | Create insert ops for vertices, all inserting after the same parent.
mkInsertAfter :: Word64 -> [Word64] -> [Op]
mkInsertAfter parent vids =
    [ Op{opId = mkUuid vid, refId = mkUuid parent, payload = [AInteger (fromIntegral vid)]}
    | vid <- vids
    ]

-- | Create a removal op: tombstone targetVertex with tombstoneEvent.
mkRemoval :: Word64 -> Word64 -> Op
mkRemoval tombstoneEvent targetVertex =
    Op
        { opId = mkUuid tombstoneEvent
        , refId = mkUuid targetVertex
        , payload = []
        }

-- | Apply raw ops via the Reducible typeclass and return the new state.
applyRaw :: RgaRep -> [Op] -> RgaRep
applyRaw state ops = fst $ applyPatches state ([], ops)

-- | Extract vertex IDs in linked-list traversal order.
getIds :: RgaRep -> [Word64]
getIds state = [x | Op{opId = UUID x _} <- stateToChunk state]

-- | Extract (vertexId, refId) pairs for checking tombstone status.
getIdsWithRef :: RgaRep -> [(Word64, Word64)]
getIdsWithRef state =
    [ (x, r)
    | Op{opId = UUID x _, refId = UUID r _} <- stateToChunk state
    ]

-- ============================================================================
-- Connection-profile types (inspired by LumberjackConnections)
--
-- In the real codebase (las-types/src/LAS/Types/Connection.hs) a device holds
-- up to three typed connection profiles (Wired, WirelessHost, WirelessClient)
-- backed by RON CRDTs so multiple replicas can update them concurrently.  Here
-- we model the same idea with plain Haskell values stored as RGA vertices so
-- we can exercise the applyPatch bug in a realistic scenario.
-- ============================================================================

data ConnType = Wired | WirelessHost | WirelessClient
    deriving (Show, Eq)

data ConnStatus = Confirmed | Unconfirmed
    deriving (Show, Eq)

data SimpleProfile = SimpleProfile
    { profileId :: Text
    , profileType :: ConnType
    , profileStatus :: ConnStatus
    }
    deriving (Show, Eq)

-- | Encode a ConnType as an Int64 atom.
encodeType :: ConnType -> Int64
encodeType Wired = 0
encodeType WirelessHost = 1
encodeType WirelessClient = 2

-- | Encode a ConnStatus as an Int64 atom.
encodeStatus :: ConnStatus -> Int64
encodeStatus Confirmed = 0
encodeStatus Unconfirmed = 1

-- | Decode ConnType from an Int64 (partial – unknown values become Wired).
decodeType :: Int64 -> ConnType
decodeType 1 = WirelessHost
decodeType 2 = WirelessClient
decodeType _ = Wired

-- | Decode ConnStatus from an Int64 (partial – unknown values become Confirmed).
decodeStatus :: Int64 -> ConnStatus
decodeStatus 1 = Unconfirmed
decodeStatus _ = Confirmed

{- | Encode a profile as the atom payload of one RGA vertex.
Layout: [AString profileId, AInteger connType, AInteger connStatus]
-}
encodeProfile :: SimpleProfile -> [Atom]
encodeProfile SimpleProfile{profileId, profileType, profileStatus} =
    [ AString profileId
    , AInteger (encodeType profileType)
    , AInteger (encodeStatus profileStatus)
    ]

{- | Decode a profile from the atom payload of one RGA vertex.
Returns Nothing for tombstoned vertices (empty payload).
-}
decodeProfile :: [Atom] -> Maybe SimpleProfile
decodeProfile [AString pid, AInteger pt, AInteger ps] =
    Just $
        SimpleProfile
            { profileId = pid
            , profileType = decodeType pt
            , profileStatus = decodeStatus ps
            }
decodeProfile _ = Nothing

{- | Build an insert op that encodes a SimpleProfile as an RGA vertex.
Use this when passing ops to applyPatches (refId = parent vertex).
-}
profileOp :: Word64 -> Word64 -> SimpleProfile -> Op
profileOp vid parentId profile =
    Op
        { opId = mkUuid vid
        , refId = mkUuid parentId
        , payload = encodeProfile profile
        }

{- | Build a state op for a SimpleProfile vertex.
Use this when building initial state via stateFromChunk: refId = Zero (alive).
Ops are ordered in the desired linked-list sequence (first = head).
-}
profileStateOp :: Word64 -> SimpleProfile -> Op
profileStateOp vid profile =
    Op
        { opId = mkUuid vid
        , refId = Zero
        , payload = encodeProfile profile
        }

{- | Extract the live (non-tombstoned) profiles in RGA traversal order.
A vertex is alive when refId = Zero (the sentinel for "not tombstoned").
-}
liveProfiles :: RgaRep -> [SimpleProfile]
liveProfiles state =
    [ p
    | Op{opId = _, refId = Zero, payload = atoms} <- stateToChunk state
    , Just p <- [decodeProfile atoms]
    ]

-- ============================================================================
-- Tag helpers
-- ============================================================================

-- | Build a tag state op. Tags are simple AString payloads.
tagStateOp :: Word64 -> Text -> Op
tagStateOp vid label =
    Op{opId = mkUuid vid, refId = Zero, payload = [AString label]}

-- | Build a tag insert op (for patches via applyPatches).
tagInsertOp :: Word64 -> Word64 -> Text -> Op
tagInsertOp vid parentId label =
    Op{opId = mkUuid vid, refId = mkUuid parentId, payload = [AString label]}

-- | Extract live tag labels from an RGA state.
liveTags :: RgaRep -> [Text]
liveTags state =
    [ t
    | Op{refId = Zero, payload = [AString t]} <- stateToChunk state
    ]

-- ============================================================================
-- SwarmDB model (inspired by RON.Swarm.Internal.DB)
--
-- In the real codebase (cardith/src/RON/Swarm/Internal/DB.hs):
--
--   data SwarmDB p (types :: [Type]) where
--     NullDB :: SwarmDB p '[]
--     (:.)   :: HasRONSwarm p a => State p a -> SwarmDB p xs -> SwarmDB p (a ': xs)
--
-- Each entity type gets its own RGA state. `resync` traverses all entities
-- concurrently and applies incoming patches via `applyPatches`. After resync,
-- both replicas should converge to the same state.
--
-- Here we model this with a plain product type holding two RGA entities:
-- connection profiles (like LumberjackConnections) and device tags (a simple
-- string list). This lets us test that the applyPatch bug affects each
-- entity independently during a multi-entity resync.
-- ============================================================================

{- | A simplified SwarmDB holding two independent entity types.
Mirrors the real SwarmDB GADT but without the type-level list machinery.
-}
data SimpleSwarmDB = SimpleSwarmDB
    { dbProfiles :: RgaRep
    -- ^ Connection profiles entity (LumberjackConnections-style)
    , dbTags :: RgaRep
    -- ^ Device tags entity (simple string labels)
    }
    deriving (Show, Eq)

{- | Simulate `resync` from DB.hs: apply pending patches to each entity
independently, as if receiving them from a remote replica over MQTT.
In the real code this is:
  resync = traverseDBConcurrently_ (\state -> requestSyncForAllKnownPartitions state)
which ultimately calls applyPatches on each entity's RON state.
-}
resyncDB :: SimpleSwarmDB -> [Op] -> [Op] -> SimpleSwarmDB
resyncDB SimpleSwarmDB{dbProfiles, dbTags} profilePatches tagPatches =
    SimpleSwarmDB
        { dbProfiles = applyRaw dbProfiles profilePatches
        , dbTags = applyRaw dbTags tagPatches
        }

{- | Simulate the full withSwarmDB lifecycle:
1. Initialize DB with base state (mkSwarmDB)
2. Apply local changes on each replica
3. Resync by exchanging patches (bidirectional)
4. Both replicas should converge

Returns (replicaA, replicaB) after bidirectional resync.
-}
bidirectionalResync ::
    -- | Shared base state
    SimpleSwarmDB ->
    -- | Replica A's local patches (profiles, tags)
    ([Op], [Op]) ->
    -- | Replica B's local patches (profiles, tags)
    ([Op], [Op]) ->
    (SimpleSwarmDB, SimpleSwarmDB)
bidirectionalResync base (aProfs, aTags) (bProfs, bTags) =
    let
        -- Each replica applies its own local changes first
        replicaA = resyncDB base aProfs aTags
        replicaB = resyncDB base bProfs bTags
        -- Then resync: A receives B's patches, B receives A's patches
        replicaA' = resyncDB replicaA bProfs bTags
        replicaB' = resyncDB replicaB aProfs aTags
     in
        (replicaA', replicaB')
