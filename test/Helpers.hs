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

  UUID encoding: This test suite uses the real RON event UUID bit layout:
    x = [Epoch:2][AppSpecific:2][timeValue:60]
    y = [variant=00:2][version=10:2][replicaOrigin:60]
  so UUID ordering matches production: primarily by time, with replica
  origin as tiebreaker.
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Helpers (
    -- * UUID generation (models RON.Event + RON.Epoch)
    ReplicaId,
    mkReplicaId,
    EventTime,
    mkEventTime,
    mkEventUuid,
    eventTime,
    replicaOrigin,
    eventAt,
    eventsAt,

    -- * Simulated monotonic clock (models RON.Epoch.EpochClock)
    TestClock (..),
    mkClock,
    tick,
    tickN,

    -- * RGA helpers
    mkState,
    mkInsertAfter,
    mkRemoval,
    applyRaw,
    getEventTimes,
    getTimesWithRef,

    -- * Connection-profile types (inspired by <nmcli profiles>)
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

    -- * Structured assertions
    assertRgaOrder,
    assertProfileIds,
    assertProfileCount,
    assertTagOrder,
    assertConverge,
    assertTombstones,
) where

import Data.Bits ((.&.), (.|.))
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import RON.Data (Reducible (..))
import RON.Data.RGA (RgaRep)
import RON.Types (Atom (..), Op (..), UUID (..))
import RON.UUID (pattern Zero)
import Test.Tasty.HUnit (assertEqual)

-- ============================================================================
-- UUID generation (models RON.Event + RON.Epoch)
--
-- In the real system (RON.Swarm.Internal.DB + RON.Storage.Sqlite):
--
--   1. Each device gets a ReplicaId from its MAC address or a random value,
--      truncated to 60 bits (leastSignificant60).
--      See: RON.Storage.Sqlite.newHandle, RON.Storage.Sqlite.getMacAddress
--
--   2. A monotonic EpochClock tracks time in hundreds of nanoseconds since
--      the RFC 4122 epoch (1582). An IORef ensures strict monotonicity:
--      each new event gets max(wallClock, lastEvent + 1).
--      See: RON.Epoch.EpochClockT, RON.Epoch.getEvents
--
--   3. Event UUIDs are encoded by RON.Event.encodeEvent as:
--        x = [Epoch:2][AppSpecific:2][timeValue:60]
--        y = [variant=00:2][version=10:2][replicaOrigin:60]
--
--   4. UUID ordering (derived Ord on (Word64, Word64)) sorts primarily by
--      time (in x), with replica origin (in y) as tiebreaker. The RGA
--      merge algorithm uses this ordering to determine vertex position.
-- ============================================================================

{- | Replica identity. In production, derived from the device's MAC address
or a random value, then truncated to 60 bits.
See: RON.Storage.Sqlite.newHandle, RON.Event.Replica
-}
newtype ReplicaId = ReplicaId Word64
    deriving (Eq, Show)

-- | Create a replica ID, masking to 60 bits like the real leastSignificant60.
mkReplicaId :: Word64 -> ReplicaId
mkReplicaId = ReplicaId . (.&. 0x0FFFFFFFFFFFFFFF)

{- | Monotonic event timestamp. In production, this is hundreds of nanoseconds
since the RFC 4122 epoch (1582), maintained by an IORef that only advances.
See: RON.Epoch.EpochClockT, RON.Epoch.getEvents
-}
newtype EventTime = EventTime Word64
    deriving (Eq, Ord, Show)

mkEventTime :: Word64 -> EventTime
mkEventTime = EventTime

{- | Encode an event UUID matching the real RON bit layout.

Real encoding (from RON.Event.encodeEvent):
  x = varietyAndValue .|. originVariety
    where Time varietyAndValue = mkTime Epoch timeValue
          originVariety = fst (encodeReplicaId (mkReplica ApplicationSpecific origin))
  y = eventVersion .|. origin
    where eventVersion = 0x_2000_0000_0000_0000

For Epoch time (variety=B10) + ApplicationSpecific origin (variety=B11):
  x = 0xB000000000000000 .|. timeValue60
  y = 0x2000000000000000 .|. replicaOrigin60

UUID Ord compares (x, y) lexicographically, so ordering is:
  1. Primarily by time (60-bit value in x)
  2. Tiebreaker by replica origin (60-bit value in y)
-}
mkEventUuid :: EventTime -> ReplicaId -> UUID
mkEventUuid (EventTime t) (ReplicaId r) =
    UUID
        ( 0xB000000000000000 -- Epoch(10) .|. AppSpecific(11) in bits 63-60
            .|. (t .&. 0x0FFFFFFFFFFFFFFF)
        )
        ( 0x2000000000000000 -- variant(00) .|. version(10=event) in bits 63-60
            .|. (r .&. 0x0FFFFFFFFFFFFFFF)
        )

{- | Extract the 60-bit event time from a UUID.
For Zero (alive sentinel), returns 0.
-}
eventTime :: UUID -> Word64
eventTime (UUID x _) = x .&. 0x0FFFFFFFFFFFFFFF

-- | Extract the 60-bit replica origin from a UUID.
replicaOrigin :: UUID -> Word64
replicaOrigin (UUID _ y) = y .&. 0x0FFFFFFFFFFFFFFF

-- | Create an event UUID at a specific time for a replica.
eventAt :: ReplicaId -> Word64 -> UUID
eventAt rid t = mkEventUuid (mkEventTime t) rid

-- | Create event UUIDs at specific times for a replica.
eventsAt :: ReplicaId -> [Word64] -> [UUID]
eventsAt rid = map (eventAt rid)

-- ============================================================================
-- Simulated monotonic clock (models RON.Epoch.EpochClock)
--
-- In production, EpochClock wraps (Replica, IORef Word60):
--   getEvents n = atomicModifyIORef' timeVar $ \cur ->
--       let begin = max realTime (succ cur)
--           end   = begin + pred n
--       in  (end, [Event (mkTime Epoch t) pid | t <- [begin..end]])
--
-- This pure model captures the same strict monotonicity guarantee:
-- each tick advances the clock, and sequential events get sequential times.
-- ============================================================================

-- | Simulated monotonic replica clock.
data TestClock = TestClock
    { clockReplica :: !ReplicaId
    , clockNextTime :: !Word64
    }
    deriving (Show)

{- | Initialize a clock for a replica at a starting epoch time.
Models: newHandleWithReplicaId + getCurrentEpochTime
-}
mkClock :: ReplicaId -> Word64 -> TestClock
mkClock = TestClock

{- | Generate one event UUID and advance the clock.
Models: RON.Event.getEventUuid
-}
tick :: TestClock -> (UUID, TestClock)
tick TestClock{clockReplica, clockNextTime} =
    ( mkEventUuid (mkEventTime clockNextTime) clockReplica
    , TestClock clockReplica (clockNextTime + 1)
    )

{- | Generate n sequential event UUIDs and advance the clock.
Models: RON.Event.getEventUuids
-}
tickN :: Int -> TestClock -> ([UUID], TestClock)
tickN n tc@TestClock{clockReplica, clockNextTime} =
    ( [ mkEventUuid (mkEventTime (clockNextTime + fromIntegral i)) clockReplica
      | i <- [0 .. n - 1]
      ]
    , tc{clockNextTime = clockNextTime + fromIntegral n}
    )

-- ============================================================================
-- RGA helpers
-- ============================================================================

{- | Build RgaRep state from event UUIDs in desired linked-list order.
Each vertex is alive (refId = Zero) with a payload carrying its event time.
UUIDs should be in descending order (newest = head of linked list).
-}
mkState :: [UUID] -> RgaRep
mkState uuids =
    stateFromChunk
        [ Op{opId = uuid, refId = Zero, payload = [AInteger (fromIntegral $ eventTime uuid)]}
        | uuid <- uuids
        ]

-- | Create insert ops for vertices, all inserting after the same parent.
mkInsertAfter :: UUID -> [UUID] -> [Op]
mkInsertAfter parent uuids =
    [ Op{opId = uuid, refId = parent, payload = [AInteger (fromIntegral $ eventTime uuid)]}
    | uuid <- uuids
    ]

-- | Create a removal op: tombstone targetVertex with tombstoneEvent.
mkRemoval :: UUID -> UUID -> Op
mkRemoval tombstoneEvent targetVertex =
    Op
        { opId = tombstoneEvent
        , refId = targetVertex
        , payload = []
        }

-- | Apply raw ops via the Reducible typeclass and return the new state.
applyRaw :: RgaRep -> [Op] -> RgaRep
applyRaw state ops = fst $ applyPatches state ([], ops)

{- | Extract event times in linked-list traversal order.
These are the 60-bit time values from each vertex's UUID.
-}
getEventTimes :: RgaRep -> [Word64]
getEventTimes state = [eventTime opId | Op{opId} <- stateToChunk state]

{- | Extract (eventTime, refTime) pairs for checking tombstone status.
Alive vertices have refTime = 0 (refId = Zero).
Tombstoned vertices have refTime = the tombstone event's time.
-}
getTimesWithRef :: RgaRep -> [(Word64, Word64)]
getTimesWithRef state =
    [ (eventTime opId, eventTime refId)
    | Op{opId, refId} <- stateToChunk state
    ]

-- ============================================================================
-- Connection-profile types (inspired by <nmcli profiles>)
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
profileOp :: UUID -> UUID -> SimpleProfile -> Op
profileOp vid parentId profile =
    Op
        { opId = vid
        , refId = parentId
        , payload = encodeProfile profile
        }

{- | Build a state op for a SimpleProfile vertex.
Use this when building initial state via stateFromChunk: refId = Zero (alive).
Ops are ordered in the desired linked-list sequence (first = head).
-}
profileStateOp :: UUID -> SimpleProfile -> Op
profileStateOp vid profile =
    Op
        { opId = vid
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
tagStateOp :: UUID -> Text -> Op
tagStateOp vid label =
    Op{opId = vid, refId = Zero, payload = [AString label]}

-- | Build a tag insert op (for patches via applyPatches).
tagInsertOp :: UUID -> UUID -> Text -> Op
tagInsertOp vid parentId label =
    Op{opId = vid, refId = parentId, payload = [AString label]}

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
-- connection profiles (like <nmcli profiles>) and device tags (a simple
-- string list). This lets us test that the applyPatch bug affects each
-- entity independently during a multi-entity resync.
-- ============================================================================

{- | A simplified SwarmDB holding two independent entity types.
Mirrors the real SwarmDB GADT but without the type-level list machinery.
-}
data SimpleSwarmDB = SimpleSwarmDB
    { dbProfiles :: RgaRep
    -- ^ Connection profiles entity (<nmcli profiles>-style)
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

-- ============================================================================
-- Structured assertions
--
-- Each assertion helper produces a clear failure message showing:
--   • Input:    what state we started with
--   • Applied:  what operations were performed
--   • Expected: what the result should be
--   • Received: what the result actually was
-- ============================================================================

{- | Assert RGA vertex ordering (by event time) after applying patches.
On failure shows the full input state, patch description, and expected
vs actual event times.
-}
assertRgaOrder ::
    (HasCallStack) =>
    -- | Initial state event times
    [Word64] ->
    -- | Description of patches applied
    String ->
    -- | Expected event times after patching
    [Word64] ->
    -- | Actual result state
    RgaRep ->
    IO ()
assertRgaOrder inputTimes patchDesc expectedTimes result =
    let actualTimes = getEventTimes result
     in assertEqual
            ( unlines
                [ ""
                , "  Input:    times = " ++ show inputTimes
                , "  Applied:  " ++ patchDesc
                , "  Expected: " ++ show expectedTimes
                , "  Received: " ++ show actualTimes
                ]
            )
            expectedTimes
            actualTimes

-- | Assert the live profile IDs (in order) after operations on an RGA.
assertProfileIds ::
    (HasCallStack) =>
    -- | Description of the scenario (base state + operations)
    String ->
    -- | Expected profile IDs in order
    [Text] ->
    -- | Actual result state
    RgaRep ->
    IO ()
assertProfileIds scenario expectedIds result =
    let actual = map profileId (liveProfiles result)
     in assertEqual
            ( unlines
                [ ""
                , "  Scenario: " ++ scenario
                , "  Expected profile IDs: " ++ show (map Text.unpack expectedIds)
                , "  Received profile IDs: " ++ show (map Text.unpack actual)
                ]
            )
            expectedIds
            actual

-- | Assert the number of live profiles in an RGA state.
assertProfileCount ::
    (HasCallStack) =>
    -- | Description of the scenario
    String ->
    -- | Expected number of live profiles
    Int ->
    -- | Actual result state
    RgaRep ->
    IO ()
assertProfileCount scenario expected result =
    let profiles = liveProfiles result
        actual = length profiles
        ids = map (Text.unpack . profileId) profiles
     in assertEqual
            ( unlines
                [ ""
                , "  Scenario:          " ++ scenario
                , "  Expected count:    " ++ show expected
                , "  Received count:    " ++ show actual
                , "  Received profiles: " ++ show ids
                ]
            )
            expected
            actual

-- | Assert the live tag labels (in order) after operations on an RGA.
assertTagOrder ::
    (HasCallStack) =>
    -- | Description of the scenario
    String ->
    -- | Expected tag labels in order
    [Text] ->
    -- | Actual result state
    RgaRep ->
    IO ()
assertTagOrder scenario expectedTags result =
    let actual = liveTags result
     in assertEqual
            ( unlines
                [ ""
                , "  Scenario:       " ++ scenario
                , "  Expected tags:  " ++ show (map Text.unpack expectedTags)
                , "  Received tags:  " ++ show (map Text.unpack actual)
                ]
            )
            expectedTags
            actual

{- | Assert two RGA states have converged (same live values).
Works for both profiles and tags by comparing via a caller-supplied extractor.
-}
assertConverge ::
    (HasCallStack) =>
    -- | What entity we're checking (e.g. "profiles", "tags")
    String ->
    -- | Extractor to get printable values from state
    (RgaRep -> [String]) ->
    -- | Replica A state
    RgaRep ->
    -- | Replica B state
    RgaRep ->
    IO ()
assertConverge entity extract replicaA replicaB =
    let valsA = extract replicaA
        valsB = extract replicaB
     in assertEqual
            ( unlines
                [ ""
                , "  Check:     " ++ entity ++ " convergence after bidirectional resync"
                , "  Replica A: " ++ show valsA
                , "  Replica B: " ++ show valsB
                ]
            )
            valsA
            valsB

{- | Assert tombstone (eventTime, refTime) pairs in the result state.
Alive vertices have refTime = 0, tombstoned vertices have
refTime = tombstone event's time.
-}
assertTombstones ::
    (HasCallStack) =>
    -- | Initial state event times
    [Word64] ->
    -- | Description of removals applied
    String ->
    -- | Expected (eventTime, refTime) pairs
    [(Word64, Word64)] ->
    -- | Actual result state
    RgaRep ->
    IO ()
assertTombstones inputTimes removalDesc expectedRefs result =
    let actualRefs = getTimesWithRef result
        formatRef (t, 0) = "t=" ++ show t ++ " (alive)"
        formatRef (t, ref) = "t=" ++ show t ++ " (tombstoned at t=" ++ show ref ++ ")"
     in assertEqual
            ( unlines
                [ ""
                , "  Input:    times = " ++ show inputTimes
                , "  Applied:  " ++ removalDesc
                , "  Expected: " ++ intercalate ", " (map formatRef expectedRefs)
                , "  Received: " ++ intercalate ", " (map formatRef actualRefs)
                ]
            )
            expectedRefs
            actualRefs
