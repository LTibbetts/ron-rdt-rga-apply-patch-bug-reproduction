# RON RGA `applyPatch` Bug Demonstration

Demonstrates a bug in the [RON](https://github.com/ff-notes/ron) RGA (Replicated Growable Array) implementation where `HashMap.union` bias in `applyPatch` causes interleaved patch vertices to become orphaned.

## The Bug

In `RON.Data.RGA.applyPatch` (~line 298), the expression:

```haskell
pure $ HashMap.insert parent item' targetItems <> newItems
```

uses `HashMap.union` (via `<>`), which is **left-biased**. `targetItems` contains **stale** `itemNext` pointers that override the **correct** pointers from `newItems` (the merge result). This causes vertices that should interleave into the linked list to become orphaned and silently dropped.

**Fix** — swap operands so `newItems` takes precedence:

```haskell
pure $ HashMap.insert parent item' $ HashMap.union newItems targetItems
```

## Setup

### Clone

```bash
git clone <repo-url>
cd ron-bug-example
git submodule update --init --recursive
```

### Build Environment

This project uses [devenv](https://devenv.sh/) to provide GHC 9.6 and cabal-install:

```bash
devenv shell
```

### Run Tests

By default the bug is present — 12 of 29 tests fail:

```bash
cabal test
```

### Apply the Fix

```bash
git -C ron apply ../fix-rga-applyPatch.patch
cabal test
```

All 29 tests pass. To revert back to the buggy state:

```bash
git -C ron checkout -- ron-rdt/lib/RON/Data/RGA.hs
```

## Test Suite

29 tests across 6 groups:

| Group                        | Tests | Description                                                            |
| ---------------------------- | ----- | ---------------------------------------------------------------------- |
| `applyPatch correctness`     | 7     | Core interleaving scenarios (5 `[BUG]`, 2 always-pass)                 |
| `removal operations`         | 4     | Tombstone semantics                                                    |
| `consistency`                | 4     | Roundtrip, Semigroup agreement, commutativity                          |
| `edge cases`                 | 4     | Empty patch, single vertex, nonexistent parent, mixed ops              |
| `device connection profiles` | 4     | Real-world scenario inspired by production product types               |
| `swarm resync`               | 6     | Multi-entity bidirectional sync inspired by a RON swarm implementation |

Tests marked `[BUG]` fail when the bug is present and pass when fixed. All other tests pass regardless.
