# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

NSI-Safnari is an NSI (Network Services Interface) v2.0 Aggregator/Coordinator. It coordinates connections between NSI requesters and multiple NSI providers, managing the full lifecycle of network connections through state machines. Built with Scala 3.3.4, Play Framework 3.0.10, Pekko actors, and PostgreSQL.

## Build & Test Commands

```bash
sbt compile              # Compile
sbt test                 # Run all tests (requires PostgreSQL, see below)
sbt scalafmtAll          # Format all Scala sources
sbt scalafmtSbt          # Format *.sbt files
sbt scalafmtCheckAll scalafmtSbtCheck  # Check formatting (CI uses this)
sbt stage                # Build distribution package
sbt run                  # Run dev server (port 9000)
```

Single test: `sbt "testOnly nl.surfnet.safnari.ReservationStateMachineSpec"`

### Database Setup (for tests)

Tests require two PostgreSQL databases owned by `nsi-safnari-user` with password `password`:
- `nsi-safnari-dev` (development)
- `nsi-safnari-test` (tests, configured in `conf/test.conf`)

Tests run with `-Dconfig.file=conf/test.conf` which uses dummy PCE and NSI actors, and cleans the database on start/stop.

### Required Environment

- Java 21 (configured via `-release:21` in scalacOptions)
- SBT 1.10.2
- `GITHUB_TOKEN` environment variable for resolving `play-nsi-support` from GitHub Packages

## Architecture

### Message-Driven State Machines

The core design is multiple **finite state machines per connection**, orchestrated by a `ConnectionEntity`:

- **ReservationStateMachine** — The most complex FSM (~34KB). Handles InitialReserve, ModifyReserve, ReserveCommit, ReserveAbort. Manages PCE interaction, child reservation criteria, and timeout handling. States: InitialReservation -> Reserved -> CheckingReservation -> Held -> Committed -> Failed/Aborted/Timeout.
- **LifecycleStateMachine** — CREATED -> TERMINATING -> TERMINATED. In TERMINATING a repeated Terminate resends to the children still terminating, and a child's refusal that it holds nothing counts as terminated (`refusedAsTerminated`).
- **ProvisionStateMachine** — RELEASED -> PROVISIONING -> PROVISIONED
- **DataPlaneStateMachine** — Monitors and aggregates data plane status from child connections
- **QueryRecursiveStateMachine** — Handles recursive queries across child providers

All state machines are composed inside `ConnectionEntity` (`app/nl/surfnet/safnari/ConnectionEntity.scala`), which routes inbound messages to the appropriate FSM.

### Message Types (`app/nl/surfnet/safnari/Message.scala`)

Inbound: `FromRequester`, `FromProvider`, `FromPce`, `AckFromProvider`, `AckFromPce`, `MessageDeliveryFailure`, `PassedEndTime`
Outbound: `ToRequester`, `ToProvider`, `ToPce`

### Connection Management

`ConnectionManager` (`app/controllers/ConnectionManager.scala`) manages all active connections as Pekko actors. Uses Scala STM (Software Transactional Memory) for thread-safe in-memory maps. On startup, replays persisted messages from PostgreSQL to restore connection state (event sourcing pattern).

### Key Flow (Initial Reserve)

1. Requester POSTs to `/nsi-v2/ConnectionServiceProvider` (SOAP)
2. `ConnectionProviderController` -> `ConnectionManager` creates a `ConnectionEntity` actor
3. `ConnectionEntity` routes to `ReservationStateMachine`
4. RSM sends `ToPce` (path computation request) and `ToProvider` (child reserves)
5. Responses arrive via `/nsi-v2/ConnectionServiceRequester` and `/pce/reply`
6. RSM aggregates child states, transitions to Held/Committed/Failed
7. Result callback sent to requester via `NsiWebService`

### PCE Integration (`app/controllers/PathComputationEngine.scala`)

Uses `Continuations[T]` — a correlation ID to Promise mapping with timeout support. PCE algorithms: CHAIN, TREE (simultaneous), SEQUENTIAL.

### Key Dependency: play-nsi-support

The `play-nsi-support` library (separate repo: `BandwidthOnDemand/play-nsi-support`) provides NSI protocol types, FSM base classes, SOAP message handling, and NSI operation definitions. Published to GitHub Packages. Changes to NSI protocol handling often require updating this library first.

### Database

PostgreSQL with Play Evolutions (`conf/evolutions/default/1.sql` through `7.sql`). Primary table: `messages` — stores all inbound/outbound NSI messages for replay-based recovery.

## CI

GitHub Actions (`.github/workflows/scala.yml`): runs `sbt test` then `sbt scalafmtCheckAll scalafmtSbtCheck` on pushes/PRs to master. Always check formatting before pushing.
