# nextpnr — Architecture Specification & Haskell Port Design

**Source of truth:** `nextpnr/` working tree at commit `51e703d0` (static: allow custom
delay prediction function). Tests submodule `nextpnr-tests` at `ce154121`.

This document extracts (1) the abstract architecture of nextpnr, (2) the algorithm
inventory per FPGA family, (3) the current test suite and its gaps, and (4) a Haskell
port design targeting maximum parallelism and high performance.

---

## 1. Overview

nextpnr is a portable, timing-driven FPGA place-and-route tool. The design is a
*common kernel + pluggable architecture* pattern:

```
 frontend (JSON) ──► pack() ──► place() ──► route() ──► bitstream / textcfg / SDF / SVG
                     ▲           ▲           ▲
                     └───────────┴───────────┴── Arch API (virtual, per-family implementation)
```

- The **kernel** (`common/`) is architecture-independent: netlist model, placement
  engines, routing engines, timing analysis, CLI, IO (JSON/SDC/SDF/SVG/report), GUI
  hooks, Python API.
- An **architecture** (e.g. `ice40/`, `ecp5/`, `generic/`, `nexus/`, `machxo2/`,
  `mistral/`, `himbaechel/`) implements the `Arch` API: the chip database (bel/wire/pip
  graph + delays + bitstream mapping), packing, placement constraints (bels, buckets,
  clusters, regions), and bitstream output.
- `himbaechel/` is a second-generation meta-architecture: a generic tile-based chipdb
  kernel plus per-vendor *uarch* plugins (`gatemate`, `gowin`, `ng-ultra`, `xilinx`,
  `example`).
- `generic/` is the reference/minimal architecture: flat in-memory arrays, ideal as the
  first Haskell port target and as a conformance oracle.

**Determinism is a first-class property**: all algorithms use a deterministic
xorshift64* RNG (`common/kernel/deterministic_rng.h`, seed `0x3141592653589793`), tie
breaking via random tags, and `sorted_shuffle`. Identical input ⇒ identical output
checksum (`ctx->checksum()`). The Haskell port must preserve this.

---

## 2. Repository Map

| Path | Role |
| --- | --- |
| `common/kernel/` | Netlist model, Arch API, BaseArch, Context, IdString, properties, timing engine, CLI, JSON/SDC/SDF/SVG/report IO, archcheck |
| `common/place/` | Placer engines: `placer1` (SA), `placer_heap` (HeAP/SimPL), `parallel_refine` (threaded SA), `placer_static` (analytic), `timing_opt` (timing-driven local opt), `detail_place_core` (thread-safe move machinery) |
| `common/route/` | `router1` (A*), `router2` (congestion-driven, multithreaded) |
| `generic/` | Reference arch; viaduct API + `example`/`fabulous` uarchs |
| `ice40/`, `ecp5/`, `machxo2/`, `nexus/`, `mistral/` | Classic per-family arches (POD chipdb blobs) |
| `himbaechel/` | Tile/shape/node chipdb meta-arch + uarch plugins |
| `bba/` | Binary Blob Assembler: builds position-independent binary chipdbs from python scripts |
| `frontend/` | JSON frontend (only remaining HDL frontend; Verilog moved to yosys `write_json`) |
| `python/`, `gui/` | Python bindings, interactive GUI |
| `rust/` | Optional Rust helper crate (example net printer) |
| `3rdparty/` | googletest, pybind11, Eigen, imgui, json11, oourafft, corrosion, … |
| `tests/` | Submodule `YosysHQ/nextpnr-tests`: regression flows + gtest sources |

### 2.1 Kernel data model

Central types (`common/kernel/nextpnr_types.h`, `basectx.h`):

- **`BaseCtx`** — the shared mutable state: idstring interning tables, `nets`
  (`dict<IdString, unique_ptr<NetInfo>>`), `cells` (`dict<IdString, unique_ptr<CellInfo>>`),
  hierarchy (non-leaf `HierarchicalCell` tree + `top_module`), `net_aliases`, top-level
  `ports` + `port_cells`, floorplan `region`s, `settings` (string-keyed config), context
  `attrs`, `timing_result`, UI-reload pools, and a main mutex + UI-priority mutex
  (`lock`/`yield` protocol so the GUI never starves).
- **`Context : Arch, DeterministicRNG`** — per-family context; adds delay prediction
  (`predictArcDelay`), net route delay queries, `checkRoutedDesign`, SDF/SVG/report
  writers, `checksum()`, `check()`, `archcheck()`, typed `setting<T>()` accessors.
- **`IdString`** — interned string: `int` index into a global-per-context table
  (`idstring_str_to_idx` / `idstring_idx_to_str`). `id("name")` interns; `name.c_str(ctx)`
  resolves. `IdStringList` = hierarchical name path (delimiter is arch-specific, e.g.
  `/` for generic, space for ice40). Porting note: interning must remain
  per-context and cheap; a global `IORef` table with `unsafePerformIO` read path is the
  natural Haskell equivalent.
- **`Property`** — variant value (int64/string/bool/vector of ints) used for cell
  attrs/params; JSON-compatible. `hashlib.h` provides `dict`/`pool` (open-addressing
  hash tables with deterministic iteration order).
- **`CellInfo`** — name, type, ports (`dict<IdString, PortInfo>`), attrs, params, bound
  `bel` + `PlaceStrength`, `cluster` id, `region`, optional `PseudoCell` (partial-reconf
  plugs), arch-specific payload via `ArchCellInfo` inheritance (e.g. ice40 `lcInfo`
  union: dffEnable, carryEnable, lutInputMask, clk/cen/sr pointers).
- **`NetInfo`** — name, driver `PortRef` + `users` (indexed_store), `wires` map
  (wire→uphill pip + strength, the routed tree), `constant_value`, `clkconstr`,
  `region`, attrs; `ArchNetInfo` payload (e.g. is_global/is_reset/is_enable).
- **`PortRef`/`PortInfo`** — cell+port pair; `store_index` for stable O(1) indexing.
- **Delays** — `delay_t` is `int` (ice40/ecp5/nexus/generic) or `double` (himbaechel
  xilinx); `DelayPair` (min,max), `DelayQuad` (min/max rise/fall). All delay math is
  closed over these two types.
- **`PlaceStrength`** — `STRENGTH_NONE < WEAK < STRONG < LOCKED`; stronger bindings
  resist ripup/legalisation.
- **`Region`** — floorplan constraint: name + optional bel/wire/pip pools.
- **`Loc`** — (x, y, z) tile coordinate.

### 2.2 The Arch API (`common/kernel/arch_api.h`)

The interface every family implements (`ArchAPI<R>` where `R` supplies range types;
`BaseArch<R>` provides default implementations using three flat maps
`base_bel2cell`, `base_wire2net`, `base_pip2net`). Method inventory by category:

- **Config**: `archId`, `getChipName`, `archArgs`/`archArgsToId`, grid dims
  (`getGridDimX/Y`, `getTileBelDimZ`, `getTilePipDimZ`), name delimiter.
- **Bels**: iteration/name lookup, checksum, `bindBel`/`unbindBel`, location,
  `getBelsByTile`, global-buffer flag, availability, bound/conflicting cell, type,
  hidden flag, attrs, pin→wire, pin type, pins, `getBelPinsForCellPin` (cell pin →
  candidate bel pins, drives LUT remapping).
- **Wires**: iteration/name, type, attrs, checksum, bind/unbind, availability,
  bound net, `getPipsDownhill/Uphill`, bel pins on wire, `getWireDelay`,
  `getWireConstantValue` (for constant nets).
- **Pips**: iteration/name, type, attrs, checksum, bind/unbind (binding a pip also
  binds its dst wire to the net — the core invariant), availability (per-net variant
  allows sharing), src/dst wire, delay, location, inverting flag.
- **Groups**: named sets of bels/wires/pips/groups (resource groups).
- **Delay model**: `predictDelay(src_bel, src_pin, dst_bel, dst_pin)` — the placement
  cost oracle; `estimateDelay(src_wire, dst_wire)` — the A* heuristic; `getDelayEpsilon`,
  `getRipupDelayPenalty`, `getDelayNS/FromNS`, `getDelayChecksum`, `getRouteBoundingBox`,
  `getArcDelayOverride`, `expandBoundingBox`.
- **Decals** (GUI): `getDecalGraphics`, `getBel/Wire/Pip/GroupDecal`.
- **Cell timing**: `getCellDelay`, `getPortTimingClass` (9 classes: clock input,
  gen clock, reg in/out, comb in/out, startpoint, endpoint, ignore), `getPortClockingInfo`
  (setup/hold, clock-to-Q, edge).
- **Placement validity**: `isValidBelForCellType`, bel buckets
  (`getBelBucketName/ByBel/ForCellType`, `getBelsInBucket` — the placer's
  cell-type→bel candidate index), `isBelLocationValid`, `getCellTypes`.
- **Clusters**: `getClusterRootCell`, `getClusterBounds`, `getClusterOffset`,
  `isClusterStrict`, `getClusterPlacement` (root bel + relative offsets → concrete
  placement; used to move macros/chains atomically).
- **Resources**: `getResourceKeyForPip`/`getResourceValueForPip`/`isGroupResource` —
  shared resource accounting for router2 (e.g. global nets).
- **Flow**: `pack()`, `place()`, `route()`, `assignArchInfo()` (arch data ↔ cell
  attrs roundtrip, used by the JSON write path).
- **Static lists**: `defaultPlacer`/`availablePlacers` (`sa`, `heap`, `static`),
  `defaultRouter`/`availableRouters` (`router1`, `router2`).

`archdefs.h` per family defines `BelId`/`WireId`/`PipId` (opaque ints for classic
arches; `{tile, index}` structs for himbaechel), `GroupId`, `DecalId`, `delay_t`,
`BelBucketId`, `ClusterId`, and the `ArchCellInfo`/`ArchNetInfo` payloads.

---

## 3. Chip Database Models

| Family | Model | DB source |
| --- | --- | --- |
| `generic` | In-memory `std::vector<WireInfo/PipInfo/BelInfo>` + name maps + `bels_by_tile` | Built at runtime via C++ API (`addBel/addWire/addPip`); no binary db |
| `ice40` | Packed POD structs (`ChipInfoPOD`: bel/wire/pip slices, tile grid, switch boxes, config bits, packages, cell timing) referenced via `RelPtr`/`RelSlice` (32-bit relative offsets) | `chipdb.py` (Python, from icestorm data) → `bba` → C array |
| `ecp5`, `machxo2` | POD chipdb (bel/wire/pip/tile slices + timing), same relptr scheme | prjtrellis database via python import |
| `nexus` | POD `DatabasePOD` with per-chip `ChipInfoPOD`, neighbourhood (`LocNeighourhoodPOD`) for near-constant-time locality queries | prjoxide database |
| `mistral` | New arch, separate dbgen flow | — |
| `himbaechel` | Tile-instance / tile-type / shape / node model: wires and pips are `{tile, index}` into per-tile-type data; repeated tile shapes share one description; `RelNodeRefPOD` encodes relative node references | `himbaechel_dbgen` + per-uarch generators (xilinx DB from vendor data) |

Key structural facts:

- ice40 wires carry `segments` (x/y/seg index), `pips_uphill/downhill` slices,
  `bel_pins`, `fast_delay`/`slow_delay`, and a wire `type` (14 types incl. SP4/SP12
  routing tracks, GLB network, LUTFF locals, carry-in mux). Pips have
  `fast_delay`/`slow_delay`, switch-box coordinates, `switch_mask`/`switch_index`
  (bitstream mapping), and flags `ROUTETHRU`/`NOCARRY`. Switch config bits are stored
  per switch (`SwitchInfoPOD`), not per pip — one switch = up to 5 config bits at a
  (row,col) — enabling fast bitstream assembly.
- The `RelPtr`/`RelSlice` scheme means the chipdb is a single contiguous, position-
  independent binary blob that can be *memory-mapped and read directly without
  parsing* (himbaechel actually `mmap`s the file). This is the model to copy in
  Haskell: `ByteString`/`ForeignPtr` + offset arithmetic or a binary-format parser
  producing unboxed vectors.

---

## 4. Main Flow (`common/kernel/command.cc`, per-arch `main.cc`)

1. Parse CLI (`boost::program_options`); per-arch chip/package selection.
2. Load design: `--json` (JSON frontend, `frontend/json_frontend.cc`) and/or
   `--run` python scripts; optional `--sdc` constraints.
3. `pack()` unless `--no-pack`/`--pack-only`; hooks: `pre-pack` python script
   (`--pre-pack`).
4. `ctx->check()` + utilisation print.
5. `place()` unless `--no-place`; hooks `pre-place`; optional `--placed-svg`.
6. `route()` unless `--no-route`; hooks `pre-route`; optional `--routed-svg`,
   `--write-sdf`.
7. Output: `--asc` (ice40), `--textcfg` (ecp5/machxo2/nexus), `--fasm` (himbaechel),
   `--json` (post-PnR JSON via `assignArchInfo` → attrs), `--report` JSON timing
   report. Final `timing_analysis` print (Fmax, critical paths, slack histogram).
8. `ctx->checksum()` printed always — the determinism oracle used by tests.

Python integration: context objects are exported (`python_export_global("ctx", *ctx)`),
letting scripts inspect/mutate the design pre/post phases.

---

## 5. Algorithm Inventory

### 5.1 Packing (per family, `pack.cc`)

Transforms the technology-mapped netlist into arch primitives (LUT-FF slices,
carry chains, IO buffers, RAM/DSP macros). Pass pipelines:

- **ice40** (`Arch::pack`): `pack_constants` → `pack_io` (IO buffers, gbuf insertion)
  → `pack_lut_lutffs` (LUT + attached DFF → `ICESTORM_LC`; uses `net_only_drives`
  fanout analysis) → `pack_nonlut_ffs` → `pack_carries` (carry chain stitching,
  `merge_carry_luts`) → `pack_ram` (SB_RAM40 → tile-sized) → `pack_special`
  (PLL/DSP/IOB/global buffer placement: greedy bel search `cell_place_unique`) →
  `pack_plls`. Globals: `insert_global`/`promote_globals` assign nets to global
  buffers by fanout/type.
- **ecp5**: `prepack_checks` → `pack_io` → `pack_dqsbuf` → `pack_iologic` →
  `pack_eclk` → `pack_ebr` (BRAM) → `pack_dsps` → `pack_dcus` (serdes) →
  `pack_misc` → `pack_constants` → `pack_dram` → `pack_carries` → `pack_luts`
  (LUT5/LUT4 + FF merging, `pack_lut5xs`) → `pack_ffs`. Clusters (EBR/DSP/carry)
  are recorded so the placer moves them atomically.
- **generic**: LUT + DFF → `GENERIC_SLICE` (`pack_lut_lutffs`, `pack_nonlut_ffs`),
  constants folded; `uarch->pack()` for viaduct uarchs.
- **himbaechel uarchs** (e.g. gatemate, xilinx): per-uarch packers (CLB packing,
  DSP/RAM inference, IOB handling).

Packing is netlist-local graph rewriting: order-dependent but per-cell independent
enough to parallelize by cell type pass (the passes are sequential because later
passes consume earlier outputs).

### 5.2 Placement engines (`common/place/`)

#### 5.2.1 `placer1` — simulated annealing (`placer1.cc`)

- **Cost**: HPWL per net (bounding box; incremental update via `NetBB` with per-edge
  cell counts `nx0/nx1/ny0/ny1`), weighted by `hpwl_scale_x/y`; plus net-share bonus
  (`netShareWeight`), constraint distance (`constraintWeight`), and timing cost
  (criticality-weighted predicted delay for high-fanout nets above
  `timingFanoutThresh`).
- **Moves**: random bel per cell (`random_bel_for_cell`), swap pairs, and whole-chain
  moves (`try_swap_chain`) for carry chains. Macro children placed via cluster
  machinery.
- **Schedule**: temperature from `startTemp`, 15 sweeps/iteration, accept/reject by
  Boltzmann probability; `placer1_refine` = same engine at low fixed temperature
  (used after HeAP).
- **Data structures**: net bounding boxes + per-arc timing costs cached in parallel
  arrays indexed by net `udata`; incremental recompute on moves.

#### 5.2.2 `placer_heap` — HeAP/SimPL analytical placer (`placer_heap.cc`)

The default for ice40/ecp5/machxo2/nexus/generic-with-IOBs.

- **Analytical solve**: builds a sparse linear system (connectivity matrix `A`,
  RHS `b`) per axis (X, Y separately) whose least-squares solution minimizes
  quadratic wirelength; solved with Eigen conjugate gradient with warm-start from
  previous iterate (`EquationSystem<T>`: sparse column-major; `add_coeff` keeps
  rows sorted). X and Y systems are solved on **two threads** in parallel.
- **Seeding**: random initial placement (`seed_placement`) using bel buckets;
  IO cells and BEL-constrained cells locked as anchors.
- **Main loop** (until `stalled ≥ 5` or `solved_hpwl ≤ 0.8·legal_hpwl`):
  1. per bucket-run (single bel-bucket runs for LUT/FF/etc, then all-buckets run;
     single-type runs skipped when ≥98% one type or `placeAllAtOnce`),
  2. solve X/Y, 3. `CutSpreader` per cell-group (cell types placed together, e.g.
     slice components) — the SimPL cut-based spreading: recursively split
     overutilized regions, move cells across the cut by linear interpolation,
     enforce region constraints and macro clearances; 4. strict legalisation
     (greedy largest-macro-first: priority queue by chain size × legalisation
     weight; each cell searches radius-increasing neighbourhood via
     `FastBels`/`bels_by_tile`, ripping up conflicting cells at radius 2, chains
     last).
- **Control sets**: `ControlSetState` tracks FF clock/enable/reset sharing; legaliser
  prefers compatible control-set neighbours (`ff_control_set_groups`, radius
  schedule) — a pure QoR/perf optimization.
- **Timing**: after each iteration, `TimingAnalyser.run()` recomputes criticalities;
  next solve weights timing-critical arcs by `criticality^exponent · timingWeight`.
- **Chains**: `update_all_chains` re-anchors chain children to the chain root so the
  solver only places roots.
- **Refinement handoff**: best solution replayed, then `parallel_refine` (if enabled)
  or `placer1_refine` (SA) cleans up.
- **Metrics/state**: per-cell `{x, y, legal_x, legal_y}` floats; `total_hpwl` from
  float positions.

#### 5.2.3 `parallel_refine` — parallel SA (`parallel_refine.cc` + `detail_place_core.*`)

- Design grid is split into rectangular **partitions** (`PlacePartition`), each
  owned by a thread with its own RNG, its own net index space (only nets touching
  the partition), local `cell2bel` map, and incremental net-bounds + timing-arc
  cost arrays. Threads run SA moves inside their partition concurrently.
- **Move transaction** (`DetailPlacerThreadState`): `add_to_move` →
  `compute_changes_for_cell` (per-axis incremental bound change classification:
  `NO_CHANGE/CELL_MOVED_INWARDS/CELL_MOVED_OUTWARDS/FULL_RECOMPUTE`) →
  `compute_total_change` → `bind_move` (arch API under `shared_timed_mutex`) →
  `check_validity` → `commit_move`/`revert_move`.
- Out-of-partition conflicts are impossible by construction (partitions own their
  cells); the arch API mutex only serializes the actual bel bindings.
- `lambda` blends global (before partition) and local (after) cost estimates;
  `inner_iters` SA sweeps per partition; partitions with < `min_thread_size` cells
  processed single-threaded.

#### 5.2.4 `placer_static` — analytic placer for very large chips (`placer_static.cc`)

Used by ice40/ecp5 `static` mode (and xilinx uarch default).

- Replaces the placer's delay oracle with a static distance model
  (`timing_c + timing_mx·dx + timing_my·dy`) so bels need not exist yet.
- Cell groups with normalized area (`StaticCellGroupCfg`: `cell_area`/`bel_area`,
  `zero_area_cells` for macro aux cells, `spacer_rect`); solves
  density-constrained placement via a custom iterative solver (grid-based density
  spreading, `StaticRect` areas), then legalises.
- Supports `get_cell_area_override` and `predict_delay` callbacks.

#### 5.2.5 `timing_opt` (`timing_opt.cc`)

Post-placement timing-driven local optimization (ice40 `--opt-timing`): recomputes
slack via `TimingAnalyser`, then tries moves for cells on critical endpoints
(swaps within neighbourhood), keeping moves that improve worst slack.

#### 5.2.6 Shared placement machinery (`place_common.cc`, `fast_bels.h`)

- `get_net_metric`/`get_cell_metric`/`get_cell_metric_at_bel` — HPWL/COST metrics.
- `legalise_relative_constraints` — satisfies relative placement (BEL/region)
  constraints before placement.
- `FastBels` — per-bucket bel index for O(1) random bel and radius-search queries
  (used by placer1 and legalisers).

### 5.3 Routing engines (`common/route/`)

#### 5.3.1 `router1` — timing-driven A* (`router1.cc`)

- Decomposes every net into **arcs** (driver wire → each sink wire, incl. multiple
  physical pins per logical sink). Arcs queued in a priority queue keyed by
  `estimateDelay(src,dst) · 100 · criticality` (from STA) — critical arcs first.
- Each arc routed with best-first search: priority queue of wires keyed by
  `delay + penalty + togo − bonus` where `togo` = `estimateDelay(wire, dst)`
  (A* heuristic), `penalty` = ripup congestion score of the wire
  (`wireScores`, exponential backoff `ripup_penalty`), `bonus` = reward for
  reusing wires already owned by the same net (`reuseBonus`). Tie-break by random
  tag. Uses downhill-pip expansion; skips locked wires; `estimatePrecision`
  controls heuristic granularity.
- **Ripup**: on conflict, the conflicting net's arc is ripped (`ripup_wire`,
  `ripup_net` with per-net score `netScores`), re-queued; wire penalty incremented.
  `cleanupReroute`/`fullCleanupReroute` passes re-route nets once all arcs done
  to remove redundant detours.
- Constant nets (`constant_value`) routed specially: any wire with matching
  `getWireConstantValue`.
- **Timing-driven ripup** (`--tmg-ripup`): when queue empties with timing failures,
  re-run STA, rip up sink wires with slack ≤ threshold, repeat (≤50 times).
- Arc queue bookkeeping: `arc_queue` + `queued_arcs` pool (dedupe), `wire_to_arcs`/
  `arc_to_wires` maps for targeted ripup.
- Ends with `checkRoutedDesign()` (every user has a wire, every wire is
  reachable from source) and `timing_analysis`.

#### 5.3.2 `router2` — congestion-driven bidirectional A* (`router2.cc`)

Default for large designs (ecp5, xilinx). Allows overlap during routing.

- **Per-wire state** (`PerWireData`): `curr_cong` (usage count), `hist_cong_cost`
  (historical congestion), `reserved_net`, `x/y` (notional location for thread
  safety), forward/backward visit data (`pip_fwd/bwd`, `cost_fwd/bwd`).
- **Cost model**: wire/pip cost = base cost (delay-derived, `get_base_cost`)
  × (1 + overuse·`curr_cong_weight`·crit) × (1 + crit·(hist − 1)); historical
  congestion decays/reinforces across iterations. `estimate_weight` scales the
  A* heuristic. Optional per-pip **resource** congestion via
  `getResourceKeyForPip` (shared routing resources with value/count accounting).
- **Search**: bidirectional A* — forward from source and backward from sink
  (`fwd_queue`, `bwd_queue`), meeting point recorded; global nets (high-fanout)
  routed backwards-only within their bounding box (`global_backwards_max_iter`).
  Bounding-box pruning (`bb_margin_x/y`); on failure retry without BB
  (`ARC_RETRY_WITHOUT_BB`), then mark net failed and re-queue for next iteration
  with `fail_count` penalties. `ipin_cost_adder` reduces interconnect sharing
  benefit at sink pins; `bias_cost_factor` biases toward net centroid.
- **Iterations**: per iteration, nets are partitioned across
  `ThreadContext`s (thread-local queues/state, per-thread bounding box); each
  thread routes its nets' arcs; after all threads join, historical congestion is
  updated from the current overuse; failed nets re-routed in the next iteration.
  Deterministic via per-thread RNG seeds and stable net→thread assignment.
- **Commit**: when no failures remain, bindings are committed to the arch API
  (overlap allowed until then), then `ctx->check()` + `checkRoutedDesign()` +
  timing analysis. Optional `--router2-heatmap` writes congestion heatmap SVGs.

#### 5.3.3 Route delay queries (`context.cc`, `router1.cc`)

`getActualRouteDelay` (BFS along bound pips from src to dst), `getNetinfoSourceWire`
(driver bel pin wire), `getNetinfoSinkWires`, `getNetinfoRouteDelay(Quad)` — used by
STA and SDF output.

### 5.4 Timing analysis (`common/kernel/timing.cc`, `timing.h`)

Static timing analyser (`TimingAnalyser`) used by placers (criticalities), routers
(ripup decisions), and final reports:

1. `setup`: `init_ports` (all cell ports, arc extraction via
   `getPortTimingClass`/`getCellDelay`/`getPortClockingInfo`, cached as `CellArc`s:
   comb/setup/hold/clk-to-q/startpoint/endpoint), `get_route_delays`,
   `topo_sort` (netlist DFS — cycles detected and broken), domain identification
   (clock net × edge → `domain_id`, async = id 0), `identify_related_domains`.
2. `run`: forward walk (`walk_forward`) propagates arrival times per
   (port, domain) with min/max (`ArrivReqTime` + path backpointers for crit-path
   reconstruction); backward walk propagates required times; `compute_slack`,
   `compute_criticality` (per sink port, worst over domain pairs).
3. Reports: `TimingResult` — per-clock Fmax, critical paths (segments typed
   CLK_TO_CLK/CLK_SKEW/CLK_TO_Q/SOURCE/LOGIC/ROUTING/SETUP/HOLD), cross-domain
   paths, detailed net timings, slack histogram, min-delay (hold) violations.

STA is a graph algorithm over the *placed* netlist: cells are combinational
DAGs with per-arc delays, sequential cells are domain boundaries. Parallelizable
in Haskell via the same topological wavefront (or a `Par`-based relaxation).

### 5.5 Bitstream generation

- **ice40** (`ice40/bitstream.cc`): iterates bound pips → switch boxes → config
  bits (`SwitchInfoPOD`); bel params → config entries (`BelConfigPOD`);
  tile entries (`TileInfoPOD`); produces `.asc`. `archcheck` validates.
- **ecp5** (`ecp5/bitstream.cc`): builds `.config` text via trellis-compatible
  config entries, then external `ecppack` → `.bit`.
- **machxo2/nexus**: similar trellis/prjoxide-backed flows.
- **himbaechel**: FASM output (xilinx/gatemate), via `uarch` bitstream writers.
- **bba** (`bba/`): assembles the binary chipdb blobs (relptr-relative format)
  from `chipdb.py` output; emitted as C arrays or raw blobs.

### 5.6 IO formats

- **JSON** (`frontend/json_frontend.cc`): yosys `write_json` netlist — cells,
  ports, nets, attrs, params, hierarchy; written back post-PnR including
  placement/routing info (via `assignArchInfo`).
- **SDC** (`sdc.cc`): clock constraints. **SDF** (`sdf.cc`): timing back-annotate.
  **SVG** (`svg.cc`): placement/routing visualizations. **Report** (`report.cc`):
  JSON timing report. **PCF/LPF** (ice40/ecp5): pin constraints.
- **Python API** (`python/`, `arch_pybindings.cc`): full netlist + context
  manipulation; script hooks `pre-pack`/`pre-place`/`pre-route`/`post-route`.

---

## 6. Test Suite Inventory and Gap Analysis

### 6.1 What exists

**A. C++ unit tests (gtest, `-DBUILD_TESTS=on`), main repo + submodule:**

| Target | Files | Coverage |
| --- | --- | --- |
| `nextpnr-ice40-test` | `ice40/tests/{hx1k,hx8k,lp1k,lp384,lp8k,up5k,main}.cc` | Per chip: bel/wire/pip name roundtrip + counts, uphill↔downhill pip consistency, (main.cc: gtest runner) |
| `nextpnr-himbaechel-test` | `himbaechel/test_main.cc` + `uarch/gatemate/tests/{lut,testing}.cc`, `uarch/ng-ultra/tests/lut_dff.cc` | Uarch chipdb sanity: bel/wire/pip iteration, LUT/DFF bel discovery, bucket checks |
| `nextpnr-*-test` GUI | `tests/gui/quadtree.cc` (submodule) | QuadTree insert/query/bounds |
| `generic` | `tests/generic/main.cc` (submodule) | **Empty gtest runner — no tests wired in `generic/CMakeLists.txt`** |
| `ecp5`, `machxo2`, `nexus`, `mistral` | — | **No unit tests at all** |

**B. Flow/regression tests (submodule `nextpnr-tests`, Makefile-driven):**

- `ice40/regressions/` — 25 dirs (`issue0065`…`issue0258`, `pr0226`, `pr0252`):
  gzipped yosys JSON + optional PCF/YS/scripts; Makefile runs `nextpnr-ice40
  --json … --asc …`, then `icebox_vlog` sanity-check; `WAIVE` files mark known
  failures; `*.sh` variants for multi-step flows. Run in CI for ice40.
- `ecp5/regressions/` — 4 dirs (`issue0191/0194/0235/0259`): same pattern with
  `--textcfg` + `ecppack`.
- `generic/flow/` — 1 test (`bel-pin`): yosys prep → `nextpnr-generic --json
  --pre-pack pre_pack.py --post-route post_route.py --no-iobs`; verifies python
  hooks and bel-pin constraints.
- `ice40/smoketest/attosoc/` — full SoC: synth → PnR → icetime timing → iverilog
  simulation vs golden output (`diff output.txt golden.txt`).
- `ice40/pack_tests/`, `ice40/carry_tests/` — synthesis+miter SAT equivalence
  check of packed output vs golden netlist.

**C. Arch self-check (`--test` flag, `archcheck.cc`):** CI runs
`nextpnr-ice40 --hx8k --test`, `--up5k --test`, `nextpnr-ecp5 --um5g-25k --test`,
`nextpnr-generic --uarch example --test`. Checks: names, locations, connectivity
(downhill/uphill symmetry, dead-end wires, pip src/dst), buckets, bel/pin sanity.

**D. Other**: `machxo2/examples/*test.sh`, `generic/examples/simtest.sh`.

### 6.2 Gaps

| # | Gap | Severity | Why it matters for the Haskell port |
| --- | --- | --- | --- |
| G1 | **Generic arch has zero unit tests** (`main.cc` runner exists, no `TEST_SOURCES` in `generic/CMakeLists.txt`) | High | Generic is the reference model for the port; no golden assertions on its chipdb/packer/timing |
| G2 | **No unit tests for any algorithm**: router1/router2, placer1/heap, TimingAnalyser, packers, design_utils, idstring, Property, hashlib dict semantics, DeterministicRNG | High | Port validation needs bit-exact oracles per algorithm, not just end-to-end regressions |
| G3 | **ecp5/machxo2/nexus/mistral: no unit tests**; ecp5 only 4 regressions | Medium | ECP5 has the most complex packer (17 passes); regression-only coverage is coarse |
| G4 | **No regression coverage for machxo2, nexus, himbaechel (gatemate/gowin/ng-ultra/xilinx), mistral** | Medium | CI only tests ice40/ecp5/generic regressions |
| G5 | **No determinism tests** (same input+seed → identical checksum; X/Y-thread and router2 MT determinism) | High | The port's #1 correctness property (per §1) is untested |
| G6 | **No negative-path tests**: unroutable designs, invalid constraints (bad BEL/pin/region), failed placement; WAIVE mechanism untested | Medium | Port must replicate error behavior |
| G7 | **No JSON roundtrip test** (write post-PnR JSON → re-read → same checksum) | Medium | JSON is the port's primary IO |
| G8 | **No packer conformance tests for generic** (LUT-FF packing, constant folding, slice counts) | Medium | Packing is the least parallel-friendly stage; needs goldens |
| G9 | **No timing-analysis unit tests** (synthetic netlists with known slack/criticality) | High | STA correctness is a prerequisite for timing-driven PnR parity |
| G10 | **No scale/performance tests** (large designs, router2 heatmaps, placer-static) | Low | Perf regression detection for the port |
| G11 | **archcheck coverage uneven**: only 2 ice40 chips, 1 ecp5 part, generic example uarch | Low | Chipdb port bugs surface as archcheck failures |
| G12 | **No Python API tests** in CI | Low | Hooks (`pre-pack` etc.) only covered by 1 generic test |

### 6.3 Gap completion (executed results)

Executable with available toolchain (g++/cmake/eigen/boost, yosys, no icestorm/trellis).

**Done — `generic/tests/` gtest suite in the main repo (37 tests, all passing, deterministic across repeated runs):**

| File | Tests | Covers |
| --- | --- | --- |
| `chip.h` | fixture | 5×5 synthetic chip (34 bels, 390 wires, 1436 pips) built via the runtime arch API; delayScale/delayOffset set so the route-delay estimate is consistent with pip costs; ripup penalty on delay scale |
| `chipdb.cc` | 9 | G1: bel/wire/pip name roundtrips + counts, uphill/downhill consistency, locations, buckets, pin wires, grid dims, chipdb checksum reproducibility |
| `kernel.cc` | 6 | G2: idstring interning, printf-id, Property semantics, RNG golden sequence (xorshift64*, 5 known values), RNG bounds/reproducibility/seed, settings |
| `pack.cc` | 6 | G8: LUT→SLICE, LUT+DFF merge, constant folding (GND/VCC), non-LUT FF, pack idempotence, empty design |
| `timing.cc` | 4 | G9: setup slack/Fmax/criticality on synthetic FF→LUT→FF paths, setup violation, criticality ordering, no-clock design |
| `place.cc` | 3 | G5: placer1 legal placement, determinism (same seed → same checksum), placement sanity |
| `route.cc` | 4 | G5/G6: router2 full flow + routed-design consistency, router2 determinism, router1 single multi-sink net, undriven net (router1) |
| `json.cc` | 3 | G7: JSON write→read fixed point (byte-stable from cycle 2), structure preservation, malformed input throws |

Wired via `TEST_SOURCES` in `generic/CMakeLists.txt` (was missing entirely — the wiring itself was a gap). Build: `cmake -DARCH=generic -DBUILD_TESTS=on -DBUILD_PYTHON=off -DBUILD_GUI=off ..` then `make nextpnr-generic-test`.

**Upstream bugs found & fixed while writing the tests (latent UB, determinism-critical):**

1. `generic/arch.cc` — `gridDimX/gridDimY` uninitialized; `addBel/addWire` grow them via `std::max` on indeterminate values → per-context garbage grid dims → placer `rng(n)` draw counts differ → nondeterministic placement. Fixed: init to 0 in the `Arch` ctor.
2. `generic/archdefs.h` — `ArchCellInfo::user_group/is_slice/slice_clk/flat_index` uninitialized; `cellsCompatible` (called by `isBelLocationValid`) reads them → random placement rejections. Fixed: default member initializers (`-1/false/nullptr/-1`).

**Algorithmic findings (valuable for the Haskell port):**

1. **router1 can livelock on congested synthetic graphs.** With several nets whose only cheap paths cross each other's gateway wires (a circular wire-dependency ring), ripup/reroute cycles forever: each net re-routes into the freshly-freed wire, `wireScores` escalate but are only consulted while the wire is *occupied at expansion time* — in the alternating window both nets always see it free. Real chips avoid this via graph scale and routing slack; router2 exists for congested cases. Port note: replicate this exact escalation semantics (it is observable and part of behavior), and do not "fix" the livelock without diverging from C++ output.
2. **router1's A* pruning assumes the delay estimate is within ~2× of real path cost** (`est/2 - precision > best_est` kills nodes). The generic arch's default `delayScale=0.1, delayOffset=0` is only usable with pip delays of the same order; the test chip uses `delayScale=40, delayOffset=20` with pip delays 5–10ns (offset ≥ max intra-tile path). Port note: the heuristic-consistency invariant must be documented per arch.
3. **`delay_t` float + epsilon-based visited updates** in router1's A*: a wire's stored score can only be beaten by a strictly-better score; with float delays and no pruning this degrades, which is why arches use int `delay_t` (ice40/ecp5) — the Haskell port should keep `delay_t` integral for classic arches.
4. **Setup/settings invariants discovered by tests**: `timing_driven`, `target_freq`, `placer1/*`, `router1/*` settings must exist before engines run (the CLI sets them; tests must too). `getBelPinsForCellPin` is keyed *per cell instance* in the generic arch (populated via `addCellBelPinMapping`) — the timing analyser and placers call it for every placed cell, so uarchs/tests must register pin maps.
5. **JSON roundtrip is a fixed point from the second write→read cycle** (the first cycle interns `$frontend$*` nets for unconnected ports and the `synth` setting, shifting idstring indices). Idstring indices are part of the checksum, so raw checksum equality across JSON generations is not a valid invariant; document-order stability is.

**Deferred (needs icestorm/trellis or a second repo):** ecp5/machxo2/nexus unit test skeletons (G3), regression additions in the `nextpnr-tests` submodule (G4), generic flow tests runnable via the submodule Makefiles (yosys present; needs the built `nextpnr-generic` binary — next step), and a `run_determinism.sh` wrapper. The 37 gtest cases cover G1/G2/G5/G6/G7/G8/G9 for the generic arch; ice40 keeps its chipdb roundtrip tests; ecp5/nexus/machxo2 remain regression-only until the submodule flows are run.

---

## 7. Haskell Port Design

Goal: bit-compatible PnR semantics (same results modulo intended algorithmic
improvements), maximum parallelism, high performance. Strategy: mirror the C++
architecture 1:1 at the module level, keep the *semantic* model identical, and
exploit Haskell's purity/parallelism where the C++ code already has thread
safety or where moves are independent.

### 7.1 Package layout

```
nextpnr-haskell/
  nextpnr.cabal / cabal.project
  src/Nextpnr/
    Kernel/            -- mirror of common/kernel
      IdString.hs      -- interned ids; IORef table; IdStringList
      Property.hs      -- variant values, JSON codec
      Delay.hs         -- DelayPair/DelayQuad closed Num algebra
      Netlist.hs       -- Cell, Net, Port, PortRef, hierarchy, aliases
      Arch.hs          -- Arch class (typeclass mirror of ArchAPI)
      BaseArch.hs      -- default implementations + binding maps
      Context.hs       -- mutable Context (IORef/STM), settings, checksum
      DeterministicRng.hs -- xorshift64* + sortedShuffle
      HashTable.hs     -- deterministic-iteration hash table (IntMap-based)
      Timing.hs        -- STA engine
      ArchCheck.hs     -- archcheck equivalent
      Json.hs Sdc.hs Sdf.hs Report.hs
    Place/
      Metrics.hs       -- HPWL/NetBB incremental
      Placer1.hs       -- SA (pure step function)
      PlacerHeap.hs    -- analytical solve (pure; hmatrix/linear solvers)
      ParallelRefine.hs-- partition-based parallel SA
      PlacerStatic.hs  -- static analytic
      TimingOpt.hs
      Legalise.hs      -- shared legaliser/constraints
    Route/
      Router1.hs       -- A* per-arc
      Router2.hs       -- bidirectional A*, congestion
      RouterCommon.hs
    Arch/
      Generic/         -- first port target (flat arrays)
      Ice40/ Ecp5/ Nexus/ Machxo2/ Himbaechel/  -- chipdb loaders (binary format)
    Flow.hs            -- pack/place/route orchestration + CLI
    Bitstream/         -- per-arch writers
  test/                -- Haskell conformance tests (golden vs C++ outputs)
```

### 7.2 Data model decisions

- **IDs**: `newtype BelId = BelId {unBel :: Int}` etc. with `PatternSynonyms` and
  `Unbox` instances; `IdString = IdString {unId :: Int}` with an intern table
  (`IORef (Map String Int, Vector String)`) — O(1) hashed lookup, cache-safe.
  Do **not** carry the string around (C++ doesn't; memory locality matters).
- **Netlist**: persistent (immutable) `Cell`/`Net` records as the *canonical*
  representation, with mutation expressed through a `Design` state (`Data.Map`
  or `HashMap` keyed by IdString) threaded explicitly or via `State`/`ST`.
  Rationale: packers/placers are whole-design transformations; pure functions
  `pack :: Arch a => Design a -> Design a` are trivially parallelisable at pass
  boundaries and testable.
- **Chipdb**: parse the relptr blobs once into unboxed `Data.Vector` arrays
  (`BelInfo`, `WireInfo` (downhill/uphill pip ranges as index pairs — copy the
  C++ slice layout), `PipInfo`). All traversal = vector index arithmetic.
  Alternatively `mmap` + offset reads via `ForeignPtr` to avoid parse cost.
- **Binding maps**: `bel2cell :: Vector (Maybe CellId)`, `wire2net :: Vector
  (Maybe NetId)`, `pip2net` — unboxed `MVector` in `ST` during routing/placement
  mutation, frozen between phases. This mirrors `base_bel2cell` exactly.
- **Delay model**: `delay_t` as `Int` for classic arches (bit-exact integer
  arithmetic — *mandatory* for checksum parity) with the same `DelayPair`/
  `DelayQuad` algebra.

### 7.3 Parallelism map

| Stage | C++ parallelism today | Haskell opportunity |
| --- | --- | --- |
| Packing | none | Pass-level `parBuffer`/pipeline; within-pass per-cell independence (LUT-FF merge, carry stitching are local rewrites) — use `Control.Parallel.Strategies` or `Par`; merge phases sequential |
| HeAP solve | 2 threads (X/Y) | 2-way `par` is trivial; **conjugate gradient itself is parallelizable** (sparse matvec + dot products via `repa`/`accelerate` or `hmatrix` + `Control.Parallel`); or use a pure Haskell CG over unboxed vectors |
| CutSpreader | sequential | Region recursion is embarrassingly parallel (independent subregions) — `Par` tree parallelism |
| Strict legaliser | sequential | Cells are independent except ripup conflicts; partition by tile region with conflict retry (as `parallel_refine` does) |
| `parallel_refine` | pthread partitions | Direct fit: partition cells → `parMap` over partitions with pure move-apply functions; no locks needed if bindings are ST-local and committed after join (C++ needs mutexes; Haskell doesn't!) |
| Router1 | none | Arc routing is independent *except* conflicts; parallelize via `Par` with shared penalty state in `MVar`/STM, or partition arcs by region like router2; deterministic via ordered commits |
| Router2 | thread contexts | Pure bidirectional A* per net-arc (pure function of (graph, congestion state)) → `parMap` over nets; congestion update phase is a vector fold (data parallel); historical costs updated between iterations (sequential barrier, cheap) |
| STA | sequential | Wavefront relaxation is embarrassingly parallel per level; use `vector` + `parMap` over ports; or `Data.Graph` topo order + `parBuffer` |
| Bitstream | sequential | Pure map over pips → config bits; `parMap` trivial |
| Checksum | sequential | `fold` over deterministic traversal; parallel `parFold` possible since commutative-ish, but keep sequential for exactness (cheap) |

**Determinism strategy**: parallel phases must reduce deterministically. The C++
code achieves determinism with seeded per-thread RNGs + ordered joins; Haskell
reproduces this exactly with `parListChunk` over a *deterministic partition* and
sequential merge, or `Par` with `IVar`s joined in index order. Never use
`parMap` with pure `random` (use the xorshift state threaded per partition).

### 7.4 Performance strategy

1. **Unboxed vectors everywhere**: chipdb, binding maps, net/arc arrays —
   `Data.Vector.Unboxed`/`Data.Array.Unboxed`; index-based IDs make this clean.
2. **Avoid laziness leaks**: strict fields (`BangPatterns`, `StrictData`) in all
   hot records; the C++ memory model (flat arrays, no per-object allocation
   churn) maps to unboxed vectors, not to `[a]`/boxed maps.
3. **Heap allocation in routers**: the A* priority queue is the hot spot —
   use `Data.Heap`/`PSQueue` (or a hand-rolled binary heap over `STUArray`)
   with the same key ordering incl. randtag tie-break.
4. **IdString**: `Int` newtype + one global intern table; never store strings in
   hot paths (C++ stores `const std::string*` and compares ints — same idea).
5. **STA caching**: mirror the C++ `CellArc` cache (per-cell-port arc lists
   computed once) — avoids repeated arch calls; cache as vectors.
6. **Memory**: mmap chipdb (himbaechel-style) or parse once to unboxed vectors;
   GC pressure controlled via `ForeignPtr`/`ByteString` for blobs.
7. **Solver**: for HeAP, options — (a) pure Haskell CG (`hmatrix` uses LAPACK,
   not parallel; a hand-rolled CG over `Data.Vector` parallelizes with
   `Control.Parallel` on matvec), (b) FFI to Eigen for bit-exact parity. Start
   with (a) but validate residuals; keep `EquationSystem` sparse column format
   identical.
8. **Tuning knobs** must read from a `settings` dict exactly like C++ (`setting<T>`
   with same defaults) so CLI parity is preserved.

### 7.5 Conformance strategy

1. **Golden tests**: run the C++ binary on every test-suite case (regressions +
   new unit tests), capture `checksum`, Fmax, wirelen, and (ice40) `.asc`
   bitstreams as goldens. Haskell port must reproduce: same checksum for same
   seed, same placement/routing decisions (deterministic algorithms), same
   bitstream bits.
2. **Order of porting**: `Kernel` (IdString, Property, delays, netlist) →
   `Arch.Generic` + chipdb loader → `Placer1` (simplest engine) → `Router1` →
   `Timing` → packer(generic) → flow/CLI → end-to-end generic tests → then
   HeAP/router2/parallel_refine → then real chipdbs (ice40 first — smallest,
   best-documented db) → bitstream.
3. **Randomized differential testing**: property-based tests (QuickCheck) that
   generate random netlists, run both implementations (Haskell + C++ via CLI or
   captured goldens), compare checksums and routed structures.
4. **archcheck port**: implement `archcheck` in Haskell (names, connectivity
   symmetry, buckets) — it is the cheapest full-chipdb validator.
5. Every C++ unit test in §6 gets a Haskell twin in `test/`; regressions stay
   runnable against the Haskell binary via the same Makefiles (drop-in
   `NPNR=nextpnr-haskell-generic`).

### 7.6 Risk register

- **Bit-exactness vs improvement**: some C++ behavior is emergent (SA acceptance
  order, hash iteration order — `hashlib` dicts have *deterministic but
  hash-dependent* iteration). The port must either replicate `hashlib`'s
  `mkhash`/open-addressing order or adopt the C++ outputs as soft goldens
  (checksum equality for same-input deterministic flows). Prefer replicating
  `mkhash` exactly — it is simple (see `common/kernel/hashlib.h`).
- **Floating point**: generic/ice40/ecp5 use integer `delay_t` (safe); himbaechel
  xilinx uses `double` — CG solver tolerance (`solverTolerance`) and SA
  probabilities must match bit-for-bit only if goldens demand it; otherwise
  tolerance-based comparison.
- **Eigen parity**: CG with warm start is order-sensitive; if bit-exact HeAP
  output is required, FFI to Eigen; otherwise accept small placement deltas and
  validate QoR (wirelen/Fmax) statistically.
- **Threading determinism of router2**: net→thread partition must be stable
  (sort nets by name/udata before chunking — same as C++'s ordered assignment).

---

## 8. Appendix: Key Constants and Defaults (for parity)

| Setting | Default |
| --- | --- |
| `placer` / `router` | ice40/ecp5/generic: `heap` / `router1` (generic `arch.cc:752-756`) |
| `router1/maxIterCnt` | 10000 |
| `router1/wireRipupPenalty`, `netRipupPenalty`, `reuseBonus`, `estimatePrecision` | 1.0 / 2.0 / 0.2 / 2.0 |
| `placer-heap-alpha/beta/critexp/timingweight` | 0.1 / 0.9 / 2 / 10 |
| `router2/initCurrCongWeight/histCongWeight/currCongWeightMult/estimateWeight` | 0.5 / 1.0 / 2.0 / 1.25 (alt: 5/0.5/0/1) |
| RNG | xorshift64*, seed 0x3141592653589793 |
| `timing_driven` | true |
| `parallel_refine` threads | `threads` setting (default hardware concurrency) |

*Verify exact defaults from `Placer1Cfg/PlacerHeapCfg/Router1Cfg/Router2Cfg` constructors when porting each module.*
