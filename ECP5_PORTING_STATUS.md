# ECP5 Port Status (lambdapnr)

Bit-compatible Haskell reimplementation of nextpnr-ECP5. Goal: byte-for-byte identical
`.config` output and identical post-pack design checksum (`Context::checksum`) on the
rioctrl reference design, pass by pass.

## Reference setup (oracles)

- **Source of truth for semantics**: pinned nextpnr submodule (51e703d0) + trellis store.
- **Source of truth for numbers**: the **nextpnr-0.10-tag oracle worktree** at
  `nextpnr-0.10` (HEAD 84856bd6), rebuilt in **`/devel/HDL/lambdapnr/oracle-build/`**
  (a copy — the original `/devel/HDL/npnr-oracle` tree went READ-ONLY after a machine
  reboot that remounted all filesystems `errors=remount-ro`; only the workspace is
  writable). The nix-store binary referenced by older notes
  (`/nix/store/zhalr5ayq…-nextpnr-ecp5`) no longer exists in the store. Patched with:
  - `LPCHK <pass>:` checksum logs after every pack pass
  - `LPDBG intern` id-table trace (interner order oracle; logs indices >= 1970)
  - `LPDBG cell/net/cellport/cellattr/cellparam` state dumps to
    `/tmp/cpp_state_<pass>.txt` — **warning: every instrumented C++ run OVERWRITES
    these files**; regenerate the lineage you need before diffing.
  - Placer dumps in `common/place/placer_heap.cc`: `/tmp/cpp_solve_dump.txt` +
    `/tmp/cpp_spread_dump.txt` (gated `iter==4`, i.e. the 5th main iteration),
    `/tmp/cpp_regions_log.txt` (spreader run headers, regions, per-cut lines:
    region/dir/ncells/total, pivot/cl/cr, trimmed/bestcut, `sorted20` raw-key
    dumps, per-bin bl/br/origL/origR/m for cut r1), `/tmp/cpp_occ_dump.txt` +
    `/tmp/cpp_exp_dump.txt` (gated `buckets.size()==3`), `/tmp/cpp_crit.txt`
    + crit-port/port dumps (`iter==1`), `/tmp/cpp_rng.txt`,
    `/tmp/cpp_cell_probe.txt`/`/tmp/cpp_bel_probe.txt` (basesoc_uart_core_tx2…
    cell), `/tmp/cpp_targets.txt`, `/tmp/cpp_legal_dump.txt`, and
    `/tmp/cpp_flags.txt`/`/tmp/cpp_valid.txt` (tile 24,24 via
    `ecp5/arch_place.cc` `isBelLocationValid` probes). See the
    "Placer (heap) — main-loop state" section for the full inventory.
  - **placer1 SA-refine dumps** in `common/place/placer1.cc`:
    `/tmp/cpp_p1_tot.txt`, `/tmp/cpp_p1_iter.txt` (IT lines incl. rngstate hex),
    `/tmp/cpp_p1_moves.txt`, `/tmp/cpp_p1_move_dump.txt`, `/tmp/cpp_p1_rbfc.txt`,
    `/tmp/cpp_p1_auto.txt`, `/tmp/cpp_p1_chain.txt`, `/tmp/cpp_p1_other.txt`,
    `/tmp/cpp_p1_chain_move.txt`, `/tmp/cpp_p1_plac17.txt`, `/tmp/cpp_crit17.txt`; plus a
    `timing.h` dump extension. All additive-only — every rebuild+run still prints
    `Checksum: 0xf1975059` (lineage A post-place).
  - **Post-heap / post-place state + per-entity checksum dumps**:
    `/tmp/cpp_postheap_state.txt` (placer_heap.cc, before placer1 handoff),
    `/tmp/cpp_postplace_state.txt` + `/tmp/cpp_postplace_ck.txt` (placer1_refine,
    after `place(true)` — `C <nameidx> <cksum>` / `N <nameidx> <cksum>` lines),
    `/tmp/cpp_p1_netbounds.txt` (per-net IGNORED/KEPT flags), and
    `/tmp/cpp_idtail_pack.txt` (id-table tail 43940.. at pack end).
  - Rebuild: `cd /devel/HDL/lambdapnr/oracle-build/build && cmake --build . -j8`;
    configure: `cmake . -DARCH=ecp5 -DTRELLIS_INSTALL_PREFIX=/nix/store/4vnsrfw7x6i09k6hmfp0qndlq0bdj7r0-trellis-unstable-2025-01-30
    -DBUILD_GUI=OFF -DBUILD_PYTHON=OFF -DBOOST_ROOT=/devel/HDL/boost191
    -DBOOST_LIBRARYDIR=/devel/HDL/boost191 -DBOOST_INCLUDEDIR=/usr/include`.
  - Run instrumented binary with `LD_LIBRARY_PATH=/devel/HDL/boost191`
    (symlinks to `/usr/lib/libboost_*.so.1.91.0`). Oracle run (~30s):
    `./oracle-build/build/nextpnr-ecp5 --json test/ecp5/rioctrl/reference/rioctrl_controller.json
    --textcfg /tmp/cpp_full.config --12k --package CABGA256 --speed 6 --seed 1
    > debug-oracle/cpp_full_nolpf.log 2>&1` (no `--lpf` = lineage A anchors).
  - `/tmp` is wiped on reboot — copy dumps into `debug-oracle/`/`debug-hs/`
    (gitignored) immediately after each run; the workspace is the only
    reboot-persistent location on this box.
- **Two oracle lineages** (same binary; the difference is `--lpf`):
  - **Lineage A = run WITHOUT `--lpf`** — the current-oracle table (after-load
    0x14bc76b5 … ffs 0xe4e9248d … globals/fixup 0xa0768a93; post-place 0xf1975059;
    post-route 0x94a9ffe1). lambdapnr matches every pass + post-place (0 per-entity
    diffs). The older 0x1ccd3d41/0x19206350/0xfef78839 values were from the old
    /tmp-era binary and are stale.
  - **Lineage B = run WITH `--lpf` (the reference build command)** — release-equivalent:
    its final `.config` matches `test/ecp5/rioctrl/reference/rioctrl_controller.textcfg`
    byte-for-byte. Current-oracle targets: after-load 0xad3760dd, io 0x89a6cf21,
    plls 0xe3c91e66, ffs 0x4c2144fe, constraints 0x4c2144fe, **globals/fixup
    0x889a4909**, post-place 0x519b603f, post-route 0x728f80cc. The old
    0x8bc4fbe8/0xd1db7350/0xc76929e2 values are the old binary's; REFERENCE.md's
    golden 0xc76929e2 needs updating at the final gate (the 0.10-tag oracle
    reproduces the reference textcfg byte-for-byte with 0x889a4909).
- The submodule (51e703d0) is **newer** than 0.10 and gives different checksums —
  all reference artifacts (textcfg, goldens) are 0.10-era, so pack order/pass set
  mirror **0.10's `Ecp5Packer::pack()`** (io → dqsbuf → plls → iologic → ebr → dsps →
  dcus → misc → constants → dram → carries → luts → lut5xs → ffs → constraints →
  globals; **no pack_eclk**).
- The C++ oracle now holds the placer dumps PLUS the restored pack-stage
  `LPCHK <pass>:` instrumentation (the pack-stage debug patches had been dropped
  when /tmp wiped the old worktree; they were re-added this session in
  `oracle-build/ecp5/pack.cc`, additive-only). The old full-patch build recipe
  above was /tmp-era; the current one is the cmake line in the previous bullet.
- Haskell side: `LAMBDAPNR_PACK_STOP=<pass>` stops packing; `LP_DUMP_ORDER=1` dumps
  iteration order to `/tmp/lp_order_<pass>.txt`; `app/Main.hs` stateDump writes the
  C++ LPDBG format (in Map order — NOT iteration order; use LP_DUMP_ORDER for order).
  `/tmp/dump_state.py a b` diffs two state logs (name-based, index-independent).
  Placer1 debug aids: `LP_P1_DUMP=1` mirrors the placer1.cc dumps to `/tmp/hs_p1_*`,
  and `LP_PLACER1_SAVE`/`LP_PLACER1_RESUME` provide the `place1Refine` save/resume
  scaffolding (wired in `app/Main.hs`); `LP_PLACE_SAVE` dumps the post-place
  cell→bel state and `LP_PLACE_CK` the post-place per-cell/per-net checksums.

## Milestones (done)

| # | Milestone | Commit |
| --- | --- | --- |
| 1 | Kernel: id table, properties, hash tables, delay calculator, deterministic RNG | 098a641 |
| 2 | Chipdb loader + memory-safe parser, arch instance | 592a4ba |
| 3 | Binding state, fanout-aware pip delays, cell timing | 8a0ac2b |
| 4 | All device variants (12k–85k), full CLI option table | a4bb50d |
| 5 | CLI option semantics + archcheck `--test` | 0c5f22e |
| 7 | prjtrellis `.config` writer, base configs, bitgen | a742093 |
| 8 | **Packer: all passes through ffs at parity** (uncommitted) | — |
| 9 | Packer final: LPF parser, globals, golden pack checksum | 2572185 |
| 10 | Placer stage 1: constraints + seed placement at parity | c0b4fb0 |
| 11 | Placer stage 2: equation solve plumbing (Eigen CG FFI) | 82bc84a |
| 12 | Placer stage 3: timing engine + initial iterations at bit-exact parity | 85cd132 |
| 13 | Placer: HeAP main loop (spread/legalise) at bit-exact parity through 20 iterations | 4512822 |
| 14 | Placer: placer1 SA refine — IT1-23 bit-exact, SA breaks at iter 23 exactly like C++; **post-place checksum at parity** (0xf1975059) after porting `fixupHierarchy` interning + `archInfoToAttributes` | 7ef8b5c |

Working tree is dirty: Placer1.hs (**new** — full placer1 SA refine port), PlacerHeap.hs
(isBelLocValidE/dspLocationValid exports + restore unbind fix), Main.hs (place1Refine +
LP_PLACER1_SAVE/RESUME save/resume, LP_PLACE_SAVE/LP_PLACE_CK post-place dumps, forces
the post-place checksum with `seq` before printf, calls `archInfoToAttributes` after the
pack checksum print), DeterministicRng.hs (rngFromState), Pack.hs (sd0Rename uses
`renamePort` instead of `movePort`; packFfs' "erase unconnected M" path now
`swapRemovePort` and updates cellPortOrder; **`fixupHierarchy` now ports the
trim/rebuild local-name interning (was a no-op)**; **new `archInfoToAttributes`
(NEXTPNR_BEL/BEL_STRENGTH/ROUTING attrs, interned in C++ order)**), Netlist.hs
(renameCellPort overwrites instead of appending a duplicate port to cellPortOrder,
matching C++ `dict operator[]`; restored the `removeNetDriver` export; **new
`setNetAttr`/`delCellAttr`**), cabal (exposes the Placer1 module),
ECP5_PORTING_STATUS.md. The placer1 workstream is **un-paused: milestone 14 is done**.

## Packer checksum status (rioctrl full design, 12k)

> **Oracle regeneration note** — the historical per-pass table below was measured by
> the old /tmp-era instrumented binary. That binary is gone; the current oracle is the
> 0.10-tag worktree (84856bd6) rebuilt in `oracle-build/` (the `/devel/HDL/npnr-oracle`
> tree went read-only after a machine reboot), with the pack-stage `LPCHK <pass>:`
> instrumentation restored (additive-only). The current oracle gives **different
> per-pass values** than the old binary (e.g. lineage B globals is now 0x889a4909, not
> the 0xc76929e2 recorded in REFERENCE.md), but it reproduces
> `rioctrl_controller.textcfg` **byte-for-byte** — the release-equivalence proof — so
> the current numbers are the authoritative targets. lambdapnr matches the current
> oracle on every pass of both lineages.

Lineage B (with `--lpf`, the reference build command) — **lambdapnr matches every
pass, including the final packing checksum**:

| Pass | C++ (with LPF) | lambdapnr | State |
| --- | --- | --- | --- |
| after-load | 0xad3760dd | ✓ | match |
| io | 0x89a6cf21 | ✓ | match (LPF LOC→BEL) |
| dqsbuf | 0x89a6cf21 | ✓ | match |
| plls | 0xe3c91e66 | ✓ | match |
| iologic | 0xe3c91e66 | ✓ | match |
| ebr | 0xbeb5d69c | ✓ | match |
| dsps | 0x1cb449a6 | ✓ | match |
| dcus | 0x1cb449a6 | ✓ | match |
| misc | 0x1cb449a6 | ✓ | match |
| constants | 0x6a53a344 | ✓ | match |
| dram | 0xf41d603f | ✓ | match |
| carries | 0x5c420e78 | ✓ | match |
| luts | 0x269431cd | ✓ | match |
| lut5xs | 0x842a63f5 | ✓ | match |
| ffs | 0x4c2144fe | ✓ | match |
| constraints | 0x4c2144fe | ✓ | match (checksum-neutral both sides) |
| **globals** | **0x889a4909** | ✓ | **match — promote_globals ported** |
| **fixup/final** | **0x889a4909** | ✓ | **match — fixupHierarchy interning ported** |

- State diff (cells/nets/ports/attrs/params/users/bels) vs C++ at globals: **0 entries**.
- Cell-dict iteration order byte-identical at every dumped stage.
- Lineage B post-place checksum **0x519b603f** matches; post-route target 0x728f80cc.
- The oracle's `--lpf` textcfg is byte-identical to `rioctrl_controller.textcfg`.
- Lineage A (no `--lpf`) also matches every pass against the current oracle:
  after-load 0x14bc76b5, io 0x95057499, plls 0xab7b548b, ebr 0xd3512f99,
  dsps 0x37eec60a, constants 0x8d7a6160, dram 0xcf09d165, carries 0xe21df31b,
  luts 0x07b3ab60, lut5xs 0x102b6954, ffs 0xe4e9248d, globals/fixup 0xa0768a93.
- Note: the old 0xc76929e2 golden in REFERENCE.md came from the old binary; the
  current 0.10-tag oracle reproduces the reference textcfg byte-for-byte with
  post-pack 0x889a4909, so REFERENCE.md should be updated at the final gate.

## Component status matrix

| Component | File(s) | Status |
| --- | --- | --- |
| Id table / interner | Kernel/IdString.hs | ✓ intern sequence matches C++ lineage B (verified 1970..43943 index-for-index against `LPDBG intern` log) |
| Checksum | Kernel/Checksum.hs | ✓ xorshift32 fold, user chains, -1 unbound bels/pips |
| JSON frontend | Kernel/JsonFrontend.hs | ✓ full faithful port; load intern tables + state match C++ exactly |
| CLI | CLI.hs, app/Main.hs | ✓ settings intern order verified (arch.package/arch.speed/seed @ 1970-1972) |
| Netlist model | Kernel/Netlist.hs | ✓ dict/pool semantics: reverse-insertion iteration, swap-erase on delete (`deleteCellSwap`) |
| Chipdb | Arch/Ecp5/Chipdb.hs | ✓ |
| Binding | Arch/Ecp5/Binding.hs | ✓ |
| Cell timing | Arch/Ecp5/CellTiming.hs | ✓ |
| ArchCellInfo | Arch/Ecp5/ArchCellInfo.hs | ✓ assignArchInfo (muxFxad = driver cell) + slicesCompatible (slice + tile checks) |
| Pack: io..dram | Arch/Ecp5/Pack.hs | ✓ checksums + iteration order match |
| Pack: carries | Pack.hs | ✓ chain order, split feeds, attr copy, comb0/comb1 new-cell order |
| Pack: luts/lut5xs | Pack.hs | ✓ PFUMX/L6MUX21 → comb1-half, roots, constraints |
| Pack: ffs | Pack.hs | ✓ macro branch + canAddFlipflopToMacro + relConstrCells; checksum + state match |
| Pack: flush machinery | Pack.hs `flushCells` | ✓ erase-before-insert semantics replicated (see bugs #7) |
| LPF parser | Arch/Ecp5.hs `applyLpf` | ✓ LOCATE/IOBUF/SYSCONFIG/FREQUENCY/BLOCK + `customAfterLoad` check loop in Main.hs |
| pack_io LOC→BEL | Pack.hs | ✓ `finish` copies attrs, resolves LOC→BEL via `getPackagePinBel` |
| generate_constraints | Pack.hs | ✓ ported; checksum-neutral; state covered by the globals diff (0 entries) |
| promote_globals | Pack.hs | ✓ get_clocks + insert_dcc + place_dcc_dcs + DCC metrics + has_short_route; DCCA placement matches C++ (LDCC3/TDCC0) |
| fixup_hierarchy | Pack.hs | ✓ ported: trim_hierarchy + rebuild_hierarchy local-name interning with `$N` uniquification (the intern sequence between globals and BEL_STRENGTH — previously a no-op) |
| archInfoToAttributes | Pack.hs | ✓ NEXTPNR_BEL/BEL_STRENGTH on placed cells (BEL erased) + ROUTING="" on every net; BEL_STRENGTH interned in C++ order (after the first bel-cell's bel-name interns) |
| Bitgen cell writers (IO/DCC/PLL/DSP/DCU) | Bitgen.hs, DcuBitstream.hs | not started |
| Base configs | BaseConfigs.hs | ✓ |
| `.config` writer | Config.hs, Bitgen.hs | ✓ validated via ecppack on earlier milestone |
| Placer: placeConstraints/seed/buildFastBels/hpwl | Arch/Ecp5/PlacerHeap.hs | ✓ seed anchors exact (mini 33/1833; full 3838/244753); seed dump byte-identical |
| Placer: EqSys + Eigen CG solver | PlacerHeap.hs, cbits/eigen_solver.cc | ✓ 5 build/solve rounds per axis, bit-exact matrices (43204 coeffs, 0 diffs) |
| Placer: timing engine + criticality weights | Kernel/TimingAnalyser.hs, CellTiming.hs | ✓ initial iters 1355/1340/1431/1384 = C++; crits byte-exact |
| Placer: HeAP main loop | PlacerHeap.hs `placeHeapMain` | ✓ bit-exact: all 20 iterations solved/spread/legal match C++ (iter5 9218/51467/55015, iter20 21832/40337/45044), stall at 20, restore completes |
| Placer: CutSpreader | PlacerHeap.hs `cutSpread` | ✓ bit-exact: findRegions/expand/cut region inventory + cut sequences match C++; growD overuse guard fixed (top-edge region +y grow) |
| Placer: StrictLegaliser | PlacerHeap.hs `strictLegalise` | ✓ bit-exact: legal anchors match C++ (iter5 55015, iter20 45044) |
| Placer: placer1 SA refine | Placer1.hs `place1Refine` | ✓ engine ported; initial cost (wirelen 43706, timing 392.00073554665477), move counters (MV1 nmove 29548 / naccept 2823), and IT1-23 dumps bit-exact (SA breaks at iter 23 exactly like C++); **post-place checksum matches (0xf1975059, lineage A; 0x519b603f, lineage B); per-cell/per-net checksum dumps 0 diffs (15620 entities)** |
| Router (router1) | — | not started |
| Tests | test/ (85 tests) | ✓ 85/85 passing (stale constructors in 5 test files fixed this session) |

## Key bugs fixed this session (packer/ffs)

1. `ccu2ToComb` — didn't copy CCU2C attrs onto comb cells.
2. `makeCarryFeedIn` / `makeCarryFeedOut` — re-added the **stale cell template**
   via `addCell` after configuring it, wiping params/ports.
3. `findChains` — chains accumulated in reversed discovery order.
4. `splitCarryChain` — double `reverse` produced reversed chain cells; feeds were
   numbered against the wrong chains.
5. `packL6` / `packL7` — packed into the comb0-half instead of
   `get_comb1_from_lut5` target; wrong roots map keys; wrong `rel_constr_cells`
   base/rel arguments; `packL7` never inserted the mux into `pkPacked` (cells
   survived flush).
6. `assignArchInfo` — `muxFxad` was the net name; C++ stores the **driver cell**.
7. `slicesCompatible` — **`<<loop>>`**: the comb-check pair began `ok0 && ...`
   INSIDE the definition of `ok0` (self-referential thunk → blackhole). C++ uses
   independent `return false` checks; removed the `ok0 &&` prefix.
8. `flushCells` — new cells are added to the design immediately by `createCell`,
   but C++ only inserts them at flush AFTER erasing packed cells. The swap-erases
   pulled fresh new cells into the holes instead of the true back elements (VCC/GND
   etc.), silently scrambling iteration order from pack_dram onward. Fix: strip
   pkNew names from the order before the erase phase, re-append after.
9. `packCarries` — `addNew comb0 (addNew comb1 …)` pushed comb1 before comb0;
   C++ pushes comb0 then comb1 (`new_cells.push_back` order) → pairwise
   COMB0/COMB1 iteration-order swaps.
10. `idToText` — the newtype pattern `IdString i` doesn't force; on-demand
    intern thunks (belNameId) were interned AFTER the IORef snapshot read,
    so freshly interned ids resolved to `""` (`X29/Y0/` BEL attr). Force the
    index before reading the table.
11. `getBelByNameStr` — looked bel name parts up in the CONSTID table;
    `"X2"` is not a constid, so the driver-bel lookup silently failed and
    `get_dcc_metric` fell back to the wirelength metric (DCC placed at the
    first free bel instead of the dedicated-route bel). C++ interns the
    parts via `ctx->id()`.
12. `insert_dcc` user rewiring — C++ net users are an `indexed_store`
    (ascending slot order), not a reverse-iterating pool; the rebuild must
    be kept ++ [dcc CLKI] and glb = moved, both in original user order.

Also verified (no change needed): erase order in flush is `reverse pkPacked`
(`packed_cells` is a nextpnr `pool` — iterates in REVERSE insertion order, like
`dict`), and `deleteCellSwap` reproduces the C++ move-back-into-hole exactly.

### Key bugs fixed this session (placer1, uncommitted)

1. **Heap-placer restore bind-desync** — best-solution restore unbound each cell from
   its SAVED bel instead of its CURRENT bel, leaving stale `bsBel2Cell` inverse-map
   entries invisible to the checksum (PlacerHeap.hs restore; post-heap checksum
   unchanged `0x7b602790`). See the "Placer (heap) — main-loop state" note.
2. **placer1 sweep double-counted move counters** — the chain fold started from the
   autoplaced accumulators, so nmove/naccept were inflated. Now seeded from 0.
3. **clusterPlacement1 abs-z coercion** — now fails like C++ (throws) instead of
   silently falling back to `rootBel`.
4. **delayNS float-vs-double promotion** — mixed float/double arithmetic promoted
   differently than C++; aligned the types.
5. **totalTimingCost running-accumulation order** — added in C++ running order with
   no per-net `0.0`-seeded sub-sums, matching the bit-exact timing total.
6. **goIter st3 update dropped `ssNetArc = arcMap'`** (Placer1.hs) — the committed
   per-arc timing costs went stale after each iteration's timing analysis; the
   iteration carried forward the previous arcMap, causing the IT3+ divergence.
7. **iter-17 move flip = 1-ulp timing_delta from changed_arcs summation ORDER** — traced
   to port-list ordering bugs: `Kernel/Netlist.hs renameCellPort` APPENDED a duplicate
   port to cellPortOrder instead of overwriting it (C++ `dict operator[]`);
   `Pack.hs sd0Rename` used `movePort` instead of `renamePort`; and packFfs' "erase
   unconnected M" path skipped updating cellPortOrder (now `swapRemovePort`).
   Netlist.hs also restored the `removeNetDriver` export. `app/Main.hs` forces the
   post-place checksum with `seq` before printf (lazy stderr interleaving). With the
   ordering fixed, SA is bit-exact through IT23 and breaks at iter 23 exactly like C++.
8. **Post-place checksum mismatch (0xce8dfae7 vs 0xf1975059) — the SA was NOT the
   culprit.** The placement/bel/strength state was identical post-place (6443 cells,
   0 diffs); the divergence was checksum-visible state written by the **pack tail**,
   which the Haskell never ported:
   - **`Context::fixupHierarchy` is NOT a no-op** — `rebuild_hierarchy` walks
     `ctx->cells` in dict order and interns each pack-created cell's *local name*
     (substring after the last `.`, with `$N` uniquification against the hierarchy's
     `leaf_cells`). That's ~430 interns between the globals-pass ids and
     `BEL_STRENGTH`, so skipping them shifted every later id (attr keys are
     checksum-visible). Ported in Pack.hs (`fixupHierarchy` with the pre-pack
     imported-name set).
   - **`BaseCtx::archInfoToAttributes` was missing entirely** — at the end of
     `Arch::pack()` (pack.cc:3038, AFTER the checksum print) it erases `BEL`, sets
     `NEXTPNR_BEL` (bel name string) + `BEL_STRENGTH` (32-bit int property) on every
     cell that has a bel at pack end (the 2 DCC cells), and sets `ROUTING=""` on
     every net. Missing these attrs changed 9177 net checksums + 2 cell checksums.
     Ported in Pack.hs; Main.hs calls it after the pack checksum print.
   - **`BEL_STRENGTH` intern order** — C++ interns it per bel-cell AFTER that cell's
     `getBelName` interns (names-then-BEL_STRENGTH, then the next bel-cell's names),
     so the first DCC's `TDCC0` precedes it and the second's `LDCC3` follows
     (…45470 TDCC0, 45471 BEL_STRENGTH, 45472 LDCC3). The Haskell interned it up
     front. Now forced with `evaluate (T.length nm)` before the intern.
   - **`<<loop>>` in the new fixupHierarchy internLocal** — the candidate text was a
     lazy thunk containing an `idToText` read of the same IORef, forced inside
     `intern`'s `atomicModifyIORef'` (the internT trap, re-entrant interner →
     blackhole). Fixed by `T.length t `seq`` before interning.
   - Verified: per-cell/per-net checksum dumps at post-place, 15620 entities,
     **0 diffs** on both lineages; lineage A post-place 0xf1975059, lineage B
     0x519b603f; pack per-pass LPCHK matches the re-instrumented 0.10-tag oracle on
     every pass of both lineages; oracle `--lpf` textcfg still byte-identical to the
     reference.

## Known traps (recurring)

- **Stale cabal artifacts** — "Up to date" lies; always verify `.o`/binary mtime
  vs sources, `cabal clean` when corrupt.
- **Flaky NVMe** — intermittent corrupt reads (`nvme0: using unchecked data buffer`);
  parser bounds-checks + retry mitigate.
- **Lazy trace strings** — trace messages interleave with nested evaluation;
  force with `deepseq`/`length (show x)`seq`` before writing.
- **`<<loop>>` = thunk blackhole** — self-referential let bindings (bug #7) or
  re-entrant interner; localize by forcing subexpressions under
  `Control.Exception.catch` + `evaluate` (see the packFfs debug sequence).
- **json11 = std::map** — every C++ object iteration is sorted-key order;
  the port must sort every map iteration (modules, cells, netnames, ports, attrs,
  params, connections, port_directions).
- **Intern order is part of the compatibility contract** — the checksum hashes
  id *indices*, so the interning sequence must replicate C++ bit-for-bit.
- **nextpnr `dict`/`pool` semantics** — iteration is REVERSE insertion order;
  erase is swap-with-last; checksums are order-INDEPENDENT (xor-fold), so
  iteration-order bugs don't show up in checksums — compare orders explicitly
  (`LP_DUMP_ORDER`). Order only matters where passes make first-wins decisions
  (e.g. two FFs pairing with one comb in pack_ffs).
- **C++ state dumps overwrite** — every instrumented C++ run rewrites
  `/tmp/cpp_state_*.txt`; note which lineage (with/without `--lpf`) produced them.
- **Oracle-edit trap** — an edit op in placer1.cc once silently DELETED the
  following line (`last_timing_cost = curr_timing_cost;`), perturbing the SA
  checksum. After every oracle-worktree edit, `git diff` the worktree and re-prove
  the checksum before trusting dumps.
- **Lazy CSE / NOINLINE dump trap** — a debug-dump binding was
  common-subexpression-eliminated, giving spurious values. Add `NOINLINE` to dump
  thunks before trusting them.
- Memory: test binary `-M2G`, exe `-M4G` (earlier OOM root-caused and fixed).

## Placer (heap) — main-loop state

No-LPF anchors (C++ seed 1, rioctrl full): seed 244753, initial iters
1355/1340/1431/1384 ✓, then 20 main iterations:
iter1 solved=1267 spread=47324 legal=57436, iter2 solved=3246 spread=47575
legal=53888, iter3 solved=5308 spread=49515 legal=56845, … then placer1 SA refine.
Post-place target `0xf1975059` (**MATCHED — milestone 14 complete**), post-route
`0x94a9ffe1`.
> **Target correction** — the earlier `0x987ddeaf` / `0xcd1327e7` were stale, from the
> old /tmp-era worktree. The current instrumented oracle prints
> `Checksum: 0xf1975059` after place (placer_heap → placer1_refine) and `0x94a9ffe1`
> post-route — re-proved twice, byte-stable across oracle rebuilds. The final
> post-place mismatch was NOT in the SA: it was the unported pack tail
> (`fixupHierarchy` interning + `archInfoToAttributes`); see placer1 bug #8.

Current Haskell: **bit-exact through all 20 iterations** — solved/spread/legal
triplets match C++ (`/tmp/cpp_full_nolpf.preinst.log`) for every iteration,
stall counter hits 20, and the solution restore completes. The final divergent
bug was the CutSpreader `expand_regions` +y grow (growD) carrying a spurious
`overused` precondition: C++ grows +y unconditionally within the y-phase
(`if (y1 < max_y) { grow(+y); if (!overused) break; }`), so a top-edge region
(y0==0) still grows +y even after the x-phase satisfied beta density. The
Haskell `growD = srY1 < maxY && (not growU || overused stY1)` fix (PlacerHeap.hs
~1302) made iter5 spread=51467/legal=55015 exact and let the loop run to 20.

Bugs already fixed in the main loop (PlacerHeap.hs unless noted):
1. growRegion never wrote expanded bounds back to ssRegions (C++ mutates in
   place) → unbounded expansion OOM. Fixed via `updateNth (const r1)`.
2. expand_regions y-phase grows UP unconditionally (the overuse break only
   exits the x-loop) → region r1 was one row short. Fixed.
3. Cut workqueue was LIFO (prepend); C++ is FIFO (emplace back). Fixed.
4. Pivot: C++ adds the cell size THEN checks `>= total/2` (break keeps the
   current index); port checked first → pivot off by one. Fixed.
5. aU minimisation: C++ is `(cellsL+cellsR) * |cellsL/belsL - cellsR/belsR|`
   (cell-density imbalance); port divided bels/bels → wrong bestcut. Fixed.
6. strictLegalise restarted every cell from the once-unbound e1/d1, discarding
   prior cells' placements → 2-cell infinite re-queue ping-pong. Fixed: `go`
   starts from the threaded LegState.
7. isBelLocValidE recomputed assignArchInfo per candidate (O(design) each) →
   legaliser crawl. Fixed: ArchInfo threaded in, computed once.
8. cellTemplate lacked cellPortOrder → pack-created cell ports missing from
   the timing engine. Fixed: per-branch orders (TRELLIS_IO = B,I,T,O,IOLDO,IOLTO).
9. Spreader fb rebuilt the 65k-bel scan per belsAt call. Fixed: shared `fbs` list.

Spreader parity is complete. The post-spread dump and post-legalise anchors are
bit-identical to C++ (6443 cells, 0 position diffs after sorting the hs and cpp
spread dumps; raw doubles also match to 0.0 abs diff).

Oracle instrumentation (additive-only, in the nextpnr-oracle worktree): `cpp_solve_dump`
+ `cpp_spread_dump` (gated `iter==4`), `cpp_crit`/`cpp_crit_ports`/`cpp_port_dump`
(`iter==1`), `cpp_rng`, `cpp_cell_probe`/`cpp_bel_probe` (`basesoc_uart_core_tx2…`
cell), `cpp_targets`, `cpp_legal_dump`, `cpp_regions_log`, `cpp_occ_dump` +
`cpp_exp_dump` (gated `buckets.size()==3`), `cpp_flags`/`cpp_valid` (tile 24,24),
and `debug_dump_crit_ports`/`debug_dump_port`/`debug_crit_aggregate` (timing.h).
The spread/solve dump gating is `iter==4` on both sides (no re-gating this
session). `/tmp/cpp_spread_dump.preinst.txt` was regenerated this session — it
had gone stale relative to the current oracle binary, which now byte-matches the
Haskell spread dump. The preserved authoritative anchor is the iteration table
in `/tmp/cpp_full_nolpf.preinst.log`.

**Restore bind-desync bug** (found by the placer1 work; uncommitted fix in
PlacerHeap.hs): the heap-placer best-solution restore unbound each cell from its
SAVED bel instead of its CURRENT bel, leaving stale `bsBel2Cell` inverse-map entries.
Those stale entries were invisible to the checksum (post-heap checksum unchanged
`0x7b602790`), but poisoned the placer1 sweep's bel lookups. Fixed to unbind from the
current bel.

**placer1 refine cfg facts**: constraintWeight=10, netShareWeight=0,
minBelsForGridPick=64, timingFanoutThresh=INT_MAX, timing_driven=true,
crit_exp=8.0f, lambda=0.5f, temp=1e-7f (fixed), diameter=3, 15 sweeps/iter, refine
break at `n_no_progress>=1`. Placer1's FastBels uses `check_bel_available=FALSE`
(unlike the heap placer's); `get_constraints_distance` is provably 0 for refine
moves.

## Open problems

1. ~~**placer1 (SA refine)** — post-place checksum mismatch~~ **RESOLVED**: the
   divergence was in the pack tail (`fixupHierarchy` local-name interning +
   `archInfoToAttributes` attrs), not in the SA. Post-place checksums now match on
   both lineages (0xf1975059 / 0x519b603f) with 0 per-entity diffs.
2. **Router (router1)** — global clock routing (route_globals) + general
   BFS ripup-retry; wire/pip binding; post-route 0x94a9ffe1 (lineage A) /
   0x728f80cc (lineage B).
3. **Packed `.config` comparison** — with placement/routing in place, the
   textcfg should match `rioctrl_controller.textcfg` byte-for-byte; the
   pack-stage config currently differs only in the placement/routing section.
4. Remove debug scaffolding: CANADD/SC/CHAINS/SPLIT traces, `LP_DUMP_FFS_ORDER`
   + `LP_DUMP_ORDER` order dumps, packFfs dump, `deepseq` hacks, TBL dump
   (`lambdapnrDebugDump`), and the placer dumps (_dbgCut/_dbgPiv/_dbgBc/
   _dbgAU/_dbgCols/_dbgBin, sorted20, spread-dump write, `LPDBG main done`);
   keep LPCHK/LPDBG until the final gate.
5. Update REFERENCE.md's golden pack checksum (0xc76929e2 is the old binary's
   value; the current 0.10-tag oracle reproduces the reference textcfg
   byte-for-byte with post-pack 0x889a4909).
5. Timing report: Fmax (reference: crg_clkout 69.73/85.62 MHz) — engine is
   ported; the report format remains.
