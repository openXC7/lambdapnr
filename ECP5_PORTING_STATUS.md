# ECP5 Port Status (lambdapnr)

Bit-compatible Haskell reimplementation of nextpnr-ECP5. Goal: byte-for-byte identical
`.config` output and identical post-pack design checksum (`Context::checksum`) on the
rioctrl reference design, pass by pass.

## Reference setup (oracles)

- **Source of truth for semantics**: pinned nextpnr submodule (51e703d0) + trellis store.
- **Source of truth for numbers**: the **nextpnr-0.10 nix binary**
  (`/nix/store/zhalr5ayq69gq0bia0nj53pdnadxz86f-nextpnr-ecp5`) and a
  git worktree at tag `nextpnr-0.10` (`/tmp/npnr-0.10`, HEAD 84856bd6), patched with:
  - `LPCHK <pass>:` checksum logs after every pack pass
  - `LPDBG intern` id-table trace (interner order oracle; logs indices >= 1970)
  - `LPDBG cell/net/cellport/cellattr/cellparam` state dumps to
    `/tmp/cpp_state_<pass>.txt` — **warning: every instrumented C++ run OVERWRITES
    these files**; regenerate the lineage you need before diffing.
  - Run instrumented binary with `LD_LIBRARY_PATH=/tmp/boost191`.
- **Two oracle lineages** (same binary; the difference is `--lpf`):
  - **Lineage A = run WITHOUT `--lpf`** — the historical checksum table
    (after-load 0x1ccd3d41 … ffs 0x19206350 … globals 0xfef78839). lambdapnr
    currently matches this lineage 100% (checksums AND iteration order).
  - **Lineage B = run WITH `--lpf` (the reference build command)** — release-equivalent:
    its final `.config` matches `test/ecp5/rioctrl/reference/rioctrl_controller.textcfg`
    byte-for-byte. Targets: after-load 0x8bc4fbe8, io 0xd1db7350, iologic 0x3e257fd2,
    ffs 0xcb9d5b45, constraints 0xcb9d5b45, **globals/final 0xc76929e2** (= the golden
    in REFERENCE.md). lambdapnr state differs from lineage B's ffs state by exactly
    2 attributes (`clk25$tr_io` BEL + LOC) — i.e. only the unported LPF path differs.
- The submodule (51e703d0) is **newer** than 0.10 and gives different checksums —
  all reference artifacts (textcfg, goldens) are 0.10-era, so pack order/pass set
  mirror **0.10's `Ecp5Packer::pack()`** (io → dqsbuf → plls → iologic → ebr → dsps →
  dcus → misc → constants → dram → carries → luts → lut5xs → ffs → constraints →
  globals; **no pack_eclk**).
- C++ build recipe (from repo root, so `nix develop` works):
  `cmake -S /tmp/npnr-0.10 -B /tmp/npnr-0.10/build -DARCH=ecp5
  -DCMAKE_BUILD_TYPE=Release -DBUILD_PYTHON=OFF -DBUILD_GUI=OFF
  -DPython3_EXECUTABLE=/home/jack/venv/bin/python3
  -DTRELLIS_INSTALL_PREFIX=/nix/store/8wz7b71s9zk9nf0vi0s8x0gbk27ipvrk-trellis-unstable-2025-01-30
  -DBoost_NO_SYSTEM_PATHS=ON -DBOOST_ROOT=/usr -DBOOST_LIBRARYDIR=/usr/lib
  -DCMAKE_EXE_LINKER_FLAGS="/usr/lib/libzstd.so /usr/lib/liblzma.so /usr/lib/libbz2.so /usr/lib/libz.so"`
  (run with `LD_LIBRARY_PATH=/tmp/boost191`).
- Haskell side: `LAMBDAPNR_PACK_STOP=<pass>` stops packing; `LP_DUMP_ORDER=1` dumps
  iteration order to `/tmp/lp_order_<pass>.txt`; `app/Main.hs` stateDump writes the
  C++ LPDBG format (in Map order — NOT iteration order; use LP_DUMP_ORDER for order).
  `/tmp/dump_state.py a b` diffs two state logs (name-based, index-independent).

## Milestones (done)

| # | Milestone | Commit |
| --- | --- | --- |
| 1 | Kernel: id table, properties, hash tables, delay calculator, deterministic RNG | 098a641 |
| 2 | Chipdb loader + memory-safe parser, arch instance | 592a4ba |
| 3 | Binding state, fanout-aware pip delays, cell timing | 8a0ac2b |
| 4 | All device variants (12k–85k), full CLI option table | a4bb50d |
| 5 | CLI option semantics + archcheck `--test` | 0c5f22e |
| 6 | yosys JSON frontend + rioctrl reference conformance | aee8612 |
| 7 | prjtrellis `.config` writer, base configs, bitgen | a742093 |
| 8 | **Packer: all passes through ffs at parity** (uncommitted) | — |

Working tree is dirty: packer, ArchCellInfo, debug scaffolding.

## Packer checksum status (rioctrl full design, 12k)

Lineage A (no `--lpf`) — **lambdapnr matches every pass**:

| Pass | C++ (no LPF) | lambdapnr | State |
| --- | --- | --- | --- |
| after-load | 0x1ccd3d41 | ✓ | match |
| io | 0x6ac5b8c1 | ✓ | match |
| plls | 0x4a4fc622 | ✓ | match |
| ebr | 0xa249f6d7 | ✓ | match |
| dsps | 0x52403d1c | ✓ | match |
| constants | 0x66d3d0b0 | ✓ | match |
| dram | 0x04dd4305 | ✓ | match |
| carries | 0x27fb1dfc | ✓ | match |
| luts | 0xe5407960 | ✓ | match |
| lut5xs | 0x74579b61 | ✓ | match |
| ffs | 0x19206350 | ✓ | match (fixed this session) |
| constraints | 0x19206350 | ✓ | match (pass is a checksum no-op both sides) |
| globals | 0xfef78839 | ✗ | `promoteGlobals` still a no-op in the port |
| final | — | — | after fixup |

Cell-dict **iteration order** verified byte-identical to C++ at every dumped stage
(load, io, plls, ebr, misc, constants, dram, carries, luts, lut5xs).

Lineage B targets (with `--lpf`), reachable once LPF parsing is ported:
after-load 0x8bc4fbe8 → io 0xd1db7350 → … → ffs 0xcb9d5b45 → constraints 0xcb9d5b45 →
**globals/final 0xc76929e2**.

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
| LPF parser | — | ✗ not ported (ecp5/lpf.cc ~200 lines); only CLI wiring exists |
| pack_io LOC→BEL | Pack.hs | ✗ missing (needs LPF LOC attr first) |
| generate_constraints | Pack.hs | ⚠ ported; checksum-neutral in C++ too — state not yet diffed |
| promote_globals | Pack.hs | ✗ no-op stub (C++ changes checksum: 0xfef78839 / 0xc76929e2) |
| fixup_hierarchy | Pack.hs | ✗ no-op stub |
| Bitgen cell writers (IO/DCC/PLL/DSP/DCU) | Bitgen.hs, DcuBitstream.hs | not started |
| Base configs | BaseConfigs.hs | ✓ |
| `.config` writer | Config.hs, Bitgen.hs | ✓ validated via ecppack on earlier milestone |
| Placer (heap) | — | not started |
| Router (router1) | — | not started |
| Tests | test/ (85 tests) | ✓ passing at last commit; rerun after packer lands |

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

Also verified (no change needed): erase order in flush is `reverse pkPacked`
(`packed_cells` is a nextpnr `pool` — iterates in REVERSE insertion order, like
`dict`), and `deleteCellSwap` reproduces the C++ move-back-into-hole exactly.

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
- Memory: test binary `-M2G`, exe `-M4G` (earlier OOM root-caused and fixed).

## Open problems

1. **Port the LPF parser** (`ecp5/lpf.cc`): LOCATE COMP/SITE, IOBUF PORT attrs,
   `settings[input/lpf]`; apply attrs to the `$nextpnr_*buf` top-port cells.
   Then the pack_io LOC→BEL step (`get_package_pin_bel`, `attrs[id_BEL]`).
   This is the ONLY remaining state delta vs lineage B (2 attrs on `clk25$tr_io`).
2. **promote_globals** (pack_globals: global net promotion, DCC insertion,
   clock constraint setup) — currently a no-op; C++ changes checksum to
   0xfef78839 (no LPF) / 0xc76929e2 (with LPF = golden).
3. **fixup_hierarchy** — currently a no-op; audit what it must change.
4. State-diff `generate_constraints` output vs C++ (checksum-neutral both sides,
   so it can't be verified by LPCHK alone).
5. Remove debug scaffolding: CANADD/SC/CHAINS/SPLIT traces, packFfs dump
   (`LP_DUMP_FFS_ORDER`), packDesign `dumpOrd`, `deepseq` hacks, TBL dump;
   rerun 85-test suite.
6. Then: bitgen cell writers (IO/DCC/PLL/DSP/DCU) → placer (heap) → router1 →
   routed `.config` byte comparison + Fmax.
