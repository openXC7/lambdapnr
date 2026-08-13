# rioctrl mini — derived lambdapnr test project

A small connected subdesign extracted from the rioctrl_controller
reference netlist (`extract_mini.py`, deterministic BFS from a seed
LUT4): 48 cells (28 LUT4, 18 TRELLIS_FF, 1 PFUMX, 1 JTAGG), 70 signal
bits, no top-level ports.

## Running it like the reference

```
lambdapnr --12k --package CABGA256 --speed 6 --seed 1 \
    --json test/ecp5/rioctrl/derived/rioctrl_mini.json
```

Loads the design and reports the counts, then stops at "packing not
yet implemented".

## Comparison status vs the nextpnr reference build

| Aspect | Status |
| --- | --- |
| Device selection (`--12k` → LFE5U-12F, 25k chipdb) | identical |
| Package/speed/seed semantics | identical (CABGA256, speed 6, seed 1) |
| Cell inventory of the full reference | **identical**: LUT4 3893, TRELLIS_FF 1910, PFUMX 918, L6MUX21 285, CCU2C 234, DP16KD 25, TRELLIS_DPR16X4 8, MULT18X18D 4, EHXPLLL 1, JTAGG 1 — matches the LiteX "before packing" log exactly |
| Netlist import (drivers/users/constants/ports, ibuf insertion) | implemented, unit-tested |
| Post-pack checksum `0xc76929e2` | **not yet** — needs the packer |
| Placement/routing/Fmax (69.73/85.62 MHz etc.) | **not yet** — needs the flow |

The full reference loads in ~1s: 7,280 cells (7,279 leaf + the clk25
ibuf; the 3 `$scopeinfo` cells are skipped like the C++), 16,890 nets.
