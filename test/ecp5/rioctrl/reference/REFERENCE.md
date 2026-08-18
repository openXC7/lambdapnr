# rioctrl_controller — nextpnr-ecp5 reference build

A real LiteX SoC built with nextpnr-ecp5, copied from
`/devel/riscv/litex-root/litex-boards/build/rioctrl_controller/gateware/`
as a conformance reference for the Haskell port.

## Build command (from build_rioctrl_controller.sh)

```
yosys -l rioctrl_controller.rpt rioctrl_controller.ys
nextpnr-ecp5 --json rioctrl_controller.json --lpf rioctrl_controller.lpf \
    --textcfg rioctrl_controller.config \
    --12k --package CABGA256 --speed 6 --timing-allow-fail --seed 1
ecppack --bootaddr 0 rioctrl_controller.config --svf ... --bit ...
```

Device: **LFE5U-12F-6BG256C** (the 12k part; nextpnr serves it from
chipdb-25k.bin). Seed 1, speed grade 6, package CABGA256.

## Golden facts (from litex.log / the JSON)

| Quantity | Value |
| --- | --- |
| Checksum (after packing) | `0x889a4909` (lineage B, current oracle; the historical `0xc76929e2` was a different nextpnr build) |
| Top module cells | 7,282 |
| LUT4 | 3,893 (= "logic LUTs" in the pack log) |
| TRELLIS_FF | 1,910 (= "Total DFFs") |
| PFUMX | 918 |
| L6MUX21 | 285 |
| CCU2C | 234 (carry chains) |
| DP16KD | 25 |
| TRELLIS_DPR16X4 | 8 (RAM LUTs) |
| MULT18X18D | 4 |
| EHXPLLL | 1 |
| JTAGG | 1 |
| nets (netnames) | 5,631 |
| top ports | clk25 (input, LVCMOS33, pin A7 per the LPF) |
| Fmax (post-PnR) | `crg_clkout` 69.73/85.62 MHz, jtag tck 191.39/179.21 MHz (two runs) |
| Checksum (post-place) | `0x519b603f` (lineage B, with LPF) / `0xf1975059` (lineage A, no LPF) |
| Checksum (post-route) | `0x728f80cc` (lineage B, with LPF) / `0x94a9ffe1` (lineage A, no LPF) |

## Files

- `rioctrl_controller.json` — yosys `synth_ecp5` netlist (input)
- `rioctrl_controller.lpf` — pin constraints (input)
- `rioctrl_controller.textcfg` — nextpnr output bitstream config (reference output)

The pack log (packing order, utilisation) and the timing report are in
the LiteX build log; the checksum is printed after packing.
