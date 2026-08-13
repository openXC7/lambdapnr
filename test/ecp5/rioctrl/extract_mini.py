#!/usr/bin/env python3
"""Extract a small connected subdesign from the rioctrl reference netlist.

BFS from a seed LUT4 over cell connections, keeping every visited cell
and every net (bit) that touches a kept cell, plus the module header
and the clk25 port when it is reached. Deterministic: the BFS visits
cells in JSON order.

Output: a valid yosys write_json netlist that the lambdapnr JSON
frontend can load.
"""

import json

REF = "test/ecp5/rioctrl/reference/rioctrl_controller.json"
OUT = "test/ecp5/rioctrl/derived/rioctrl_mini.json"
MAX_CELLS = 48

d = json.load(open(REF))
m = d["modules"]["rioctrl_controller"]

cells = m["cells"]
netnames = m["netnames"]

# bit index -> name, from the netnames section
bit_to_name = {}
for name, nn in netnames.items():
    for i, b in enumerate(nn["bits"]):
        if isinstance(b, int) and b >= 0:
            bit_to_name.setdefault(b, name)


def cell_net_bits(cell):
    """All signal (non-constant) bit indices a cell touches."""
    bits = set()
    for conn in cell["connections"].values():
        for b in conn:
            if isinstance(b, int) and b >= 0:
                bits.add(b)
    return bits


# BFS over cells sharing nets
seed = next(n for n, c in cells.items() if c["type"] == "LUT4")
kept_cells = {}
kept_bits = set()
frontier = [seed]
while frontier and len(kept_cells) < MAX_CELLS:
    name = frontier.pop(0)
    if name in kept_cells:
        continue
    cell = cells[name]
    if cell["type"].startswith("$"):
        continue  # skip meta cells in the mini
    kept_cells[name] = cell
    bits = cell_net_bits(cell)
    kept_bits |= bits
    # enqueue neighbours: cells touching any of our bits
    for other_name, other in cells.items():
        if other_name in kept_cells or other["type"].startswith("$"):
            continue
        if cell_net_bits(other) & bits:
            frontier.append(other_name)

# netnames for the kept bits (keep original names where they exist)
kept_netnames = {}
for name, nn in netnames.items():
    keep = [b for b in nn["bits"] if isinstance(b, int) and b in kept_bits]
    if keep:
        kept_netnames[name] = {
            "hide_name": nn.get("hide_name", 0),
            "bits": keep,
            "attributes": nn.get("attributes", {}),
        }

# top ports: keep only those touched by kept cells
kept_ports = {}
for pname, p in m.get("ports", {}).items():
    if any(isinstance(b, int) and b in kept_bits for b in p["bits"]):
        kept_ports[pname] = p

mini = {
    "creator": "lambdapnr rioctrl mini extraction",
    "modules": {
        "rioctrl_controller": {
            "attributes": m.get("attributes", {}),
            "ports": kept_ports,
            "cells": kept_cells,
            "netnames": kept_netnames,
        }
    },
}

json.dump(mini, open(OUT, "w"), indent=1)
n_cells = len(kept_cells)
n_bits = len(kept_bits)
from collections import Counter

print(
    f"wrote {OUT}: {n_cells} cells, {n_bits} signal bits, "
    f"{len(kept_netnames)} netnames, {len(kept_ports)} ports"
)
print("types:", dict(Counter(c["type"] for c in kept_cells.values())))
