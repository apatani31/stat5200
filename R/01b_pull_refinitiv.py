"""01b_pull_refinitiv.py

Pulls official spot rates, 1M forward/NDF outrights and (optionally) an
Argentine parallel rate from LSEG Workspace (Refinitiv) into data/raw/.

Requirements
  * LSEG Workspace desktop app open and signed in on the same machine
  * pip install lseg-data pandas
Run from the repository root:
  python R/01b_pull_refinitiv.py

The RICs live in data/refinitiv_rics.csv so they are documented in one place.
Blank RICs are skipped. Refinitiv data are licensed: the output file is
git-ignored and must not be pushed to a public repository.
"""

from pathlib import Path

import lseg.data as ld
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
RICS = pd.read_csv(ROOT / "data" / "refinitiv_rics.csv").dropna(subset=["ric"])
OUT = ROOT / "data" / "raw" / "refinitiv_fx.csv"

START, END = "2021-01-01", "2026-09-30"  # keep in sync with R/00_config.R

ld.open_session()  # desktop session through the running Workspace app
frames = []
for row in RICS.itertuples():
    df = ld.get_history(universe=row.ric, fields=["BID", "ASK"],
                        interval="daily", start=START, end=END)
    if df is None or df.empty:
        print(f"WARNING: no data for {row.ric}; check the RIC in Workspace")
        continue
    df = df.reset_index()
    df.columns = ["date", "bid", "ask"]
    df["mid"] = (pd.to_numeric(df["bid"]) + pd.to_numeric(df["ask"])) / 2
    df["iso3"], df["series"], df["ric"] = row.iso3, row.series, row.ric
    frames.append(df)
ld.close_session()

out = pd.concat(frames, ignore_index=True)
out.to_csv(OUT, index=False)
print(f"Wrote {len(out):,} rows to {OUT}")
