"""01b_pull_refinitiv.py

Pulls official spot rates, 1M forward/NDF outrights and (optionally) an
Argentine parallel rate from LSEG Workspace (Refinitiv) into data/raw/.

Requirements
  * LSEG Workspace desktop app open and signed in on the same machine
  * Project virtual environment: py -m venv .venv
                                 .venv\\Scripts\\python -m pip install -r requirements.txt
Run from the repository root:
  .venv\\Scripts\\python R/01b_pull_refinitiv.py

The RICs live in data/refinitiv_rics.csv so they are documented in one place.
Blank RICs are skipped. The optional `fields` column names the bid and ask
fields as "BID_FIELD|ASK_FIELD" (default "BID|ASK"); e.g. ARS1MNDF= quotes
forward points in BID/ASK and the outright in ORTS_BID/ORTS_ASK. Refinitiv
data are licensed: the output file is git-ignored and must not be pushed to
a public repository.
"""

from pathlib import Path

import lseg.data as ld
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
RICS = pd.read_csv(ROOT / "data" / "refinitiv_rics.csv", dtype=str)
RICS = RICS.dropna(subset=["ric"])
RICS["fields"] = RICS["fields"].fillna("BID|ASK")
OUT = ROOT / "data" / "raw" / "refinitiv_fx.csv"

START, END = "2021-01-01", "2026-09-30"  # keep in sync with R/00_config.R

ld.open_session()  # desktop session through the running Workspace app
frames = []
try:
    for row in RICS.itertuples():
        bid_f, ask_f = row.fields.split("|")
        df = ld.get_history(universe=row.ric, fields=[bid_f, ask_f],
                            interval="daily", start=START, end=END)
        if df is None or df.empty:
            print(f"WARNING: no data for {row.ric}; check the RIC in Workspace")
            continue
        # get_history returns a Date index and one column per field; select by
        # name rather than by position so extra or reordered columns are safe.
        missing = {bid_f, ask_f} - set(df.columns)
        if missing:
            raise KeyError(f"{row.ric}: fields {missing} not returned; "
                           f"got {list(df.columns)}")
        df = pd.DataFrame({
            "date": pd.to_datetime(df.index).date,
            "bid": pd.to_numeric(df[bid_f], errors="coerce").to_numpy(),
            "ask": pd.to_numeric(df[ask_f], errors="coerce").to_numpy(),
        })
        df = df.dropna(subset=["bid", "ask"], how="all")
        df["mid"] = (df["bid"] + df["ask"]) / 2
        df["iso3"], df["series"], df["ric"] = row.iso3, row.series, row.ric
        df["fields"] = row.fields
        frames.append(df)
        print(f"{row.ric:<14} {len(df):>5} rows")
finally:
    ld.close_session()

out = pd.concat(frames, ignore_index=True)
out.to_csv(OUT, index=False)
print(f"Wrote {len(out):,} rows to {OUT}")
