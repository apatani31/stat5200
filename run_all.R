# run_all.R
# Reproduces every output from raw data. Run from the repository root:
#   Rscript run_all.R
# First run only: install.packages("renv"); renv::restore()

source("R/00_config.R")
source("R/01_download.R")
# Refinitiv pull runs separately (needs the Workspace app open):
#   python R/01b_pull_refinitiv.py
# 02_clean.R uses its output if present, otherwise falls back to Yahoo.
source("R/02_clean.R")
source("R/03_descriptives.R")
# Later stages (added as the project develops):
# source("replication/01_replicate_gks2024.R")
# source("R/04_models.R")
