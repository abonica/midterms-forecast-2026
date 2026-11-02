#!/usr/bin/env bash
## Runs the production House, Senate and governor models from this folder (R >= 4.3 with data.table, xgboost, ranger, glmnet, mgcv).
## Outputs go to output_v3/. The published forecast uses current 2026 information (polls through the fetch date, the latest
## expert ratings, money through the Oct 14 pre-general reports); past cycles always use the Oct 18 / Sep 30 cutoffs.
set -euo pipefail
cd "$(dirname "$0")"
export SML_PROJ="$PWD" SML_SCRATCH="$PWD/ext/scratch" SML_CP_DATA="$PWD/ext/cp_data" V3_POLLS_LATEST="${V3_POLLS_LATEST:-1}" V3_RATINGS_LATEST="${V3_RATINGS_LATEST:-1}" V3_MONEY_LATEST="${V3_MONEY_LATEST:-1}"
for s in forecast_v3 forecast_senate_v3 forecast_governor_v3; do
  Rscript --no-environ $s.R > output_v3/$s.log 2>&1 || { echo "FAILED $s (see output_v3/$s.log)"; exit 1; }
  grep -E "RECENT2018_24|2026 House:|2026 Senate|2026 governors" output_v3/$s.log || true
done
