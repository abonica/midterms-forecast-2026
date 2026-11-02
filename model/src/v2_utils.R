###############################################################################
## Shared settings and helpers for the v2 pipeline (run_v2.sh)
###############################################################################
library(data.table)

PROJ        <- Sys.getenv('SML_PROJ', '/workspace/sml_election_forecast')
V2_DIR      <- file.path(PROJ, 'data/v2')                        ## small intermediates
SCRATCH     <- Sys.getenv('SML_SCRATCH', '/ssd2/bonica/sml_v2')   ## big files, off Dropbox
ARESULTS    <- file.path(SCRATCH, 'aresults_79_26_pinned_20260922.rda')
CP_REFACTOR <- Sys.getenv('SML_CP_REFACTOR', '/workspace/cp_proj_refactor')

CYCLES         <- seq(1996, 2026, 2)
FORECAST_CYCLE <- 2026
## Money cutoff for the transaction-derived "asof" features, the same in every cycle:
##  'MM-DD':   only those dated on or before this month-day of the election year (default 09-30 = the Q3
##             reports, used with the Oct 18 forecast date; 06-30 for the Oct 9 one)
##  'election': receipts and spending through each cycle's election day, so past cycles are complete
##             and 2026 has everything loaded so far (for V3_CUTOFF=none)
CUTOFF_MMDD    <- Sys.getenv('SML_CUTOFF_MMDD', '09-30')
## general election day: the Tuesday after the first Monday of November
election.day <- function(cyc){ d <- as.Date(paste0(cyc, '-11-02')); d + (2L - as.POSIXlt(d)$wday) %% 7L }
money.cutoff <- function(cyc) if(CUTOFF_MMDD == 'election') election.day(cyc) else as.Date(paste0(cyc, '-', CUTOFF_MMDD))
## training-set file for a money cutoff: data/cong_training_set.rdata (no cutoff) or _<MMDD> (e.g. _0930)
trainset.file <- function(cut = CUTOFF_MMDD) file.path(PROJ, if(cut == 'election') 'data/cong_training_set.rdata' else
                                                        sprintf('data/cong_training_set_%s.rdata', gsub('-', '', cut)))

dir.create(V2_DIR, showWarnings = FALSE, recursive = TRUE)

cfdb.con <- function() stop("cfdb is not available in the release bundle")

## '2024_AL_01' -> '2024_AL_1'; '2024_AZ_S' unchanged; at-large '_0' -> '_1'
norm.distcyc <- function(x){
    p <- tstrsplit(x, '_', fixed = TRUE)
    d <- p[[3]]
    num <- suppressWarnings(as.integer(d))
    d[!is.na(num)] <- as.character(pmax(num[!is.na(num)], 1L))
    paste(p[[1]], toupper(p[[2]]), d, sep = '_')
}

## Follow absorbed -> keeper rid chains from cfdb.fix_rid_merge_backup
apply.rid.map <- function(rid, rid.map){
    if(is.null(rid.map) || nrow(rid.map) == 0) return(rid)
    for(i in 1:5){
        m <- match(rid, rid.map$bonica_rid_old)
        if(!any(!is.na(m))) break
        rid[!is.na(m)] <- rid.map$bonica_rid_new[m[!is.na(m)]]
    }
    rid
}

log.msg <- function(...) cat(format(Sys.time(), '%H:%M:%S'), ..., '\n')
