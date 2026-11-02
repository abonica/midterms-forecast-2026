###############################################################################
## features_cand_v3.R: candidate feature blocks for forecast_v3.R, sourced after src/features_v3.R
## (right before STAGE2). Each block is behind a V3_* flag (off by default) and appends to EXT.F.
##   data/v2/donor_profiles_<MMDD>.rds   src/build_donor_profiles.R       donor profiles as of the money cutoff
##   data/bios/contest_background.csv    src/code_candidate_bios.py,      background as of each election,
##                                       src/build_candidate_background.R coded from cfdb.cont_cand_db bios
###############################################################################

## V3_DPROF: donor-profile contrasts (D - R), standardized within cycle. pacwatch's gift files hold far more small
## earmarked gifts from 2020 on, so the levels jump between 2018 and 2020; within-cycle z-scores compare
## candidates only with others in the same cycle. 2012+ only (NA before).
## Adopted 2026-10-21 (default on): on the pre-stage-1-change base 46 -> 42 misses, Brier .0221 -> .0214; on top of
## V3_INC_COMP equal misses (41), Brier .0210 -> .0207, competitive -.0021, seat CRPS 3.06 -> 3.03
if(Sys.getenv('V3_DPROF', '1') == '1'){
    f <- Sys.getenv('V3_DPROF_FILE', file.path(PROJ, sprintf('data/v2/donor_profiles_%s.rds', gsub('-', '', CUTOFF.MD))))   ## V3_DPROF_FILE: an alternate profile file (diagnostics)
    if(file.exists(f)){
        dp <- as.data.table(readRDS(f))
        DP.V <- c('dp.cap100.d', 'dp.cap25', 'dp.small', 'dp.age65', 'dp.new', 'dp.gpd')
        for(v in DP.V){
            kd <- match(paste(H$dem.rid.static, H$cycle), paste(dp$rid, dp$cycle))
            kr <- match(paste(H$rep.rid.static, H$cycle), paste(dp$rid, dp$cycle))
            set(H, j = paste0(v, '.x'), value = dp[[v]][kd] - dp[[v]][kr])
            H[, (paste0(v, '.x')) := (get(paste0(v, '.x')) - mean(get(paste0(v, '.x'))[scored | contested26], na.rm = TRUE)) /
                  sd(get(paste0(v, '.x'))[scored | contested26], na.rm = TRUE), by = cycle]
        }
        EXT.F <- c(EXT.F, paste0(DP.V, '.x'))
        cat(sprintf('donor profiles: %s; coverage %s\n', basename(f), paste(H[cycle >= 2012 & (scored | contested26), sprintf('%d %.0f%%', cycle, 100 * mean(!is.na(dp.cap25.x))), by = cycle]$V1, collapse = ', ')))
    } else warning('V3_DPROF: no file ', f, ' (build with src/build_donor_profiles.R)')
}

## V3_BIO: candidate background as of each election: highest prior office level for each side (0 none ... 4 Congress;
## incumbents set to 4), and D - R gaps in strong local roots, military service, prominence and earlier federal runs.
## NA where a side has no bio (about 6-20% of nominees a cycle; 45% of 2026 non-incumbents).
if(Sys.getenv('V3_BIO', '0') == '1'){
    f <- file.path(PROJ, 'data/bios/contest_background.csv')
    if(file.exists(f)){
        bgw <- fread(f); k <- match(H$rn, bgw$rn)
        H[, `:=`(bio.office.d = bgw$bg.office_dem[k], bio.office.r = bgw$bg.office_rep[k], bio.local = bgw$bg.local.strong.diff[k],
                 bio.mil = bgw$bg.military.diff[k], bio.notable = bgw$bg.notable.diff[k], bio.prior.fed = bgw$bg.prior.fed.diff[k])]
        EXT.F <- c(EXT.F, 'bio.office.d', 'bio.office.r', 'bio.local', 'bio.mil', 'bio.notable', 'bio.prior.fed')
        cat(sprintf('candidate background: coverage %s\n', paste(H[cycle >= 2006 & (scored | contested26), sprintf('%d %.0f%%', cycle, 100 * mean(!is.na(bio.local))), by = cycle]$V1, collapse = ', ')))
    } else warning('V3_BIO: no file ', f)
}
