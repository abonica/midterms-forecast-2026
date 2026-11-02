###############################################################################
## forecast_senate_v3.R: Senate forecast with a rolling-origin backtest
##
## y - .5 = a*lean + b*inc + c*env + residual model + poll blend, where env is
## the House national cycle effect (generic ballot model, predicted from
## earlier cycles only). ~30 races a cycle, so the residual model is ridge +
## random forest rather than boosting. Polls dominate where they exist.
## in:  data/cong_training_set.rdata, data/expert_ratings/senate_ratings.csv,
##      data/polls/historical/realclearpolling_senate/..., data/polls/senate_polls_2026*.csv
## out: output_v3/senate_*
###############################################################################
source(file.path(Sys.getenv('SML_PROJ', '/workspace/sml_election_forecast'), 'forecast_v3.R'))   ## House data, helpers, national model
## Senate-specific error scales. Pinned to the values the Senate was tuned with (the House moved to NAT_SCALE = 1
## and a 4-cycle district SD on 2026-10-21; a Senate retest of those is still to do). State legislatures inherit these.
SCALE_D   <- as.numeric(Sys.getenv('V3S_SCALE_D', .8))
NAT_SCALE <- as.numeric(Sys.getenv('V3S_NAT_SCALE', 1.5))
DSCALE_N  <- as.integer(Sys.getenv('V3S_DSCALE_N', 0))
DSCALE_ROLL <- FALSE   ## the Senate has its own rolling multiplier (V3S_SCALE_ROLL); V3_NAT_MODE still applies
BT_S <- seq(2010, 2024, 2)
## training window for both stages, cycles >= min(S_MIN_TRAIN, T - 8) as in the House
## (the incumbency effect fell from ~.11 in 1996-2004 to ~.03 in 2018-24); 0 = all cycles
S_MIN_TRAIN <- as.numeric(Sys.getenv('V3S_MIN_TRAIN', 2010))

S <- as.data.table(cong.trainset$X, keep.rownames = 'rn')[grepl('_S2?$', rn)]   ## _S2: second race of a state-cycle (special)
S[, ref := cycle - fifelse(cycle %% 4 == 0, 4, 2)]
S[, lean := dist.pres.vs - nat.pres[as.character(ref)]]
S[, inc := fcase(inc.status == 'I', 1, inc.status == 'C', -1, default = 0)]
S <- qa.apply(S, 'senate', cols = setdiff(fread(file.path(PROJ, 'data/qa/qa_fixes_2026-10-23.csv'))[office == 'senate', unique(column)], 'scored'))
S[, y := dem.cand.vs - .5]
S[, dr := !is.na(dem.name) & !is.na(rep.name) & tolower(dem.name) != tolower(rep.name)]
S[, scored := dr & !is.na(dem.cand.vs) & dem.cand.vs > .05 & dem.cand.vs < .95 & !is.na(lean)]
S <- qa.apply(S, 'senate', cols = 'scored', drop = FALSE)
## V3_QA: the governor rule for strong third candidates (a third candidate with >= 15% makes the D-vs-R share a different
## quantity: 2006 CT Lieberman, 2010 AK Murkowski write-in, 2010 FL Crist, 2016 AK Miller, 2022 AK R-vs-R ...): not scored
if(QA){
    tq <- fread(file.path(PROJ, 'data/third_party/senate_third_party.csv'))[top_third_share >= .15, distcyc]
    cat('QA: Senate races with a third candidate >= 15% not scored:', paste(intersect(tq, S[(scored), rn]), collapse = ' '), '\n')
    S[rn %in% tq, scored := FALSE]
}

## 2026 Dem-backed independents (dem slot empty): the independent takes the
## anti-Republican slot for modeling. Money for them is not in the data (NA).
ind26 <- data.table(rn = c('2026_NE_S', '2026_ID_S', '2026_SD_S', '2026_MT_S'),
                    ind = c('Dan Osborn', 'Todd Achilles', 'Bengs', 'Bodnar'))
S[, contested26 := cycle == FORECAST & !is.na(rep.name) & (!is.na(dem.name) | rn %in% ind26$rn)]

## candidate's prior overperformance (House or Senate race), as for the House
S[, cyc.mean := mean((y - lean)[scored]), by = cycle]
S[, resid0 := fifelse(scored, y - lean - cyc.mean, NA_real_)]
perf.s <- rbind(perf, S[!is.na(dem.rid.static) & !is.na(resid0), .(rid = dem.rid.static, cycle, over = resid0)],
                S[!is.na(rep.rid.static) & !is.na(resid0), .(rid = rep.rid.static, cycle, over = -resid0)])
perf.s <- perf.s[, .(over = mean(over)), by = .(rid, cycle)]; setkey(perf.s, rid, cycle)
prev.s <- function(r, cy) perf.s[data.table(rid = r, cycle = cy - 1), over, roll = Inf]
S[, dem.prev.over := prev.s(dem.rid.static, cycle)]
S[, rep.prev.over := prev.s(rep.rid.static, cycle)]

S[, `:=`(m.tot = ld(total.dem, total.rep), m.ind = ld(total.ind.dem, total.ind.rep),
         m.pac = ld(total.pac.dem, total.pac.rep), m.cash = ld(spare.cash.dem, spare.cash.rep),
         m.q6 = ld(dem.q6, rep.q6))]
## 2026 Senate: cfdb has not linked some candidates' Senate committees to their
## rids (GA Collins, IA Hinson, SC D. Graham, ME Jackson: no receipts) and one rid
## is inflated (AK Sullivan, $238M vs $9M in FEC summaries). Where the as-of
## total is missing or far off the FEC summary, use the FEC-summary D/R ratio
## for m.tot (the ratio cancels the date mismatch) and leave the rest NA.
off <- function(tot, fc) !is.na(fc) & fc > 1e5 & (fcoalesce(tot, 0) < .25 * fc | tot > 4 * fc)
S[, bad.money := cycle == FORECAST & (off(total.dem, fullcycle.total_receipts.dem) | off(total.rep, fullcycle.total_receipts.rep))]
S[(bad.money), `:=`(m.tot = ld(fullcycle.total_receipts.dem, fullcycle.total_receipts.rep),
                    m.ind = NA_real_, m.pac = NA_real_, m.cash = NA_real_, m.q6 = NA_real_)]
S[cycle == FORECAST & !bad.money & (fcoalesce(total.dem, 0) == 0 | fcoalesce(total.rep, 0) == 0),
  c('m.tot', 'm.ind', 'm.pac', 'm.cash', 'm.q6') := NA_real_]
## FEC Q3 report summaries (cycle-to-date through Sept 30, matching the money cutoff) for
## the races with cfdb linkage problems: data/2026/senate_fec_summaries_2026.csv
fecf <- file.path(PROJ, 'data/2026/senate_fec_summaries_2026.csv')
if(file.exists(fecf)){
    fs <- fec.pick(fread(fecf))
    fd <- fs[party %in% c('D', 'DEM', 'Democratic')]; fr2 <- fs[party %in% c('R', 'REP', 'Republican')]
    for(rr in intersect(fd$distcyc, fr2$distcyc)){
        a <- fd[distcyc == rr][1]; b <- fr2[distcyc == rr][1]
        S[rn == rr, `:=`(m.tot = ld(a$total_receipts, b$total_receipts), m.ind = ld(a$indiv_contrib, b$indiv_contrib),
                         m.pac = ld(a$pac_contrib, b$pac_contrib), m.cash = ld(a$cash_on_hand, b$cash_on_hand), m.q6 = NA_real_)]
    }
    cat('Senate money from FEC report summaries:', paste(intersect(fd$distcyc, fr2$distcyc), collapse = ' '), '\n')
}
## V3_FILL2: races whose money is missing or far off take both sides from the FEC report summaries as of Sep 30
## (data/fec/fec_fill_asof.csv, src/fetch_fec_fill.py); this also gives the 2026 FEC-sourced races their Q6 bin
if(FILL2 && file.exists(file.path(PROJ, 'data/fec/fec_fill_asof.csv'))){
    ff <- fread(file.path(PROJ, 'data/fec/fec_fill_asof.csv'))[office == 'senate' & !is.na(receipts)]
    for(rr in intersect(ff[side == 'dem', rn], ff[side == 'rep', rn])){
        a <- ff[rn == rr & side == 'dem'][1]; b <- ff[rn == rr & side == 'rep'][1]
        S[rn == rr, `:=`(m.tot = ld(a$receipts, b$receipts), m.ind = ld(a$indiv, b$indiv), m.pac = ld(a$pac, b$pac),
                         m.cash = ld(a$receipts - a$disb, b$receipts - b$disb), m.q6 = ld(a$q6, b$q6))]
    }
    cat('Senate FEC fill:', paste(intersect(ff[side == 'dem', rn], ff[side == 'rep', rn]), collapse = ' '), '\n')
}
## V3_MONEY_LATEST (2026 only): shift each money contrast by the Oct 1-14 money in the candidates' pre-general reports,
## from the Sep 30 levels the contrast was built on (FEC summaries where the race takes its money from them, else DIME)
if(MONEY.LATEST && !is.null(fec.late)){
    ff <- if(file.exists(file.path(PROJ, 'data/fec/fec_fill_asof.csv'))) fread(file.path(PROJ, 'data/fec/fec_fill_asof.csv'))[office == 'senate' & !is.na(receipts)] else data.table()
    base <- function(s, v, fv){ x <- S[[paste0(v, '.', s)]]; if(nrow(ff)){ k <- match(paste(S$rn, s), paste(ff$rn, ff$side)); x <- fcoalesce(ff[[fv]][k], x) }; fcoalesce(x, 0) }
    sh <- function(v, fv, inc){ d0 <- base('dem', v, fv); r0 <- base('rep', v, fv); ld(d0 + late.inc(S, 'dem', inc), r0 + late.inc(S, 'rep', inc)) - ld(d0, r0) }
    cash <- function(s) base(s, 'total', 'receipts') - base(s, 'total.disbursements', 'disb')
    dc <- ld(cash('dem') + late.inc(S, 'dem', 'inc_receipts') - late.inc(S, 'dem', 'inc_disb'), cash('rep') + late.inc(S, 'rep', 'inc_receipts') - late.inc(S, 'rep', 'inc_disb')) - ld(cash('dem'), cash('rep'))
    S[, `:=`(m.tot = m.tot + sh('total', 'receipts', 'inc_receipts'), m.ind = m.ind + sh('total.ind', 'indiv', 'inc_indiv'),
             m.pac = m.pac + sh('total.pac', 'pac', 'inc_pac'), m.cash = m.cash + dc)]
    cat('Senate money through Oct 14: pre-general reports for', S[cycle == FORECAST, sum(dem.fec.cand.id %in% fec.late$cand_id)], 'D and', S[cycle == FORECAST, sum(rep.fec.cand.id %in% fec.late$cand_id)], 'R nominees\n')
}
for(v in c('m.tot', 'm.ind', 'm.pac', 'm.cash', 'm.q6'))
    S[, (v) := get(v) - median(get(v)[scored | contested26], na.rm = TRUE), by = cycle]
## independents' money is missing, not zero
S[rn %in% ind26$rn, c('m.tot', 'm.ind', 'm.pac', 'm.cash', 'm.q6') := NA_real_]
S[, `:=`(dem.ext = dem.ip - dem.ip.party.cycle.avg, rep.ext = rep.ip - rep.ip.party.cycle.avg)]

## expert ratings as of Oct 9; tables listing competitive races only: unlisted = Safe for the incumbent party
srt <- rate.file(file.path(PROJ, 'data/expert_ratings/senate_ratings.csv'))[rater %in% c('cook', 'inside', 'sabato')]
srt <- rbind(srt[, .(distcyc, rater, rating_num)], own.ratings(), fill = TRUE)   ## 2026 dossier rating as a fourth rater
srt <- srt[, .(rating = mean(rating_num, na.rm = TRUE) / 3), by = distcyc]
S[, rating := srt$rating[match(rn, srt$distcyc)]]
S[cycle >= 2006 & cycle <= 2024 & is.na(rating), rating := fifelse(inc != 0, inc, sign(lean))]

FS <- c('lean', 'inc', 'm.tot', 'm.ind', 'm.pac', 'm.cash', 'm.q6', 'dem.prev.over', 'rep.prev.over',
        'dem.ip', 'rep.ip', 'dem.ext', 'rep.ext', 'quality.chall.dem', 'quality.chall.rep', 'rating')
## incumbent senators' Nokken-Poole score, career to date through the Congress before the election (as the
## House V3_NP; adopted 2026-10-20: 2018-24 RMSE .0366 -> .0362, same misses, Brier no worse)
if(Sys.getenv('V3S_NP', '1') == '1'){
    lnm <- function(x){ x <- tolower(iconv(x, 'UTF-8', 'ASCII//TRANSLIT')); x <- fifelse(grepl(',', x), sub(',.*', '', x), x)
        x <- trimws(gsub('[^a-z ]', ' ', gsub('-', ' ', x))); x <- sub(' (jr|sr|ii|iii|iv)$', '', x); sub('.* ', '', x) }
    sv <- fread(file.path(Sys.getenv('SML_SCRATCH', '/ssd2/bonica/sml_v2'), 'dl/HSall_members.csv'))[chamber == 'Senate' & party_code %in% c(100, 200),
        .(congress, icpsr, st = state_abbrev, pty = fifelse(party_code == 100, 'D', 'R'), np = nokken_poole_dim1, nv = nominate_number_of_votes, last = lnm(sub(',.*', '', bioname)))]
    sv <- sv[!duplicated(paste(congress, icpsr))]
    sv[order(congress), np := { w <- fifelse(is.na(np), 0, fcoalesce(as.numeric(nv), 0)); cumsum(w * fcoalesce(np, 0)) / cumsum(w) }, by = icpsr]
    su <- sv[, if(.N == 1) .SD, by = .(congress, st, pty, last)]
    snp <- function(icp, nm, pty){ ic <- suppressWarnings(as.integer(fifelse(grepl('^[0-9]+$', icp), substr(icp, 1, nchar(icp) - 4), NA_character_)))
        cg <- (S$cycle - 1788) / 2
        fcoalesce(sv$np[match(paste(cg, ic), paste(sv$congress, sv$icpsr))], su$np[match(paste(cg, S$state, pty, lnm(nm)), paste(su$congress, su$st, su$pty, su$last))]) }
    S[, `:=`(dem.np = snp(dem.icpsr, dem.name, 'D'), rep.np = snp(rep.icpsr, rep.name, 'R'))]
    S[inc != 1, dem.np := NA_real_]; S[inc != -1, rep.np := NA_real_]
    cat(sprintf('Senate Nokken-Poole: %.1f%% of incumbents matched\n', 100 * S[inc != 0, mean(!is.na(fifelse(inc == 1, dem.np, rep.np)))]))
    FS <- c(FS, 'dem.np', 'rep.np')
}
## a Dem-backed independent is not the dem-slot candidate (MT: Democrat Bankhead is also on the ballot), so the
## dem-side candidate features are unknown for these rows, not Bankhead's
for(v in intersect(c('dem.ip', 'dem.ext', 'dem.prev.over', 'dem.np', 'dem.female', 'quality.chall.dem'), names(S)))
    set(S, which(S$rn %in% ind26$rn), v, NA)
## V3S_TP: minor candidates on the ballot (known before the election; data/third_party/, src/build_senate_third_party.R):
## any left / right minor candidate, and the count. Vote shares are not used (2026 has none).
if(Sys.getenv('V3S_TP', '0') == '1'){
    tp <- fread(file.path(PROJ, 'data/third_party/senate_third_party.csv'))
    k <- match(S$rn, tp$distcyc)
    S[, `:=`(tp.left = as.numeric(tp$n_left[k] > 0), tp.right = as.numeric(tp$n_right[k] > 0), tp.n = as.numeric(tp$n_third[k]))]
    cat(sprintf('Senate third-party ballots: %.1f%% of rows matched\n', 100 * S[, mean(!is.na(k))]))
    FS <- c(FS, 'tp.left', 'tp.right', 'tp.n')
}

###############################################################################
## National environment from the House model: actual cycle effects for
## training cycles; for the test cycle, predicted from the generic ballot with
## a model fit on earlier cycles
###############################################################################
nat.env <- function(T){
    all <- H[scored & cycle < T]
    m <- lm(y ~ 0 + lean + inc + factor(cycle), all)
    fe <- data.table(cycle = as.numeric(sub('factor\\(cycle\\)', '', grep('cycle', names(coef(m)), value = TRUE))),
                     fe = coef(m)[grep('cycle', names(coef(m)))])
    cyc <- unique(all[, .(cycle, gb, mid.pen)])[fe, on = 'cycle']
    m2 <- lm(fe ~ gb + mid.pen, cyc)
    list(fe = fe, pred = predict(m2, unique(H[cycle == T, .(gb, mid.pen)]))[1])
}

fit.senate <- function(T, te){
    ne <- nat.env(T)
    tr <- S[scored & cycle < T & cycle >= min(S_MIN_TRAIN, T - 8)]
    tr[, env := ne$fe$fe[match(cycle, ne$fe$cycle)]]; te <- copy(te)[, env := ne$pred]
    ## V3S_INC_COMP: incumbency x state safety, as the House stage 1 (adopted there 2026-10-21); the Senate
    ## fundamentals also overweight lean (residual slope -.095), which the interaction may absorb in part
    m1 <- if(Sys.getenv('V3S_INC_COMP', '0') == '1') lm(y ~ 0 + lean + inc + inc:abs(lean) + env, tr) else lm(y ~ 0 + lean + inc + env, tr)
    r <- tr$y - predict(m1, tr)
    FSx <- FS
    if(S_POLLFEAT){
        ## poll average minus the stage-1 base, and poll volume (NA when unpolled: imputed + flagged).
        ## V3S_PGAP_OOS (default 1): the training base uses each training cycle's own out-of-sample predicted
        ## environment, as the test cycle's does (the House fix of 2026-10-21); 0 = the realized environment
        if(Sys.getenv('V3S_PGAP_OOS', '1') == '1'){
            uc <- unique(tr$cycle); eo <- sapply(uc, function(c) tryCatch(nat.env(c)$pred, error = function(e) NA_real_))
            tr.oos <- copy(tr)[, env := fcoalesce(eo[match(cycle, uc)], env)]
            tr[, pgap := poll.x - predict(m1, tr.oos)]
        } else tr[, pgap := poll.x - predict(m1, tr)]
        tr[, pneff := log1p(fcoalesce(neff.x, 0))]
        te[, `:=`(pgap = poll.x - predict(m1, te), pneff = log1p(fcoalesce(neff.x, 0)))]
        FSx <- c(FS, 'pgap', 'pneff')
    }
    Xtr <- as.matrix(tr[, ..FSx]); Xte <- as.matrix(te[, ..FSx])
    wts <- rep(1, nrow(Xtr))
    ## V3S_POOL: the residual model also learns from the House rows of the same cycles (their residuals from the
    ## House stage 1, the same feature names), with an office indicator; Senate rows weighted V3S_POOL_W (default 5)
    ## Adopted 2026-10-21 (default on): 2018-24 Brier .0545 -> .0524 (paired -.0020, se .0007), misses 9 -> 9; 2010-24 Brier
    ## .0579 -> .0558, misses 19 -> 20; 2026 P(51+) .67 -> .68
    if(Sys.getenv('V3S_POOL', '1') == '1'){
        hw <- H[scored & cycle < T & cycle >= min(S_MIN_TRAIN, T - 8)]
        hs1 <- stage1(H[scored & cycle < T], hw, T, Inf, F2); hr <- hw$y - base1(hs1, hw, hs1$fe$fe[match(hw$cycle, hs1$fe$cycle)])
        fh <- intersect(FSx, names(hw)); miss <- setdiff(FSx, fh)
        Xh <- as.matrix(hw[, ..fh]); for(v in miss) Xh <- cbind(Xh, setNames(rep(NA_real_, nrow(Xh)), v)); Xh <- Xh[, FSx, drop = FALSE]
        Xtr <- rbind(cbind(Xtr, office = 1), cbind(Xh, office = 0)); Xte <- cbind(Xte, office = 1)
        r <- c(r, hr); wts <- c(rep(as.numeric(Sys.getenv('V3S_POOL_W', 5)), nrow(tr)), rep(1, nrow(hw)))
    }
    imp <- function(M, ref) { for(j in seq_len(ncol(M))) M[is.na(M[, j]), j] <- fcoalesce(median(ref[, j], na.rm = TRUE), 0); M }
    na.f <- function(M) { z <- is.na(M[, colSums(is.na(Xtr)) > 0, drop = FALSE]) * 1; colnames(z) <- paste0(colnames(z), '.na'); z }
    A <- cbind(imp(Xtr, Xtr), na.f(Xtr)); B <- cbind(imp(Xte, Xtr), na.f(Xte))
    cv <- glmnet::cv.glmnet(A, r, alpha = 0, nfolds = 5, weights = wts)
    p.ridge <- as.numeric(predict(cv, B, s = 'lambda.min'))
    rf <- ranger::ranger(x = imp(Xtr, Xtr), y = r, num.trees = 500, min.node.size = 10, num.threads = 8, seed = 1, case.weights = wts)
    p.rf <- predict(rf, imp(Xte, Xtr))$predictions
    predict(m1, te) + (p.ridge + p.rf) / 2
}

###############################################################################
## Senate polls: RealClearPolling 2006-2024 (no sponsor flags) + Wikipedia 2026
###############################################################################
rcp <- fread(file.path(PROJ, 'data/polls/historical/realclearpolling_senate/2006_to_2024_senate_polls_with_actuals.csv'))
rcp <- rcp[is.na(overlapping_special_election) | overlapping_special_election %in% c('NA', '', 'FALSE', FALSE)]
rcp[, st := state.abb[match(tolower(gsub('-', ' ', state)), tolower(state.name))]]
sp <- rcp[!is.na(st) & !is.na(dem) & !is.na(rep), .(cycle = year, distcyc = paste(year, st, 'S', sep = '_'),
          end_date = as.IDate(gsub('/', '-', end_date)), sample_size, dem_pct = dem, rep_pct = rep, sponsor = 'N')]
cur <- function(f) { g <- sub('\\.csv$', '_current.csv', f); if(file.exists(g)) g else asof.file(f) }   ## NYT-merged files (data/polls/scripts/merge_nyt_2026.R)
p26 <- fread(cur(file.path(PROJ, 'data/polls/senate_polls_2026.csv')))
p26 <- p26[, .(cycle, distcyc, end_date = as.IDate(end_date), sample_size, dem_pct, rep_pct,
               sponsor = fifelse(partisan_sponsor %in% c('D', 'R'), partisan_sponsor, 'N'))]
i26.file <- cur(file.path(PROJ, 'data/polls/senate_polls_2026_independents.csv'))
if(file.exists(i26.file)){
    i26 <- fread(i26.file)
    ## the independent's share in the anti-R slot. Three-way (MT) is plurality,
    ## so only independent vs Republican matters; the Democrat's share is dropped.
    i26[, anti := as.numeric(ind_pct)]
    p26 <- rbind(p26[!distcyc %in% i26$distcyc], i26[, .(cycle, distcyc, end_date = as.IDate(end_date), sample_size, dem_pct = anti, rep_pct,
                              sponsor = fifelse(partisan_sponsor %in% c('D', 'R'), partisan_sponsor, 'N'))])
}
## combined history (538 raw_polls + 538 polls page + RCP, deduplicated, with sponsor flags;
## data/polls/senate_polls_historical_combined.csv) replaces the RCP-only history
comb.file <- file.path(PROJ, 'data/polls/senate_polls_historical_combined.csv')
if(Sys.getenv('V3S_POLLS', 'combined') == 'combined' && file.exists(comb.file)){
    ## specials are keyed _S2 when the state also held its regular race, else _S, as in X
    cb <- fread(comb.file)
    sp <- cb[cycle <= 2024 & grepl('_S2?$', distcyc), .(cycle, distcyc, end_date = as.IDate(end_date), sample_size, dem_pct, rep_pct,
                                                     sponsor = fifelse(partisan_sponsor %in% c('D', 'R'), partisan_sponsor, 'N'))]
}
spolls <- rbind(sp, p26)
spolls[, `:=`(sample_size = suppressWarnings(as.numeric(gsub('[^0-9.]', '', sample_size))),
              dem_pct = as.numeric(dem_pct), rep_pct = as.numeric(rep_pct))]
spolls <- spolls[!is.na(dem_pct) & !is.na(rep_pct) & !is.na(end_date)]
spolls[, `:=`(p2 = dem_pct / (dem_pct + rep_pct) - .5,
              days = as.numeric(poll.ref(cycle) - end_date),
              n = fifelse(is.na(sample_size), 600, pmin(sample_size, 1500)))]
spolls <- spolls[days >= 0 & days <= 365]
## sponsor bias: from the House polls (the RCP-only Senate history had no flags)
s.bias <- sponsor.bias(FORECAST)
## Senate sponsor bias from Senate history (cycles < T, polls within 60 days of the election)
## V3S_BIAS: sponsor bias of partisan polls relative to nonpartisan ones, from earlier cycles only. 'senate' (default
## since 2026-10-21): Senate polls (R-tagged -1.7 pts pooled, ~0 vs results since 2018); 'house': House polls, mostly
## campaign internals (R -2.9), which over-corrected R-leaning public pollsters (SC 2026: raw polls R+3, average D+0.6).
## Before 2026-10-21 the 'house' value was computed once from all cycles before 2026 and applied to every backtest cycle.
s.bias.T <- function(T){
    if(Sys.getenv('V3S_BIAS', 'senate') != 'senate') return(sponsor.bias(T))
    e <- spolls[cycle < T & days <= 60][, y := S$y[match(distcyc, S$rn)]][!is.na(y), .(err = mean(p2 - y), n = .N), by = sponsor]
    if(!all(c('N', 'D', 'R') %in% e$sponsor)) return(sponsor.bias(T))
    setNames(e$err - e$err[e$sponsor == 'N'], e$sponsor)[c('D', 'R', 'N')]
}
s.avg <- function(cyc, hl){
    b <- s.bias.T(cyc)
    d <- spolls[cycle == cyc][, adj := p2 - b[sponsor]][, w := 0.5^(days / hl) * sqrt(n / 600) * fifelse(sponsor == 'N', 1, PARTISAN_W)]
    d[, .(poll = sum(w * adj) / sum(w), n.eff = sum(w)), by = distcyc]
}
SGRID <- CJ(hl = c(14, 30, 60, 120), k = c(.25, .5, 1, 2, 4), r0 = 0)
## V3S_FLOOR: poll weight b = 1 / (1 + r0 + k / n.eff), i.e. the poll average's error variance has a floor (r0, relative
## to the fundamentals' variance) that more polls do not shrink. r0 = 0 is the production form n.eff / (n.eff + k).
## Motivation (2026-10-21): production gives polls .91-.98 of the weight in every cycle, while the hindsight-best
## constant weight fell from .66-.95 (2010-16) to .55 (2018) and .39 (2022) as the fundamentals improved and their
## errors stopped correlating with the polls' (r .4-.5 -> ~0). Adopted 2026-10-21 with V3S_TUNE_N=4 (user's decision):
## 2018-24 .0359/9/.0522 -> .0324/10/.0452; 2026 poll weight ~.46 instead of .84-.97. V3S_FLOOR=0 for the old form.
if(Sys.getenv('V3S_FLOOR', '1') == '1') SGRID <- CJ(hl = c(14, 30, 60, 120), k = c(.25, .5, 1, 2, 4, 8, 16), r0 = c(0, .25, .5, 1, 2))
## V3S_TUNE_N: tune the blend on the last N earlier cycles only (0 = all earlier cycles)
S_TUNE_N <- as.integer(Sys.getenv('V3S_TUNE_N', '4'))
sblend <- function(p, pa, k, r0) { b <- 1 / (1 + r0 + k / pa$n.eff); fifelse(is.na(pa$poll), p, p + b * (pa$poll - p)) }
## V3S_POLLFEAT: Senate polls as residual-model features, as the House V3_POLLFEAT (as-of average, 30-day
## half-life); the blend may then switch off (k = 1e6). V3S_BLEND_OFF: polls only through the model.
S_POLLFEAT <- Sys.getenv('V3S_POLLFEAT', '0') == '1'
if(S_POLLFEAT){
    sf.all <- rbindlist(lapply(sort(unique(spolls$cycle)), function(c) s.avg(c, 30)))
    S[, `:=`(poll.x = sf.all$poll[match(rn, sf.all$distcyc)], neff.x = sf.all$n.eff[match(rn, sf.all$distcyc)])]
    SGRID <- unique(rbind(SGRID, copy(SGRID)[, `:=`(k = 1e6, r0 = 0)]))
    if(Sys.getenv('V3S_BLEND_OFF', '0') == '1') SGRID <- SGRID[k == 1e6]
}
s.tune <- function(oos, cycles) SGRID[, .(sse = sum(sapply(cycles, function(c){
    r <- oos[cycle == c]; pa <- s.avg(c, hl)[match(r$rn, distcyc)]; sum((sblend(r$pf, pa, k, r0) - r$y)^2) }))), by = .(hl, k, r0)][which.min(sse)]

if(sys.nframe() == 0){
    oos <- rbindlist(lapply(c(seq(2006, 2024, 2), FORECAST), function(T){
        te <- S[(if(T == FORECAST) contested26 else scored) & cycle == T]
        te[, pf := fit.senate(T, te)]; te
    }))
    oos[, `:=`(pp = pf, has.poll = FALSE, prob = NA_real_, poll = NA_real_)]
    ## systematic Senate poll error: mean (poll average - result) over polled races in the last
    ## PBIAS_N earlier cycles, times PBIAS (0 = off), subtracted from every poll average before the blend
    PBIAS <- as.numeric(Sys.getenv('V3S_PBIAS', 0)); PBIAS_N <- as.numeric(Sys.getenv('V3S_PBIAS_N', 3))
    poll.bias <- function(T){
        e <- oos[cycle < T & has.poll & !is.na(y), .(err = mean(poll - y)), by = cycle][order(cycle)]
        if(PBIAS == 0 || !nrow(e)) 0 else PBIAS * mean(tail(e$err, PBIAS_N))
    }
    for(T in c(BT_S, FORECAST)){
        tc <- seq(2006, T - 2, 2); if(S_TUNE_N > 0) tc <- tail(tc, S_TUNE_N)
        bp <- s.tune(oos, tc)
        pa <- s.avg(T, bp$hl); i <- which(oos$cycle == T); k <- match(oos$rn[i], pa$distcyc)
        pb <- poll.bias(T); pk <- pa[k][, poll := poll - pb]
        if(T == FORECAST && PBIAS > 0) cat(sprintf('Senate poll bias correction for 2026: %+.3f (polls shifted toward R)\n', pb))
        oos[i, `:=`(pp = sblend(pf, pk, bp$k, bp$r0), r0 = bp$r0, has.poll = !is.na(k), poll = pa$poll[k], n.eff = pa$n.eff[k], hl = bp$hl, kk = bp$k, pbias = pb)]
    }
    ## district error-scale multiplier re-chosen each cycle to minimise the Brier score on earlier Senate cycles
    ## (normal approximation, each past cycle with its own scales). The Senate had inherited the House's
    ## district scale; out of sample it always wants narrower bands (adopted 2026-10-20: Brier .0511 -> .0496,
    ## 2018-24, with Nokken-Poole). V3S_SCALE_ROLL=0 for the inherited fixed scale.
    S.ROLL <- Sys.getenv('V3S_SCALE_ROLL', '1') == '1'; ssc <- list()
    sd.s <- function(d, sc){ sdi <- fifelse(d$has.poll, sc$d.poll, sc$d.nopoll)
        if(!is.null(sc$hfit)) sdi <- sdi * exp(predict(sc$hfit$m, sc$hfit$hx(d)) - sc$hfit$mu) / sc$hfit$norm; sdi }
    for(T in c(BT_S, FORECAST)){
        ## error scales from blended cycles only: 2006/08 never get the poll blend (pp = pf, has.poll FALSE), so their
        ## fundamentals-only errors inflated the national and unpolled scales (2026 national SD .031 vs .026)
        ## (from the 2018 backtest on, when 2010-16 hold enough unpolled races for that scale; before that the old window)
        sc <- err.scales(oos, if(T >= 2018) seq(2010, T - 2, 2) else seq(2006, T - 2, 2)); i <- which(oos$cycle == T); ssc[[as.character(T)]] <- sc
        if(S.ROLL){ past <- BT_S[BT_S < T]
            if(length(past) >= 2){ g <- seq(.3, 1.3, .05)
                br <- sapply(g, function(m) sum(sapply(past, function(c){ d <- oos[cycle == c & !is.na(y)]; s0 <- ssc[[as.character(c)]]
                    p <- pnorm(d$pp / sqrt(s0$nat^2 + (m * sd.s(d, s0))^2)); sum((p - (d$y > 0))^2) })))
                m <- g[which.min(br)]; sc$d.poll <- sc$d.poll * m; sc$d.nopoll <- sc$d.nopoll * m
                cat(sprintf('Senate %d: district error-scale multiplier %.2f\n', T, m))
                if(T == FORECAST) cat(sprintf('Senate district error-scale multiplier for 2026: %.2f (effective scale %.2f)\n', m, m * SCALE_D)) } }
        oos[i, prob := simulate(oos[i], sc)$prob]
        if(T == FORECAST) { sims26 <- simulate(oos[i], sc, nat.z = nat.z.2026()); oos[i, prob := sims26$prob] }
    }
    bt <- oos[cycle %in% BT_S]
    sm <- bt[, .(n = .N, rmse.fund = sqrt(mean((pf - y)^2)), rmse = sqrt(mean((pp - y)^2)),
                 miss.fund = sum(sign(pf) != sign(y)), miss = sum(sign(pp) != sign(y)), polled = sum(has.poll),
                 brier = mean((prob - (y > 0))^2), hl = hl[1], k = kk[1], r0 = r0[1]), by = cycle]
    ## uncontested races (no major-party opponent): certain calls, counted in the all-race accuracy
    unc <- S[cycle %in% BT_S & !(rn %in% bt$rn) & (uncontested.rep %in% 1 | uncontested.dem %in% 1), .(unc = .N), by = cycle]
    sm[unc, on = 'cycle', unc := i.unc]; sm[is.na(unc), unc := 0L]
    sm[, `:=`(n.all = n + unc, acc.all = 1 - miss / (n + unc), brier.all = brier * n / (n + unc))]
    print(sm)
    r <- bt[cycle >= 2018, .(rmse = sqrt(mean((pp - y)^2)), misses = sum(sign(pp) != sign(y)), brier = mean((prob - (y > 0))^2))]
    cat(sprintf('SENATE_RECENT2018_24 rmse=%.4f misses=%d brier=%.4f (n=%d)\n', r$rmse, r$misses, r$brier, nrow(bt[cycle >= 2018])))
    fwrite(sm, file.path(OUT, 'senate_backtest_by_cycle.csv'))
    log.score('Senate', bt, 'ridge+rf residual; env from House generic-ballot model; RCP+Wikipedia polls')
    print(bt[, .(n = .N, mean.prob = round(mean(prob), 3), dem.won = round(mean(y > 0), 3)),
             by = .(bin = cut(prob, c(0, .05, .2, .4, .6, .8, .95, 1), include.lowest = TRUE))][order(bin)])

    ## 2026: seats not up are held over; D needs 51 (the VP is a Republican)
    f <- merge(S[cycle == FORECAST], oos[cycle == FORECAST, .(rn, pf, pp, prob, poll, n.eff)], by = 'rn', all.x = TRUE)
    f[, anti.r := fifelse(rn %in% ind26$rn, paste(ind26$ind[match(rn, ind26$rn)], '(I, Dem-backed)'), dem.name)]
    nom <- fread(file.path(PROJ, 'data/2026/senate_nominees_2026.csv'))
    d.up <- sum(nom$incumbent_party %in% c('D', 'I', 'Ind'))
    holdover.d <- 47 - d.up
    seats <- holdover.d + sims26$seats
    out <- f[, .(distcyc = rn, anti.r, rep.name, inc.status, state.pres = dist.pres.vs, fund.dvs = round(.5 + pf, 4),
                 poll.dvs = round(.5 + poll, 4), pred.dvs = round(.5 + pp, 4), p.anti.r = round(prob, 3),
                 rating = cut(prob, c(-Inf, .05, .2, .4, .6, .8, .95, Inf),
                              c('Safe R', 'Likely R', 'Lean R', 'Toss-up', 'Lean D', 'Likely D', 'Safe D')), forecast.note)][order(-p.anti.r)]
    fwrite(out, file.path(OUT, 'senate_2026_forecast.csv'))
    q <- quantile(seats, c(.05, .5, .95))
    q <- round(q)   ## the median can fall between two integers
    cat(sprintf('\n2026 Senate: %d D/independent seats not up; median D+I seats %d (90%%: %d-%d); P(51+) = %.2f\n',
                holdover.d, q[2], q[1], q[3], mean(seats >= 51)))
    print(out[, .(distcyc, anti.r, rep.name, fund.dvs, poll.dvs, pred.dvs, p.anti.r)])
    saveRDS(list(oos = oos, seats = seats), file.path(OUT, 'senate_oos.rds'))
    ## joint outcomes: both chambers simulated with the same national draws
    hs <- file.path(OUT, 'house_2026_seat_sims.rds')
    if(file.exists(hs)){
        h <- readRDS(hs)
        if(length(h) == length(seats))
            cat(sprintf('Joint (shared national error): P(D House & Senate) = %.2f; P(D House, R Senate) = %.2f; P(R both) = %.2f\n',
                        mean(h >= 218 & seats >= 51), mean(h >= 218 & seats < 51), mean(h < 218 & seats < 51)))
    }
}
