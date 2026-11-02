###############################################################################
## forecast_governor_v3.R: governor forecast with a rolling-origin backtest
##
## Same structure as the Senate model: y - .5 = a*lean + b*inc + c*env, then a
## ridge + random forest residual model, then a poll blend tuned on earlier
## cycles. env is the House national cycle effect (nat.env). No money in v1:
## state disclosure coverage at the cutoff is uneven and untested.
## in:  cp_proj_refactor combined state results (governor generals, with rids),
##      cp_proj_refactor federal_president_state.csv (statewide presidential vote),
##      data/ballot_context/governor_races.csv (2012-2026 nominees, incumbents, ratings),
##      538 governor polls (raw_polls 1998-2022, polls page 2018-2024),
##      data/polls/governor_polls_2026_oct18.csv
## out: output_v3/governor_*
###############################################################################
source(file.path(Sys.getenv('SML_PROJ', '/workspace/sml_election_forecast'), 'forecast_senate_v3.R'))   ## House + Senate data, helpers, nat.env, perf.s
BT_G <- seq(2010, 2024, 2)
## governor error scales (Senate/House defaults were too wide: 2010-24 Brier .0750 at nat 1.5 / district .8,
## .0721 at 1.0 / .6; lower values gained little and overcorrected the middle bins)
SCALE_D   <- as.numeric(Sys.getenv('V3G_SCALE_D', .6))
NAT_SCALE <- as.numeric(Sys.getenv('V3G_NAT_SCALE', 1))
## training window for both stages, cycles >= min(G_MIN_TRAIN, T - 8); 0 = all cycles
G_MIN_TRAIN <- as.numeric(Sys.getenv('V3G_MIN_TRAIN', 0))
CP_DATA <- CPD
source(file.path(PROJ, 'src/v2_utils.R'))                         ## apply.rid.map
rid.map <- readRDS(file.path(PROJ, 'data/v2/rid_map.rds'))

## statewide presidential two-party vote by election year
pv <- fread(file.path(CP_DATA, 'fec_election_results/_derived/federal_president_state.csv'))[stage == 'general' & party %in% c('D', 'R')]
pv <- dcast(pv[, .(votes = sum(votes)), by = .(cycle, state_po, party)], cycle + state_po ~ party, value.var = 'votes')
pv[, pres := D / (D + R)]

###############################################################################
## Past governor generals: regular even-year November races. Off-year races
## (VA, NJ, KY, LA, MS) are filed under the next even cycle and have a different
## national environment, so they are dropped (election_year != cycle).
###############################################################################
## surname key: 'BRANSTAD, TERRY E.' / 'Terry Branstad' / 'Branstad' -> 'branstad'
sur <- function(x){ x <- tolower(iconv(x, 'UTF-8', 'ASCII//TRANSLIT'))
    x <- fifelse(grepl(',', x), sub(',.*', '', x), sub(',? +(jr|sr|ii|iii|iv)\\.?$', '', x))
    gsub('[^a-z]', '', sub('.* ', '', trimws(x))) }
## cfdb.cand_db governor candidates (read-only, cached in SCRATCH): party, incumbency and rid by
## cycle-state-surname. The SoS results often leave party, rid and incumbency blank.
cdb.f <- file.path(SCRATCH, 'gov_cand_db.rds')
if(!file.exists(cdb.f)){
    con <- cfdb.con()
    saveRDS(as.data.table(DBI::dbGetQuery(con, "select cycle, upper(state) state, lname, Name, party, Incum_Chall ico, bonica_rid
        from cand_db where seat = 'state:governor' and cycle between 1990 and 2026")), cdb.f)
    DBI::dbDisconnect(con)
}
cdb <- readRDS(cdb.f)[, `:=`(cycle = as.integer(cycle), Name = trimws(gsub('\\(.*\\)', '', Name)))]
cdb[, `:=`(sur = sur(Name), party = fcase(tolower(party) %in% c('100', 'democrat', 'democratic', 'dem'), '100',
                                          tolower(party) %in% c('200', 'republican', 'rep'), '200', default = party))]
cdb[bonica_rid == '', bonica_rid := NA_character_]
cdb <- cdb[order(-(ico == 'I'))][!duplicated(paste(cycle, state, sur, party))]
cdb.key <- function(cy, st, s) match(paste(cy, st, s), cdb[, paste(cycle, state, sur)])

## nominees, incumbency and ratings 2012-2026 from governor_races.csv (Wikipedia)
gb <- fread(file.path(PROJ, 'data/ballot_context/governor_races.csv'))[special == FALSE]
gb[, state := state.abb[match(state, state.name)]]
gb[, `:=`(inc.wiki = fcase(incumbent_running & incumbent_party == 'D', 1, incumbent_running & incumbent_party == 'R', -1, default = 0),
          rating = rating_num / 3)]

gr0 <- fread(file.path(CP_DATA, 'state_import/_combined/all_election_results_combined.csv'), colClasses = list(character = 'bonica_rid'))[
    seat == 'state:governor' & stage == 'general' & !special & as.integer(substr(election_year, 1, 4)) == cycle & cycle < FORECAST]
gr0[, `:=`(state = toupper(state), s = sur(candidate_name))]
k <- gr0[, cdb.key(cycle, state, s)]
gr0[, pty := fcase(party == '100', 'D', party == '200', 'R', default = NA_character_)]
gr0[, cpty := cdb$party[k]]
gr0[is.na(pty), pty := fcase(cpty == '100', 'D', cpty == '200', 'R', default = 'O')]
## Wikipedia nominees decide the D and R slots where listed (e.g. VT 2022 Siegel ran as Dem/Prog)
wk <- match(paste(gr0$cycle, gr0$state), paste(gb$cycle, gb$state))
gr0[!is.na(wk) & s == sur(gb$dem_name[wk]), pty := 'D']
gr0[!is.na(wk) & s == sur(sub(';.*', '', gb$rep_name[wk])), pty := 'R']
gr0[bonica_rid == '', bonica_rid := NA_character_]
gr0[, rid := apply.rid.map(fcoalesce(bonica_rid, cdb$bonica_rid[k]), rid.map)]
gr0[, is.inc := incumbent == 'I' | cdb$ico[k] %in% 'I']
top <- function(d, p) d[pty == p][order(-votes)][1]
G <- gr0[, {
    dd <- top(.SD, 'D'); rr <- top(.SD, 'R'); tot <- sum(votes, na.rm = TRUE)
    .(dem.name = dd$candidate_name, rep.name = rr$candidate_name, dem.rid = dd$rid, rep.rid = rr$rid,
      dem.votes = dd$votes, rep.votes = rr$votes, dem.inc = dd$is.inc, rep.inc = rr$is.inc,
      third = max(c(0, votes[pty == 'O' | (pty == 'D' & candidate_name != dd$candidate_name) |
                             (pty == 'R' & candidate_name != rr$candidate_name)]), na.rm = TRUE) / tot)
}, by = .(cycle, state)]
G[, dem.cand.vs := dem.votes / (dem.votes + rep.votes)]
G <- merge(G, gb[cycle < FORECAST, .(cycle, state, inc.wiki, rating)], by = c('cycle', 'state'), all.x = TRUE)
G[, inc := fcoalesce(inc.wiki, fcase(dem.inc %in% TRUE, 1, rep.inc %in% TRUE, -1, default = 0))]
## V3G_RATE0810 (default on, adopted 2026-10-23): 2008 and 2010 ratings from the same Oct-9 Wikipedia revisions
## (data/ballot_context/scripts/14_gov_ratings_2008_2010.py; 2010 Cook/Inside/Sabato, 2008 Sabato only). Earlier
## cycles have no source: no ratings in the 2006 Oct-9 revision, no article by Oct 9 for 1994-2004.
if(Sys.getenv('V3G_RATE0810', '1') == '1'){
    r0 <- fread(file.path(PROJ, 'data/ballot_context/governor_ratings_2008_2010.csv'))[rater %in% c('cook', 'inside', 'sabato'), .(r0 = mean(rating_num) / 3), by = .(cycle, state)]
    G[r0, on = .(cycle, state), rating := fcoalesce(rating, i.r0)]
}

###############################################################################
## 2026 rows from governor_races.csv; rids from the 2026 primary returns
###############################################################################
g26 <- gb[cycle == FORECAST]
## Alaska lists every Republican top-four finalist; the first is used as the R nominee
g26[, rep.name := trimws(sub(';.*', '', rep_name))]
## rid: the latest House, Senate or governor race in the state (same person across offices; this is
## the rid with race history), else the 2026 governor cand_db row; matched on state, party, surname, initial
## first initial too, so relatives and namesakes don't match (Hannah vs Chellie Pingree, Gina vs Ruben Hinojosa)
ini <- function(x){ x <- tolower(iconv(x, 'UTF-8', 'ASCII//TRANSLIT')); substr(trimws(fifelse(grepl(',', x), sub('^[^,]*, *', '', x), x)), 1, 1) }
fed <- rbind(rbind(H, S, fill = TRUE)[, .(cycle, state, s = sur(dem.name), i = ini(dem.name), p = 'D', rid = dem.rid.static)],
             rbind(H, S, fill = TRUE)[, .(cycle, state, s = sur(rep.name), i = ini(rep.name), p = 'R', rid = rep.rid.static)],
             G[, .(cycle, state, s = sur(dem.name), i = ini(dem.name), p = 'D', rid = dem.rid)],
             G[, .(cycle, state, s = sur(rep.name), i = ini(rep.name), p = 'R', rid = rep.rid)])[!is.na(rid)][order(-cycle)]
rid26 <- function(st, nm, p){
    k <- match(paste(FORECAST, st, sur(nm), ini(nm), c(D = '100', R = '200')[p]), cdb[, paste(cycle, state, sur, ini(Name), party)])
    apply.rid.map(fcoalesce(fed$rid[match(paste(st, sur(nm), ini(nm), p), fed[, paste(state, s, i, p)])], cdb$bonica_rid[k]), rid.map)
}
g26 <- g26[, .(cycle, state, dem.name = dem_name, rep.name, dem.rid = rid26(state, dem_name, 'D'), rep.rid = rid26(state, rep.name, 'R'),
               inc = inc.wiki, rating, dem.cand.vs = NA_real_, third = NA_real_, notes)]
G <- rbind(G, g26, fill = TRUE)

G[, rn := paste(cycle, state, 'G', sep = '_')]
G <- qa.apply(G, 'governor')
G[, ref := cycle - fifelse(cycle %% 4 == 0, 4, 2)]
G[, lean := pv$pres[match(paste(ref, state), paste(pv$cycle, pv$state_po))] - nat.pres[as.character(ref)]]
G[, y := dem.cand.vs - .5]
## strong third candidates make D-vs-R share meaningless (e.g. CO 2010 Tancredo): not scored
G[, scored := cycle < FORECAST & !is.na(dem.name) & !is.na(rep.name) & !is.na(dem.cand.vs) & dem.cand.vs > .05 & dem.cand.vs < .95 &
               third < .15 & !is.na(lean)]
G[, contested26 := cycle == FORECAST & !is.na(dem.name) & !is.na(rep.name)]

## candidate's prior overperformance: House, Senate or governor race, most recent before the cycle
G[, cyc.mean := mean((y - lean)[scored]), by = cycle]
G[, resid0 := fifelse(scored, y - lean - cyc.mean, NA_real_)]
perf.g <- rbind(perf.s, G[!is.na(dem.rid) & !is.na(resid0), .(rid = dem.rid, cycle, over = resid0)],
                G[!is.na(rep.rid) & !is.na(resid0), .(rid = rep.rid, cycle, over = -resid0)])
perf.g <- perf.g[, .(over = mean(over)), by = .(rid, cycle)]; setkey(perf.g, rid, cycle)
prev.g <- function(r, cy) perf.g[data.table(rid = r, cycle = cy - 1), over, roll = Inf]
G[, `:=`(dem.prev.over = prev.g(dem.rid, cycle), rep.prev.over = prev.g(rep.rid, cycle))]

FG <- c('lean', 'inc', 'rating', 'dem.prev.over', 'rep.prev.over')

fit.gov <- function(T, te){
    ne <- nat.env(T)
    tr <- G[scored & cycle < T & cycle >= min(G_MIN_TRAIN, T - 8)]
    tr[, env := ne$fe$fe[match(cycle, ne$fe$cycle)]]; te <- copy(te)[, env := ne$pred]
    tr <- tr[!is.na(env)]                              ## no House cycle effect before 1996
    m1 <- lm(y ~ 0 + lean + inc + env, tr)
    r <- tr$y - predict(m1, tr)
    Xtr <- as.matrix(tr[, ..FG]); Xte <- as.matrix(te[, ..FG])
    imp <- function(M, ref) { for(j in seq_len(ncol(M))) M[is.na(M[, j]), j] <- fcoalesce(median(ref[, j], na.rm = TRUE), 0); M }
    na.f <- function(M) { z <- is.na(M[, colSums(is.na(Xtr)) > 0, drop = FALSE]) * 1; colnames(z) <- paste0(colnames(z), '.na'); z }
    A <- cbind(imp(Xtr, Xtr), na.f(Xtr)); B <- cbind(imp(Xte, Xtr), na.f(Xte))
    set.seed(1); cv <- glmnet::cv.glmnet(A, r, alpha = 0, nfolds = 5)
    p.ridge <- as.numeric(predict(cv, B, s = 'lambda.min'))
    rf <- ranger::ranger(x = imp(Xtr, Xtr), y = r, num.trees = 500, min.node.size = 10, num.threads = 8, seed = 1)
    predict(m1, te) + (p.ridge + predict(rf, imp(Xte, Xtr))$predictions) / 2
}

###############################################################################
## Governor polls: 538 raw_polls (Gov-G, 1998-2022, last ~60 days) and the 538
## polls page (2018-2024, full year), deduplicated on poll_id; Wikipedia 2026
###############################################################################
H538 <- file.path(PROJ, 'data/polls/historical/538')
sp.code <- function(x) fcase(toupper(x) %in% c('D', 'DEM'), 'D', toupper(x) %in% c('R', 'REP'), 'R', default = 'N')
rp <- fread(file.path(H538, 'pollster_ratings/raw_polls.csv'))[type_simple == 'Gov-G' & cycle %% 2 == 0 & race == paste0(cycle, '_Gov-G_', location) &
                                                              month(as.IDate(electiondate)) == 11]
rp <- rp[, .(poll_id, cycle, state = location, end_date = as.IDate(polldate), sample_size = samplesize, sponsor = sp.code(partisan),
             dem_pct = fifelse(cand1_party == 'DEM', cand1_pct, fifelse(cand2_party == 'DEM', cand2_pct, NA_real_)),
             rep_pct = fifelse(cand1_party == 'REP', cand1_pct, fifelse(cand2_party == 'REP', cand2_pct, NA_real_)))]
pp <- fread(file.path(H538, 'polls_page/governor_polls_historical.csv'))[stage == 'general' & hypothetical %in% c(FALSE, 'false') & cycle %% 2 == 0]
pp[, `:=`(state = state.abb[match(state, state.name)], end_date = as.IDate(end_date, '%m/%d/%y'), ord = match(population, c('lv', 'rv', 'v', 'a')))]
pp <- pp[!is.na(state)]
## one question per poll: likeliest population, then the lowest question id; top D and top R answers
pq <- pp[order(poll_id, ord, question_id)][, .SD[question_id == question_id[1]], by = poll_id]
pq <- pq[, .(cycle = cycle[1], state = state[1], end_date = end_date[1], sample_size = sample_size[1], sponsor = sp.code(partisan[1]),
             dem_pct = suppressWarnings(max(pct[party == 'DEM'])), rep_pct = suppressWarnings(max(pct[party == 'REP']))), by = poll_id]
gpolls <- rbind(pq, rp[!poll_id %in% pq$poll_id])[is.finite(dem_pct) & is.finite(rep_pct)][, poll_id := NULL]
g26f <- file.path(PROJ, 'data/polls/governor_polls_2026_current.csv')   ## NYT-merged (data/polls/scripts/merge_nyt_2026.R)
if(!file.exists(g26f)) g26f <- file.path(PROJ, 'data/polls/governor_polls_2026_oct18.csv')
if(file.exists(g26f)) gpolls <- rbind(gpolls, fread(g26f)[, .(cycle, state, end_date = as.IDate(end_date), sample_size, sponsor = sp.code(partisan_sponsor), dem_pct, rep_pct)])
gpolls[, `:=`(distcyc = paste(cycle, state, 'G', sep = '_'), sample_size = suppressWarnings(as.numeric(sample_size)))]
gpolls[, `:=`(p2 = dem_pct / (dem_pct + rep_pct) - .5, days = as.numeric(poll.ref(cycle) - end_date),
              n = fifelse(is.na(sample_size), 600, pmin(sample_size, 1500)))]
gpolls <- gpolls[days >= 0 & days <= 365]
g.avg <- function(cyc, hl){
    ## sponsor bias: House polls of cycles before cyc (it was all pre-2026 House polls in every backtest cycle)
    d <- gpolls[cycle == cyc][, adj := p2 - sponsor.bias(cyc)[sponsor]][, w := 0.5^(days / hl) * sqrt(n / 600) * fifelse(sponsor == 'N', 1, PARTISAN_W)]
    d[, .(poll = sum(w * adj) / sum(w), n.eff = sum(w)), by = distcyc]
}
## the governor blend keeps the original Senate grid (the Senate's floor form, V3S_FLOOR, is not tested here)
GGRID <- CJ(hl = c(14, 30, 60, 120), k = c(.25, .5, 1, 2, 4))
g.tune <- function(oos, cycles) GGRID[, .(sse = sum(sapply(cycles, function(c){
    r <- oos[cycle == c]; pa <- g.avg(c, hl)[match(r$rn, distcyc)]; sum((blend(r$pf, pa, k) - r$y)^2) }))), by = .(hl, k)][which.min(sse)]

if(sys.nframe() == 0){
    print(gpolls[, .(polls = .N, races = uniqueN(distcyc)), by = cycle][order(cycle)])
    oos <- rbindlist(lapply(c(seq(2002, 2024, 2), FORECAST), function(T){
        te <- G[(if(T == FORECAST) contested26 else scored) & cycle == T]
        te[, pf := fit.gov(T, te)]; te
    }))
    oos[, `:=`(pp = pf, has.poll = FALSE, prob = NA_real_, poll = NA_real_)]
    for(T in c(BT_G, FORECAST)){
        bp <- g.tune(oos, seq(2002, T - 2, 2))
        pa <- g.avg(T, bp$hl); i <- which(oos$cycle == T); k <- match(oos$rn[i], pa$distcyc)
        oos[i, `:=`(pp = blend(pf, pa[k], bp$k), has.poll = !is.na(k), poll = pa$poll[k], n.eff = pa$n.eff[k], hl = bp$hl, kk = bp$k)]
    }
    for(T in c(BT_G, FORECAST)){
        sc <- err.scales(oos, seq(2002, T - 2, 2)); i <- which(oos$cycle == T)
        oos[i, prob := simulate(oos[i], sc)$prob]
        if(T == FORECAST) { sims26 <- simulate(oos[i], sc, nat.z = nat.z.2026()); oos[i, prob := sims26$prob] }
    }
    bt <- oos[cycle %in% BT_G]
    sm <- bt[, .(n = .N, rmse.fund = sqrt(mean((pf - y)^2)), rmse = sqrt(mean((pp - y)^2)),
                 miss.fund = sum(sign(pf) != sign(y)), miss = sum(sign(pp) != sign(y)), polled = sum(has.poll),
                 brier = mean((prob - (y > 0))^2), hl = hl[1], k = kk[1]), by = cycle]
    print(sm)
    fwrite(sm, file.path(OUT, 'governor_backtest_by_cycle.csv'))
    log.score('Governor', bt, sprintf('ridge+rf residual; env from House generic-ballot model; 538+Wikipedia polls; min_train=%s', G_MIN_TRAIN))
    print(bt[, .(n = .N, mean.prob = round(mean(prob), 3), dem.won = round(mean(y > 0), 3)),
             by = .(bin = cut(prob, c(0, .05, .2, .4, .6, .8, .95, 1), include.lowest = TRUE))][order(bin)])

    ## 2026: D wins among the 36 races, and net change against the party holding each seat
    f <- merge(G[cycle == FORECAST, .(rn, state, dem.name, rep.name, inc, lean, rating, notes)],
               oos[cycle == FORECAST, .(rn, pf, pp, prob, poll, n.eff)], by = 'rn', all.x = TRUE)
    f[, held := gb$incumbent_party[match(paste(FORECAST, state), paste(gb$cycle, gb$state))]]
    ## a strong independent makes the D-vs-R share (and so p.dem) beside the point
    caveat26 <- c(RI = 'Ken Block (I) polls up to 35%; p.dem is P(D beats R), not P(D wins)')
    f[, caveat := caveat26[state]]
    out <- f[, .(distcyc = rn, dem.name, rep.name, held, inc, fund.dvs = round(.5 + pf, 4), poll.dvs = round(.5 + poll, 4),
                 pred.dvs = round(.5 + pp, 4), p.dem = round(prob, 3),
                 rating = cut(prob, c(-Inf, .05, .2, .4, .6, .8, .95, Inf),
                              c('Safe R', 'Likely R', 'Lean R', 'Toss-up', 'Lean D', 'Likely D', 'Safe D')), caveat, notes)][order(-p.dem)]
    fwrite(out, file.path(OUT, 'governor_2026_forecast.csv'))
    q <- round(quantile(sims26$seats, c(.05, .5, .95))); d.held <- sum(f$held == 'D', na.rm = TRUE)   ## round: the median can fall between integers
    cat(sprintf('\n2026 governors: D wins %d of %d races (90%%: %d-%d); D holds %d of them now, so median net %+d D; P(D net gain) = %.2f\n',
                q[2], nrow(f), q[1], q[3], d.held, q[2] - d.held, mean(sims26$seats > d.held)))
    print(out[, .(distcyc, dem.name, rep.name, held, fund.dvs, poll.dvs, pred.dvs, p.dem)])
    saveRDS(list(oos = oos, seats = sims26$seats), file.path(OUT, 'governor_oos.rds'))
}
