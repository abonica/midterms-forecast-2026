###############################################################################
## forecast_v3.R: House forecast, two-stage model with a rolling-origin backtest
##
## Stage 1 (national): dvs - .5 = a*lean + b*inc + cycle effect. The cycle
##   effect is predicted from the generic ballot and approval (linear, so it
##   extrapolates to a cycle unlike the training ones).
## Stage 2 (district): gradient boosting on the stage-1 residual using
##   candidate and campaign features.
## Backtest: each cycle T in BT_CYCLES is predicted from cycles < T only.
## in:  data/cong_training_set.rdata
## out: output_v3/
###############################################################################
suppressMessages({library(data.table); library(xgboost)})
## roots (environment overrides let the release bundle run the same code from its own folder)
PROJ <- Sys.getenv('SML_PROJ', '/workspace/sml_election_forecast')
SCR <- Sys.getenv('SML_SCRATCH', '/ssd2/bonica/sml_v2')
CPD <- Sys.getenv('SML_CP_DATA', '/workspace/cp_proj_refactor/data')
OUT  <- Sys.getenv('V3_OUT', file.path(PROJ, 'output_v3')); dir.create(OUT, showWarnings = FALSE)   ## V3_OUT: experiment output dir
BT_CYCLES <- seq(2006, 2024, 2)
FORECAST  <- 2026
## V3_CUTOFF: information cutoff, the same rule in every cycle (default 10-18; 'none' was the default on 2026-10-21 for a few hours)
##  'none'  each past cycle uses everything through its election day: polls through election eve, the
##          final generic ballot, election-eve ratings, money through election day
##          (data/cong_training_set.rdata, built with SML_CUTOFF_MMDD=election). 2026 uses everything
##          available now, treated as if complete. The backtest therefore uses election-eve information.
##  'MM-DD' as-of date in every cycle, e.g. 10-18: polls and ratings through Oct 18, the Oct 1 generic
##          ballot, money through Sep 30 (data/cong_training_set_0930.rdata)
CUTOFF <- Sys.getenv('V3_CUTOFF', '10-18'); NOCUT <- CUTOFF == 'none'
CUTOFF.MD <- Sys.getenv('SML_CUTOFF_MMDD', if(NOCUT) 'election' else '09-30')   ## money cutoff of the training set in use
## forecast date (month-day, same for every cycle): district/Senate polls through this date;
## generic ballot, approval and expert ratings as of this date when those files exist
ASOF <- Sys.getenv('V3_ASOF', CUTOFF)
asof.file <- function(f){                         ## e.g. house_ratings.csv -> house_ratings_oct18.csv (_final with no cutoff)
    if(ASOF == '10-09') return(f)
    g <- sub('(\\.csv)$', paste0('_', c(`10-18` = 'oct18', none = 'final')[[ASOF]], '\\1'), f)
    if(file.exists(g)) g else { warning('no as-of file ', g, '; using ', f); f }
}
## V3_RATINGS_LATEST=1 (with a dated cutoff): 2026 expert ratings from the latest Wikipedia revisions
## (data/expert_ratings/*_ratings_latest.csv, same build as the as-of files); every past cycle keeps the as-of date
RATINGS.LATEST <- Sys.getenv('V3_RATINGS_LATEST', '0') == '1'
rate.file <- function(f){
    d <- fread(asof.file(f)); g <- sub('\\.csv$', '_latest.csv', f)
    if(RATINGS.LATEST && file.exists(g)) d <- rbind(d[cycle != FORECAST], fread(g)[cycle == FORECAST], fill = TRUE)
    d
}
## general election day: the Tuesday after the first Monday of November
election.day <- function(cyc){ d <- as.IDate(paste0(cyc, '-11-02')); d + (2L - as.POSIXlt(d)$wday) %% 7L }
## reference date for poll recency: election day (2026: the NYT fetch date) with no cutoff, else the as-of date
FETCH.DATE <- local({ f <- file.path(PROJ, 'data/polls/generic_ballot_2026_nyt.csv')
    if(file.exists(f)) as.IDate(fread(f)[point == 'latest']$as_of) else as.IDate(Sys.Date()) })
## V3_POLLS_LATEST=1 (with a dated cutoff): 2026 polls run through the fetch date, and the 2026 generic
## ballot is NYT's latest 30-day mean; everything else, and every past cycle, keeps the cutoff
POLLS.LATEST <- Sys.getenv('V3_POLLS_LATEST', '0') == '1'
poll.ref <- function(cyc){
    r <- if(NOCUT) election.day(cyc) else as.IDate(paste0(cyc, '-', POLL_CUTOFF))
    if(NOCUT || POLLS.LATEST) r[cyc == FORECAST] <- FETCH.DATE
    r }
## FEC report summaries (data/2026/*_fec_summaries_2026.csv): the latest report with no cutoff, else the money cutoff
fec.pick <- function(d) if(NOCUT) d[, .SD[as.IDate(period_end) == max(as.IDate(period_end))], by = distcyc] else
    d[as.IDate(period_end) == as.IDate(paste0(FORECAST, '-', CUTOFF.MD))]
USE_DPREV <- Sys.getenv('V3_DPREV', '0') == '1'   ## district's previous House result (experiment flag)
USE_STLEG <- Sys.getenv('V3_STLEG', '0') == '1'   ## prior state office
USE_E78 <- Sys.getenv('V3_E78', '0') == '1'       ## Jul-Aug conduit earmarks
USE_COMP <- Sys.getenv('V3_COMP', '0') == '1'     ## composite ideology scores
## V3_FILL2 (default on, adopted 2026-10-23): fill model inputs that are missing by linkage error rather than by design
## (unmatched incumbents' roll-call scores; 2026 money from FEC report summaries)
FILL2 <- Sys.getenv('V3_FILL2', '1') == '1'
## V3_QA (default on, adopted 2026-10-23): data corrections from the QA review of the House, Senate and governor datasets
## ($SML_SCRATCH/qa_2026-10-23/*/findings.md), listed with reasons in data/qa/qa_fixes_2026-10-23.csv (office, rn,
## column, value; column 'drop' removes the row), plus rules in each script (zero-donor slots, Senate strong third
## candidates, 2026 redrawn-state trend). The 1996 House corrections are held in data/qa/qa_fixes_held_1996.csv (correct,
## but the 2006 environment rests on 1996-2004 and they move it 1.6 pts; see Model_v3_notes.md): V3_QA_FILE to test them
QA <- Sys.getenv('V3_QA', '1') == '1'
qa.apply <- function(D, off, cols = NULL, drop = TRUE){
    if(!QA) return(D)
    f <- fread(Sys.getenv('V3_QA_FILE', file.path(PROJ, 'data/qa/qa_fixes_2026-10-23.csv')), colClasses = 'character')
    f <- f[f$office == off]
    nd <- 0L
    if(drop){ dr <- f[column == 'drop', rn]; nd <- sum(D$rn %in% dr); D <- D[!rn %in% dr] }
    f <- f[column != 'drop' & rn %in% D$rn]
    if(!is.null(cols)) f <- f[column %in% cols]
    for(j in seq_len(nrow(f))){
        v <- f$value[j]; cl <- class(D[[f$column[j]]])[1]
        v <- switch(cl, logical = as.logical(v), integer = as.integer(v), numeric = as.numeric(v), v)
        set(D, which(D$rn == f$rn[j]), f$column[j], v)
    }
    cat(sprintf('QA fixes (%s): %d values, %d rows dropped\n', off, nrow(f), nd))
    D
}
USE_NP <- Sys.getenv('V3_NP', '1') == '1'         ## incumbents' roll-call (Nokken-Poole) score, career to date through the election Congress
STATE_SHIFT <- Sys.getenv('V3_STATE', '0') == '1' ## statewide Senate poll residuals shift House districts
USE_ACS <- Sys.getenv('V3_ACS', '0') == '1'       ## demographic main effects
USE_PB <- Sys.getenv('V3_PB', '0') == '1'         ## lagged precinct down-ballot partisanship
USE_BALLOT <- Sys.getenv('V3_BALLOT', '0') == '1' ## ballot composition: governor/Senate races, measures
USE_PRIM <- Sys.getenv('V3_PRIM', '0') == '1'     ## primary turnout and nominee primary strength
USE_TP <- Sys.getenv('V3_TP', '0') == '1'         ## third-party candidates on the ballot
USE_EL <- Sys.getenv('V3_EL', '0') == '1'         ## district elasticity to the national swing
EL_LINEAR <- Sys.getenv('V3_EL_LINEAR', '0') == '1' ## elasticity in the linear base
USE_EL2 <- Sys.getenv('V3_EL2', '1') == '1'       ## swing elasticity by competitiveness, tuned after the fact
EL2_SAFE <- Sys.getenv('V3_EL2_SAFE', '0') == '1' ## ... applied to safe seats only (competitive seats keep the uniform swing). Default 0 since 2026-10-21: the all-seat version, judged on Brier and seat CRPS, is better (CRPS 2.97 -> 2.71, misses equal, Brier equal); 2026 median 242 -> 245
USE_TREND <- Sys.getenv('V3_TREND', '1') == '1'   ## district presidential trend in midterms (House lags the pres realignment), weight tuned on earlier midterms
TREND_IDIO <- Sys.getenv('V3_TREND_IDIO', '1') == '1'  ## ... only the part of the trend demographics don't explain (within cycle)
TREND_MIN <- as.numeric(Sys.getenv('V3_TREND_MIN', .10))  ## ... applied where |fundamentals margin| >= this (NA: EL2's safe seats)
USE_SPAM <- Sys.getenv('V3_SPAM', '0') == '1'    ## count only money not spent on fundraising (pacwatch fundraising-spending share)
SPAM_SRC <- Sys.getenv('V3_SPAM_SRC', 'asof')      ## asof: pacwatch-coded spending through the cutoff (training set); full: pacwatch full-cycle file
USE_POLSPEND <- Sys.getenv('V3_POLSPEND', '1') == '1'  ## money on the spending side: political spending and fundraising spending (pacwatch-coded, as of the cutoff)
USE_SPAM_FEAT <- Sys.getenv('V3_SPAM_FEAT', '0') == '1'  ## fundraising shares as residual-model features instead
USE_TOP2 <- Sys.getenv('V3_TOP2', '0') == '1'    ## CA/WA top-two primary D share as a district signal (data/top2_primary.csv)
USE_CORR <- Sys.getenv('V3_CORR', '0') == '1'    ## simulation: regional (Census division) and similar-district (demographic) shocks
LA_MODE <- Sys.getenv('V3_LA', 'none')          ## Louisiana outcomes: none | recode (first-round party share) | drop (recode, and drop multi-candidate jungle races)
OPENTRAIT <- Sys.getenv('V3_OPENTRAIT', '1') == '1'
## V3_L2LAYER=A|B|C (experiment 2026-10-27, off): L2 turnout pools as a post-fit layer (data/l2/district_turnout_features.csv,
## src/build_l2_turnout.R; pre-election history only, the post_* columns are never read). A: marginal D-R voters x predicted
## environment; B: presidential-only D-R voters x midterm; C: both. Never residual-tree features (that produced an era artifact).
L2LAYER <- Sys.getenv('V3_L2LAYER', '')   ## open-seat candidate-trait layer (moderation, Hispanic); adopted 2026-10-26 (default on)
USE_INCC <- Sys.getenv('V3_INCC', '1') == '1'    ## incumbency effect that grows with the margin (overrated in close races), tuned after the fact
LEAN3 <- Sys.getenv('V3_LEAN3', '0') == '1'      ## cubic lean term in stage 1 (needs V3_INC_COMP=1)
LEAN_TREND <- Sys.getenv('V3_LEAN_TREND', '0') == '1'  ## stage 1: lean slope with a time trend extrapolated to T (needs V3_INC_COMP=1)
## national-model experiments (2026-10-21)
NAT_SHRINK <- Sys.getenv('V3_NAT_SHRINK', '0') == '1'  ## cycle-effect regression shrunk toward the structural slopes, penalty by leave-one-cycle-out
DRIFT <- Sys.getenv('V3_DRIFT', '0') == '1'            ## Oct 1 -> election-day ballot drift modeled on momentum since Labor Day, shrunk
## wave experiments (2026-10-21)
LOGIT_ENV <- Sys.getenv('V3_LOGIT_ENV', '0') == '1'   ## apply the cycle effect on the log-odds scale (uniform swing in log-odds: competitive seats move most)
INC_ENV <- Sys.getenv('V3_INC_ENV', '0') == '1'       ## stage 1: inc x env x president's party (incumbency insulates less in a wave against the president's party; needs V3_INC_COMP=1)
SIM_EL <- as.numeric(Sys.getenv('V3_SIM_EL', 1))      ## simulation: the national shock scaled by 1 + SIM_EL x (competitiveness / mean - 1). Default 1 since 2026-10-21 by the user's decision (a wave draw moves competitive seats more; backtest CRPS 3.02 vs 2.97)
ENVW <- as.numeric(Sys.getenv('V3_ENVW', 0))          ## boosting weights by environment similarity: exp(-|env_c - env_T| / ENVW), in share units (0 = off)
INC_COMP <- Sys.getenv('V3_INC_COMP', '1') == '1' ## ... the same idea inside stage 1: inc x |lean| estimated jointly with lean. Adopted 2026-10-21 with INCC kept: 2018-24 46 -> 41 misses, Brier .0221 -> .0210
USE_TREND_FEAT <- Sys.getenv('V3_TREND_FEAT', '0') == '1'  ## ... as residual-model features instead (trend, trend x midterm)
TREND_FILE <- Sys.getenv('V3_TREND_FILE', file.path(PROJ, 'data/district_pres_trend.csv'))
EL.LIN <- strsplit(Sys.getenv('V3_EL_VARS', 'abs.lean,pct_hispanic,pct_ba_plus,pct_white_nh_noncollege,log_density'), ',')[[1]]
PDEMO <- Sys.getenv('V3_PDEMO', '1') == '1'       ## current-cycle poll residuals regressed on demographics
USE_PREVOOS <- Sys.getenv('V3_PREVOOS', '0') == '1' ## candidate's previous out-of-sample model error
USE_TRUMP <- Sys.getenv('V3_TRUMP', '0') == '1'     ## Trump-on-ballot term in the national model
SCANDAL <- Sys.getenv('V3_SCANDAL', '0') == '1'      ## dossier scandal adjustment
SCANDAL_SHRINK <- as.numeric(Sys.getenv('V3_SCANDAL_SHRINK', 1))
STACK <- Sys.getenv('V3_STACK', '0') == '1'        ## learned stacking weights
MONO <- Sys.getenv('V3_MONO', '1') == '1'          ## monotone constraints in xgboost
PPART <- Sys.getenv('V3_PPART', '0') == '1'        ## ... plus the partisan share of the poll weight and the gap x that share (2026: half of polled districts have internals only)
POLLFEAT <- Sys.getenv('V3_POLLFEAT', '1') == '1'  ## district polls as residual-model features (gap to the stage-1 base, poll volume); the blend may switch off. Adopted 2026-10-20 by choice: 2 fewer misses, Brier slightly worse
HETERO <- Sys.getenv('V3_HETERO', '1') == '1'      ## district-specific error size
CLASS <- Sys.getenv('V3_CLASS', '0') == '1'        ## logistic stacking of win probabilities
RECAL <- Sys.getenv('V3_RECAL', '0') == '1'        ## shrink predictions with a rolling y ~ pp calibration
RECAL_N <- as.numeric(Sys.getenv('V3_RECAL_N', 5))  ## cycles used for the calibration
set.seed(1)

## alternate training sets (e.g. a different money cutoff) via V3_TRAINSET
TS.FILE <- Sys.getenv('V3_TRAINSET', file.path(PROJ, if(NOCUT) 'data/cong_training_set.rdata' else 'data/cong_training_set_0930.rdata'))
load(TS.FILE)
## the training set's money cutoff must match the cutoff mode (sets built before 2026-10-21 are 09-30)
TS.CUT <- if(is.null(cong.trainset$money.cutoff)) '09-30' else cong.trainset$money.cutoff
if(NOCUT != (TS.CUT == 'election') && Sys.getenv('V3_ALLOW_MISMATCH') != '1')   ## diagnostics only
    stop(sprintf('V3_CUTOFF=%s but %s has money cutoff %s', CUTOFF, TS.FILE, TS.CUT))
H <- as.data.table(cong.trainset$X, keep.rownames = 'rn')[!grepl('_S2?$', rn)]   ## _S2: second Senate race of a state-cycle

###############################################################################
## Features
###############################################################################
## national two-party presidential vote, from the federal results file
nat.pres <- c(`1992` = .5348, `1996` = .5474, `2000` = .5039, `2004` = .4880, `2008` = .5365,
              `2012` = .5205, `2016` = .5103, `2020` = .5221, `2024` = .4937)
H[, ref := cycle - fifelse(cycle %% 4 == 0, 4, 2)]
H[, lean := dist.pres.vs - nat.pres[as.character(ref)]]
## V3_MCLEAN: 2018+ lean from multi-contest partisanship (statewide contests of the previous election, precinct
## returns carried onto this cycle's lines, at the state's reference presidential cut; src/precinct_curve.R).
## Falls back to the presidential lean where missing (before 2018, 2026 redrawn states).
if(Sys.getenv('V3_MCLEAN', '0') == '1'){
    mcp <- fread(file.path(PROJ, 'data/precinct/district_mc_partisanship.csv'))
    H[, mc.share := mcp$mc.share[match(rn, mcp$distcyc)]]
    cat(sprintf('multi-contest lean: %s\n', paste(H[cycle >= 2018, sprintf('%d %.0f%%', cycle, 100 * mean(!is.na(mc.share))), by = cycle]$V1, collapse = ', ')))
    H[!is.na(mc.share), lean := mc.share - nat.pres[as.character(ref)]]
}
H[, inc := fcase(inc.status == 'I', 1, inc.status == 'C', -1, default = 0)]
H[, pres.party := fifelse(dem.pres == 1, 1, -1)]
H[, gb := generic.ballot / 100]
## momentum of the ballot (Oct 1 minus Labor Day) and the final reading, from data/polls/generic_ballot_by_cycle.csv
## (used by V3_DRIFT; the final is NA for 2026)
gbc <- fread(file.path(PROJ, 'data/polls/generic_ballot_by_cycle.csv'))
H[, `:=`(gb.mom = (gbc$gb_dem_margin_oct1 - gbc$gb_dem_margin_labor_day)[match(cycle, gbc$cycle)] / 100,
         gb.final = fifelse(cycle == FORECAST, NA_real_, gbc$gb_dem_margin_final[match(cycle, gbc$cycle)] / 100))]
## final generic ballot with no cutoff (and in the V3_GB_FINAL hindsight experiment)
if(NOCUT || Sys.getenv('V3_GB_FINAL') == '1') H[!is.na(generic.ballot.final), gb := generic.ballot.final / 100]
## 2026: NYT's national polls (data/polls/generic_ballot_2026_nyt.csv, merge_nyt_2026.R): the latest
## 30-day mean with no cutoff, else the Oct 1 value
gbn <- file.path(PROJ, 'data/polls/generic_ballot_2026_nyt.csv')
if(file.exists(gbn) && (NOCUT || Sys.getenv('V3_GB_FINAL') != '1')){
    g26 <- fread(gbn)[point == if(NOCUT || POLLS.LATEST) 'latest' else 'oct1']
    if(nrow(g26) == 1 && !is.na(g26$gb_dem_margin)){ H[cycle == FORECAST, gb := g26$gb_dem_margin / 100]
        cat(sprintf('2026 generic ballot (%s, NYT, %d polls): D%+.1f\n', g26$as_of, g26$n_polls, g26$gb_dem_margin)) }
}
## V3_GB_2026: scenario override of the 2026 generic ballot, D-R margin in points (e.g. 11)
if(Sys.getenv('V3_GB_2026') != ''){ H[cycle == FORECAST, gb := as.numeric(Sys.getenv('V3_GB_2026')) / 100]
    cat(sprintf('2026 generic ballot set by V3_GB_2026: D%+.1f\n', as.numeric(Sys.getenv('V3_GB_2026')))) }
## V3_GB_TREND=plain|full: the 2026 generic ballot from the daily Kalman trend of the national polls
## (data/polls/scripts/gb_trend.R; plain = local level only, full = with house, population and sponsor
## effects) instead of NYT's 30-day mean. Past cycles already use trend aggregates (538). Oct 1 with a
## dated cutoff (forward filter on polls through Oct 1); the last day with no cutoff or V3_POLLS_LATEST.
GB_TREND <- Sys.getenv('V3_GB_TREND', '0')
if(GB_TREND %in% c('plain', 'full') && Sys.getenv('V3_GB_2026') == ''){
    suf <- if(GB_TREND == 'plain') '_plain' else ''
    if(NOCUT || POLLS.LATEST){
        gt <- fread(file.path(PROJ, sprintf('data/polls/gb_trend_daily%s.csv', suf)))[cycle == FORECAST & as.IDate(end) <= FETCH.DATE][.N]
        v <- gt$mu; when <- gt$end
    } else {
        gt <- fread(file.path(PROJ, sprintf('data/polls/gb_trend_by_cycle%s.csv', suf)))[cycle == FORECAST]
        v <- gt$est; when <- gt$as_of
    }
    H[cycle == FORECAST, gb := v / 100]
    cat(sprintf('2026 generic ballot from the %s trend (%s): D%+.2f\n', GB_TREND, when, v))
}
## V3_GB_ORACLE=1 (hindsight demonstration, never for a forecast): every past cycle's generic ballot replaced by the
## actual national House popular vote margin (D - R share of the total vote, Clerk of the House), in training and test
## cycles alike, so the national stage knows the size of the swing; 2026 keeps its ballot. The Senate and governor models
## take their national environment from this House model, so they get it too.
if(Sys.getenv('V3_GB_ORACLE') == '1'){
    nv <- c(`1996` = -.4, `1998` = -1.1, `2000` = -.5, `2002` = -4.8, `2004` = -2.8, `2006` = 8.0, `2008` = 10.6, `2010` = -6.8,
            `2012` = 1.2, `2014` = -5.7, `2016` = -1.1, `2018` = 8.6, `2020` = 3.1, `2022` = -2.8, `2024` = -2.6)
    H[as.character(cycle) %in% names(nv), gb := nv[as.character(cycle)] / 100]
    cat('V3_GB_ORACLE: generic ballot = actual national House vote margin in', length(nv), 'cycles\n')
}
H[, appr := pres.approval.net.dem / 100]
## the national model uses the Oct 1 generic ballot unless V3_GB_ASOF=1: in the backtest, mid-October
## values drifted toward Democrats in several cycles (2004, 2016, 2020) and scored worse
if(ASOF != '10-09' && Sys.getenv('V3_GB_ASOF') == '1'){
    gbf <- asof.file(file.path(PROJ, 'data/polls/generic_ballot.csv'))
    if(file.exists(gbf)){
        g18 <- fread(gbf); k <- match(H$cycle, g18$cycle)
        H[, gb := g18$gb_dem_margin_oct18[k] / 100]
        H[, appr := fifelse(dem.pres == 1, 1, -1) * g18$pres_approval_net_oct18[k] / 100]
    }
}
H[, mid.pen := midterm * pres.party]                ## +1: Dem president in a midterm
## Louisiana: the master file mixes runoff head-to-heads, top-D-vs-top-R first-round shares and
## party totals. recode: D share of the first-round D+R vote everywhere (data/la_house_party_share.csv,
## src/build_la_party_share.R; 2008-10 are ordinary generals)
if(LA_MODE %in% c('recode', 'drop')){
    la <- fread(file.path(PROJ, 'data/la_house_party_share.csv'))[n_D > 0 & n_R > 0]
    k <- match(H$rn, la$distcyc)
    H[!is.na(k), dem.cand.vs := la$dem_share_of_major[k[!is.na(k)]]]
    H[, la.multi := !is.na(k) & la$jungle[k] & (la$n_D[k] > 1 | la$n_R[k] > 1)]
}
## V3_QA: corrected outcomes, incumbency, names and flags (scored overrides are applied below)
H <- qa.apply(H, 'house', cols = setdiff(fread(file.path(PROJ, 'data/qa/qa_fixes_2026-10-23.csv'))[office == 'house', unique(column)], 'scored'))
if(QA) for(s in c('dem', 'rep')){   ## > $100k raised with no individual money and no donors: the individual rows are missing, not zero
    i <- H[, which(fcoalesce(get(paste0('total.', s)), 0) > 1e5 & get(paste0('total.ind.', s)) %in% 0 & get(paste0('num.donors.', s)) %in% 0)]
    for(v in paste0(c('total.ind.', 'num.donors.'), s)) set(H, i, v, NA_real_)
    cat(sprintf('QA: %d %s slots with > $100k raised but no individual money or donors set NA\n', length(i), s))
}
if(L2LAYER != '' && file.exists(file.path(PROJ, 'data/l2/district_turnout_features.csv'))){
    l2 <- fread(file.path(PROJ, 'data/l2/district_turnout_features.csv'))[, .(rn, l2.marg = marg_dmr_k, l2.po = presonly_dmr_k, l2.gap = dob_gap %in% c(TRUE, 'TRUE', 1))]
    H[l2, on = 'rn', `:=`(l2.marg = i.l2.marg, l2.po = i.l2.po, l2.gap = i.l2.gap)]
}
H[, y := dem.cand.vs - .5]
## genuine D-vs-R contests: both nominees named and distinct (drops D-vs-D top-two,
## LA jungle rows with one candidate in both slots), and a two-party share
## inside (.05, .95) (drops write-in-only and mislinked "opponents")
H[, dr := !is.na(dem.name) & !is.na(rep.name) & tolower(dem.name) != tolower(rep.name)]
H[, scored := dr & !is.na(dem.cand.vs) & dem.cand.vs > .05 & dem.cand.vs < .95 & !is.na(lean)]
if(LA_MODE == 'drop') H[(la.multi), scored := FALSE]   ## several candidates of a party split the vote: not a two-candidate contest
H <- qa.apply(H, 'house', cols = 'scored', drop = FALSE)
## 2026: a D-vs-R race on the ballot
H[, contested26 := cycle == FORECAST & !nominee.unresolved & uncontested.dem == 0 & uncontested.rep == 0]

## candidate's prior overperformance: residual of their most recent earlier House
## general, relative to lean and that cycle's mean (matched by rid, so it holds
## across redistricting). Sign is for the candidate's own party.
H[, cyc.mean := mean((y - lean)[scored]), by = cycle]
H[, resid0 := fifelse(scored, y - lean - cyc.mean, NA_real_)]
perf <- rbind(H[!is.na(dem.rid.static) & !is.na(resid0), .(rid = dem.rid.static, cycle, over = resid0)],
              H[!is.na(rep.rid.static) & !is.na(resid0), .(rid = rep.rid.static, cycle, over = -resid0)])
perf <- perf[, .(over = mean(over)), by = .(rid, cycle)]
setkey(perf, rid, cycle)
## most recent race strictly before cyc (rolling join on cycle - 1)
prev.over <- function(r, cy) perf[data.table(rid = r, cycle = cy - 1), over, roll = Inf]
## V3_HIST_K: instead of the most recent race alone, the career mean of prior overperformance shrunk toward 0 by
## K pseudo-races at 0 (sum / (n + K)); a one-race +8 then counts half as much as a four-race +8 (K = 1)
HIST_K <- as.numeric(Sys.getenv('V3_HIST_K', 0))
if(HIST_K > 0){
    perf[, `:=`(cum = cumsum(over), n = seq_len(.N)), by = rid]   ## perf is keyed by rid, cycle
    perf[, sh := cum / (n + HIST_K)]
    prev.over <- function(r, cy) perf[data.table(rid = r, cycle = cy - 1), sh, roll = Inf]
}
H[, dem.prev.over := prev.over(dem.rid.static, cycle)]
H[, rep.prev.over := prev.over(rep.rid.static, cycle)]

## candidate's previous out-of-sample model error (own-party sign): overperformance net
## of what the model already knew (opponent money, environment, polls), from a saved
## reference run whose predictions for each cycle used only earlier cycles
if(USE_PREVOOS && file.exists(file.path(OUT, 'house_oos_reference.rds'))){
    ro <- readRDS(file.path(OUT, 'house_oos_reference.rds'))$oos[, .(rn, cycle, dem.rid.static, rep.rid.static, e = y - pp)]
    po <- rbind(ro[!is.na(dem.rid.static), .(rid = dem.rid.static, cycle, over = e)],
                ro[!is.na(rep.rid.static), .(rid = rep.rid.static, cycle, over = -e)])[, .(over = mean(over)), by = .(rid, cycle)]
    setkey(po, rid, cycle)
    prev.oos <- function(r, cy) po[data.table(rid = r, cycle = cy - 1), over, roll = Inf]
    H[, `:=`(dem.prev.oos = prev.oos(dem.rid.static, cycle), rep.prev.oos = prev.oos(rep.rid.static, cycle))]
}

## district's previous House result relative to lean and that cycle's mean
## (local ticket-splitting), only where the lines did not change since then
redrawn <- list(`2012` = state.abb, `2022` = state.abb, `2002` = state.abb, `1996` = c('GA','LA','TX','FL'),
                `1998` = c('NC','VA','NY'), `2000` = 'NC', `2004` = 'TX', `2006` = c('GA','TX'),
                `2016` = c('FL','NC','VA'), `2018` = 'PA', `2020` = 'NC', `2024` = c('AL','GA','LA','NC','NY'),
                `2026` = c('AL','CA','FL','LA','NC','OH','TN','TX','UT'))
H[, dkey := sub('^\\d{4}_', '', rn)]
H[, same.lines := !mapply(function(cy, st) st %in% redrawn[[as.character(cy)]], cycle, state)]
H[, dist.prev.resid := fifelse(same.lines, resid0[match(paste(cycle - 2, dkey), paste(cycle, dkey))], NA_real_)]

## prior state office: state legislator, or statewide office (governor, AG, ...),
## from cfdb.cand_db state records (incumbent that cycle, or a win in an earlier cycle)
so <- readRDS(file.path(SCR, 'state_office_history.rds'))
so[, held := Incum_Chall %in% 'I' | winner %in% 'W']
so[, type := fifelse(seat %in% c('state:lower', 'state:upper'), 'leg', 'statewide')]
prior.office <- function(r, cy, ty){
    z <- so[type == ty & ((Incum_Chall %in% 'I' & cycle <= max(cy)) | (winner %in% 'W'))]
    k <- z[, .(first = min(cycle[Incum_Chall %in% 'I' | winner %in% 'W'])), by = bonica_rid]
    f <- k$first[match(r, k$bonica_rid)]
    as.numeric(!is.na(f) & f <= cy)
}
H[, `:=`(dem.stleg = prior.office(dem.rid.static, cycle, 'leg'), rep.stleg = prior.office(rep.rid.static, cycle, 'leg'),
         dem.statewide = prior.office(dem.rid.static, cycle, 'statewide'), rep.statewide = prior.office(rep.rid.static, cycle, 'statewide'))]

## 2026 ballot-check fixes (data/2026/ballot_check_2026-10-19.csv):
##  - MI-10: the R nominee is Mike Bouchard Jr.; cfdb merged him into his father's rid, so
##    history-based features (prior races, CFscore) describe the father and are blanked
##  - AK-1: the main anti-R candidate is independent Bill Hill (Alaska Democratic Party-backed),
##    not the D-labeled Eric Hafner; money comes from FEC summaries
##    (data/2026/house_fec_summaries_2026.csv), Hafner's history is dropped
##  - CA-6: Kevin Kiley (no party preference since March 2026; ex-R incumbent who caucuses with
##    Republicans) is modeled as the Republican against Richard Pan (D). Top-two left the R slot
##    empty, so Kiley's rid, CFscore and prior races are filled in here, and both sides' money
##    comes from FEC summaries (same file, through Sep 30) so the D/R ratios share a source
H[rn == '2026_MI_10', c('rep.prev.over', 'rep.ip') := NA_real_]
if('2026_AK_1' %in% H$rn){
    H[rn == '2026_AK_1', `:=`(dem.name = 'Bill Hill (I, Dem-backed)', dem.prev.over = NA_real_, dem.ip = NA_real_,
                              quality.chall.dem = 0, dem.female = 0)]
    hf <- file.path(PROJ, 'data/2026/house_fec_summaries_2026.csv')
    if(file.exists(hf)){
        hq <- fec.pick(fread(hf)[distcyc == '2026_AK_1'])
        a <- hq[party %in% c('I', 'D')][1]; b <- hq[party == 'R'][1]
        if(nrow(a) && nrow(b))
            H[rn == '2026_AK_1', `:=`(total.dem = a$total_receipts, total.rep = b$total_receipts, total.ind.dem = a$indiv_contrib,
                                      total.ind.rep = b$indiv_contrib, total.pac.dem = a$pac_contrib, total.pac.rep = b$pac_contrib,
                                      spare.cash.dem = a$cash_on_hand, spare.cash.rep = b$cash_on_hand,
                                      total.disbursements.dem = a$disbursements, total.disbursements.rep = b$disbursements,
                                      num.donors.dem = NA_real_, dem.q6 = NA_real_, total.cc.dem = 0)]
    }
}
if('2026_CA_6' %in% H$rn){
    kr <- 'cand131342'                                   ## Kevin Kiley (CA-3 2022, 2024)
    hf <- file.path(PROJ, 'data/2026/house_fec_summaries_2026.csv')
    hq <- if(file.exists(hf)) fec.pick(fread(hf)[distcyc == '2026_CA_6']) else data.table(party = character(), total_receipts = numeric())
    a <- hq[party == 'D'][1]; b <- hq[party == 'I'][1]
    ## plain name here so the Nokken-Poole surname match finds Kiley; the output CSV adds the label
    H[rn == '2026_CA_6', `:=`(rep.name = 'Kevin Kiley', rep.rid.static = kr,
                              rep.ip = H[rep.rid.static == kr & cycle < FORECAST][order(-cycle)]$rep.ip[1],
                              rep.prev.over = prev.over(kr, FORECAST), rep.female = 0,
                              rep.stleg = prior.office(kr, FORECAST, 'leg'), rep.statewide = prior.office(kr, FORECAST, 'statewide'),
                              nominee.unresolved = FALSE, uncontested.rep = 0, contested26 = TRUE)]
    ## no FEC rows for this cutoff (e.g. a 06-30 experiment): money is missing, not mixed-source
    if(!nrow(a) || !nrow(b) || anyNA(c(a$total_receipts, b$total_receipts))){
        warning('CA-6: no FEC summary rows for Pan and Kiley at ', CUTOFF.MD, '; money features left NA')
        a <- b <- data.table(total_receipts = NA_real_, indiv_contrib = NA_real_, pac_contrib = NA_real_, cash_on_hand = NA_real_, disbursements = NA_real_)
    }
    H[rn == '2026_CA_6', `:=`(total.dem = a$total_receipts, total.rep = b$total_receipts, total.ind.dem = a$indiv_contrib,
                              total.ind.rep = b$indiv_contrib, total.pac.dem = a$pac_contrib, total.pac.rep = b$pac_contrib,
                              spare.cash.dem = a$cash_on_hand, spare.cash.rep = b$cash_on_hand,
                              total.disbursements.dem = a$disbursements, total.disbursements.rep = b$disbursements,
                              num.donors.dem = NA_real_, num.donors.rep = NA_real_, dem.q6 = NA_real_, rep.q6 = NA_real_,
                              total.cc.dem = 0, total.cc.rep = 0)]
}
## V3_FILL2: races whose DIME money is missing or far off (receipts under 25% of the FEC full-cycle total, e.g.
## 2018 D challengers whose conduit money DIME missed; no disbursements beside real receipts; unlinked slots;
## 2026 rows without a Q6 bin) take both sides' money from the FEC report summaries as of Sep 30, received by
## Oct 18 (src/fetch_fec_fill.py -> data/fec/fec_fill_asof.csv). Both sides, so the D/R ratios share a source;
## spare cash is receipts minus disbursements as in DIME. The undercounted side's donor count is set NA.
if(FILL2 && file.exists(file.path(PROJ, 'data/fec/fec_fill_asof.csv'))){
    ff <- fread(file.path(PROJ, 'data/fec/fec_fill_asof.csv'))[office == 'house' & !is.na(receipts)]
    both <- ff[, .N, by = rn][N == 2, rn]
    for(s in c('dem', 'rep')){
        a <- ff[side == s & rn %in% both]; i <- match(a$rn, H$rn); ok <- !is.na(i); a <- a[ok]; i <- i[ok]
        low <- which(fcoalesce(H[[paste0('total.', s)]][i], 0) < .25 * fcoalesce(H[[paste0('fullcycle.total_receipts.', s)]][i], 0))
        set(H, i[low], paste0('num.donors.', s), NA_real_)
        set(H, i, paste0('total.', s), a$receipts); set(H, i, paste0('total.ind.', s), a$indiv)
        set(H, i, paste0('total.pac.', s), a$pac); set(H, i, paste0('total.cc.', s), a$cand)
        set(H, i, paste0('total.disbursements.', s), a$disb); set(H, i, paste0('spare.cash.', s), a$receipts - a$disb)
        set(H, i, paste0(s, '.q6'), a$q6)
    }
    ## donor counts the fill left missing (undercounted sides, and the zero-donor slots: $ raised but no individual rows in
    ## DIME, e.g. 2018 TX-3 Van Taylor) are imputed from the FEC individual money at the cycle and party's median DIME
    ## dollars per donor, so a funded candidate does not read as one with no donors (NA reads as non-viable to the trees)
    nimp <- 0L
    for(s in c('dem', 'rep')){
        nd <- paste0('num.donors.', s); ti <- paste0('total.ind.', s)
        med <- H[!(rn %in% both) & get(nd) > 0 & get(ti) > 1e5, .(r = median(get(ti) / get(nd))), by = cycle]
        i <- H[, which(rn %in% both & is.na(get(nd)) & fcoalesce(get(ti), 0) > 0)]
        set(H, i, nd, H[[ti]][i] / med$r[match(H$cycle[i], med$cycle)]); nimp <- nimp + length(i)
    }
    cat(sprintf('FEC fill: %d House races (%d before %d); donor counts imputed for %d sides\n', length(both), sum(H[rn %in% both, cycle] < FORECAST), FORECAST, nimp))
}
## V3_MONEY_LATEST=1 (2026 only): money through the pre-general report (Oct 14) instead of Sep 30. Each 2026 candidate's
## 12G report (coverage Oct 1-14; src/fetch_fec_late.py -> data/fec/fec_late_2026.csv) adds its receipts, individual, PAC,
## self-funding and spending to the Sep 30 totals, joined on the FEC candidate id; past cycles keep Sep 30. Donor counts
## and the Q6 bin are unchanged.
MONEY.LATEST <- Sys.getenv('V3_MONEY_LATEST', '0') == '1'
fec.late <- if(file.exists(file.path(PROJ, 'data/fec/fec_late_2026.csv'))) fread(file.path(PROJ, 'data/fec/fec_late_2026.csv')) else NULL
## both sides of a race need a pre-general report (else neither moves: a one-sided update would tilt the race)
late.inc <- function(D, s, v) { k <- match(D[[paste0(s, '.fec.cand.id')]], fec.late$cand_id)
    both <- D$dem.fec.cand.id %in% fec.late$cand_id & D$rep.fec.cand.id %in% fec.late$cand_id
    fcoalesce(fec.late[[v]][k], 0) * (D$cycle == FORECAST & both) }
money.latest <- function(D, off){
    if(!MONEY.LATEST || is.null(fec.late)) return(D)
    for(s in c('dem', 'rep')){
        add <- c(total = 'inc_receipts', total.ind = 'inc_indiv', total.pac = 'inc_pac', total.cc = 'inc_cand', total.disbursements = 'inc_disb')
        for(v in names(add)){ col <- paste0(v, '.', s); if(col %in% names(D)) set(D, j = col, value = D[[col]] + late.inc(D, s, add[[v]])) }
        sc <- paste0('spare.cash.', s); if(sc %in% names(D)) set(D, j = sc, value = D[[sc]] + late.inc(D, s, 'inc_receipts') - late.inc(D, s, 'inc_disb'))
    }
    m <- D[cycle == FORECAST, .(d = dem.fec.cand.id %in% fec.late$cand_id, r = rep.fec.cand.id %in% fec.late$cand_id)]
    cat(sprintf('money through Oct 14 (%s): pre-general reports for %d D and %d R nominees (both sides in %d races)\n', off, sum(m$d), sum(m$r), sum(m$d & m$r)))
    D
}
H <- money.latest(H, 'house')

## money: D-R log ratios, centered within cycle (DIME double-counting and
## conduit coverage drift across eras and parties)
## the offset damps ratios between tiny amounts (e.g. $500 vs $5k of individual money)
MONEY_OFFSET <- as.numeric(Sys.getenv('V3_MONEY_OFFSET', 25000))
## hindsight experiment (V3_FULL_MONEY): full-cycle FEC totals in place of the as-of-Sep-30 money
if(Sys.getenv('V3_FULL_MONEY') == '1') for(s in c('dem', 'rep')) {
    fc <- function(v) H[[sprintf('fullcycle.%s.%s', v, s)]]
    H[, (paste0('total.', s)) := fcoalesce(fc('total_receipts'), get(paste0('total.', s)))]
    H[, (paste0('total.ind.', s)) := fcoalesce(fc('total_indiv_contrib'), get(paste0('total.ind.', s)))]
    H[, (paste0('total.pac.', s)) := fcoalesce(fc('total_pac_contribs'), get(paste0('total.pac.', s)))]
    H[, (paste0('total.cc.', s)) := fcoalesce(fc('contribs_from_candidate'), get(paste0('total.cc.', s)))]
    H[, (paste0('total.disbursements.', s)) := fcoalesce(fc('total_disbursements'), get(paste0('total.disbursements.', s)))]
    H[, (paste0('num.donors.', s)) := fcoalesce(as.numeric(fc('num_givers')), as.numeric(get(paste0('num.donors.', s))))]
    H[, (paste0('spare.cash.', s)) := get(paste0('total.', s)) - get(paste0('total.disbursements.', s))]
}
## spam fundraising pipelines: some campaigns spend most of what they raise on list rental and digital
## fundraising shops, so receipts overstate their campaign. Scale receipts and spending by the share not
## spent on fundraising (pacwatch, full-cycle; unmatched candidates get the cycle median).
if(USE_SPAM || USE_SPAM_FEAT){
    if(SPAM_SRC == 'full'){
        pw <- fread(file.path(PROJ, 'data/pacwatch_fundraising_share.csv'))
        frs <- function(s) { rid <- H[[paste0(s, '.rid.static')]]; x <- pw$fr_share[match(paste(rid, H$cycle), paste(pw$rid, pw$cycle))]
            med <- pw[, median(fr_share), by = cycle]; fcoalesce(x, med$V1[match(H$cycle, med$cycle)], 0) }
    } else {
        ## as of the cutoff: pacwatch-coded fundraising spending / all spending (disbursements_v2.R)
        frs <- function(s) { tot <- H[[paste0(s, '.disb.total')]]; x <- fifelse(tot >= 25000, H[[paste0(s, '.disb.pw.fundraising')]] / tot, NA_real_)
            x <- pmin(pmax(x, 0), 1); med <- tapply(x, H$cycle, median, na.rm = TRUE); fcoalesce(x, as.numeric(med[as.character(H$cycle)]), 0) }
    }
    H[, `:=`(fr.dem = frs('dem'), fr.rep = frs('rep'))]
}
if(USE_SPAM){
    for(s in c('dem', 'rep')) for(v in c('total.%s', 'total.ind.%s', 'total.disbursements.%s', '%s.q6')) {
        col <- sprintf(v, s); H[, (col) := get(col) * (1 - get(paste0('fr.', s)))] }
}
## V3_MONEY0 (default on, adopted 2026-10-23): a non-incumbent nominee on the ballot with no money records at all (13 of 15 such rows since 2004 have no DIME
## record, i.e. no FEC committee) raised and spent essentially nothing: 0, not a median-imputed amount
if(Sys.getenv('V3_MONEY0', '1') == '1') for(s in c('dem', 'rep')){
    i <- H[, which(cycle >= 2004 & is.na(get(paste0('total.', s))) & !is.na(get(paste0(s, '.name'))) & get(paste0(s, '.name')) != '' & !(get(paste0(s, '.inc')) %in% 1))]
    for(v in intersect(paste0(c('total', 'total.ind', 'total.pac', 'total.cc', 'num.donors', 'total.disbursements', 'spare.cash'), '.', s), names(H))) set(H, i, v, 0)
    if(length(i)) cat(sprintf('money set to 0 for %d %s nominees with no records\n', length(i), s))
}
## a coded zero against real money is a missing link, not zero spending (bug review 2026-10-21): the AK-1/CA-6 FEC-summary
## overrides keep the original slot's pacwatch columns (AK-1: Hafner's $0 against Hill's $2.2M spent; CA-6: Kiley's slot
## empty), MD-6's Ficker has $1.0M disbursed and none coded, GA-14's Harris has $7.6M raised and no 2026 disbursement rows.
## These become NA (median-imputed and flagged by the residual models) instead of reading as no spending.
for(s in c('dem', 'rep')){
    pol <- paste0(s, '.disb.pw.political'); frs <- paste0(s, '.disb.pw.fundraising')
    td <- paste0('total.disbursements.', s); tr <- paste0('total.', s); sc <- paste0('spare.cash.', s)
    if(all(c(pol, frs) %in% names(H))){
        ## no political spending coded on > $200k disbursed (MD-6 has some fundraising spend coded, none political)
        i <- H[, which(cycle >= 2006 & fcoalesce(get(pol), 0) == 0 & fcoalesce(get(td), 0) > 2e5)]
        if(length(i)) cat(sprintf('coded spending missing (%s): %s\n', s, paste(H$rn[i][H$cycle[i] == FORECAST], collapse = ' ')))
        set(H, i, pol, NA_real_)
        set(H, i[fcoalesce(H[[frs]][i], 0) == 0], frs, NA_real_)
    }
    i <- H[, which(cycle >= 2006 & fcoalesce(get(td), 0) == 0 & fcoalesce(get(tr), 0) > 2e5)]   ## spending coding is patchy before 2006 (2004: 68% of rows coded)
    if(length(i)) cat(sprintf('no disbursements on > $200k raised (%s): %s\n', s, paste(H$rn[i][H$cycle[i] == FORECAST], collapse = ' ')))
    set(H, i, intersect(c(td, pol, frs, sc), names(H)), NA_real_)
}
ld <- function(d, r, c = MONEY_OFFSET) log(pmax(d, 0) + c) - log(pmax(r, 0) + c)
H[, `:=`(m.tot = ld(total.dem, total.rep), m.ind = ld(total.ind.dem, total.ind.rep),
         m.pac = ld(total.pac.dem, total.pac.rep), m.donors = ld(num.donors.dem, num.donors.rep, MONEY_OFFSET / 100),
         m.disb = ld(total.disbursements.dem, total.disbursements.rep),
         m.cash = ld(spare.cash.dem, spare.cash.rep), m.q6 = ld(dem.q6, rep.q6),
         m.self = ld(total.cc.dem, total.cc.rep))]
## resources actually deployed: political spending (ads, voter contact, field) vs fundraising spending,
## so a lucrative list operation counts through what it nets and spends, not through gross receipts
if(USE_POLSPEND) H[, `:=`(m.pol = ld(dem.disb.pw.political, rep.disb.pw.political), m.frs = ld(dem.disb.pw.fundraising, rep.disb.pw.fundraising))]
## money edge held by a heavy fundraising spender (+ for D-side, - for R-side)
if(USE_SPAM_FEAT) H[, fr.edge := fr.dem * pmax(m.tot, 0) - fr.rep * pmax(-m.tot, 0)]
## disbursement categories (legacy keyword coding from disburse_db; NA before 2004)
USE_DISB <- Sys.getenv('V3_DISB', '0') == '1'
if(USE_DISB){
    dcat <- c('advertising', 'polling', 'consulting.and.campaign.services', 'fundraising', 'salarypayroll', 'events')
    for(v in dcat) H[, (paste0('m.d.', v)) := ld(get(paste0('dem.disb.', v)), get(paste0('rep.disb.', v)), MONEY_OFFSET / 5)]
    ## share of each side's spending that went to advertising and polling
    H[, `:=`(d.adv.sh = dem.disb.advertising / pmax(dem.disb.total, 1), r.adv.sh = rep.disb.advertising / pmax(rep.disb.total, 1),
             d.poll.any = as.numeric(dem.disb.polling > 0), r.poll.any = as.numeric(rep.disb.polling > 0))]
}
## conduit-earmarked receipts Jul 1 - Aug 31 (public by ~Sept 20). Republicans
## have WinRed only from 2019, so each party's level enters separately,
## centered within cycle, rather than as a D-R ratio
if(USE_E78){
    H[, `:=`(e78.d = log1p(pmax(earmark.jul.aug.dem, 0)), e78.r = log1p(pmax(earmark.jul.aug.rep, 0)))]
    for(v in c('e78.d', 'e78.r')) H[, (v) := get(v) - median(get(v)[scored | contested26], na.rm = TRUE), by = cycle]
}
money <- grep('^m\\.', names(H), value = TRUE)
for(v in money) H[, (v) := get(v) - median(get(v)[scored | contested26], na.rm = TRUE), by = cycle]

H[, `:=`(dem.ext = dem.ip - dem.ip.party.cycle.avg, rep.ext = rep.ip - rep.ip.party.cycle.avg)]
## composite ideology (Bonica & Grumbach cf.all.mo; static per candidate, may draw on
## roll calls cast after a challenger's first election, so treat gains with caution)
if(USE_COMP){
    e <- new.env(); load(Sys.getenv('SML_COMPOSITE', '/home/bonica/Dropbox/claude_code/bonica_grumbach_ideology/data/composite_ideology_all_final_2025.rdata'), e)
    H[, `:=`(dem.comp = e$pp.tmp$cf.all.mo[match(dem.rid.static, rownames(e$pp.tmp))],
             rep.comp = e$pp.tmp$cf.all.mo[match(rep.rid.static, rownames(e$pp.tmp))])]
    ## leakage guard: non-incumbent winners always have a composite (later roll calls feed it)
    ## while many losers do not, so only incumbents' scores are used
    H[inc != 1, dem.comp := NA_real_]; H[inc != -1, rep.comp := NA_real_]
    H[, `:=`(dem.comp.ext = dem.comp - mean(dem.comp, na.rm = TRUE), rep.comp.ext = rep.comp - mean(rep.comp, na.rm = TRUE)), by = cycle]
    rm(e)
}
## Nokken-Poole DW-NOMINATE (dim 1) for the Congress that ends with the election, so
## only roll calls cast before it. Matched on icpsr, else on surname within
## state-party-Congress; incumbents only, like the composite.
if(USE_NP){
    lastname <- function(x){ x <- tolower(iconv(x, 'UTF-8', 'ASCII//TRANSLIT')); x <- fifelse(grepl(',', x), sub(',.*', '', x), x)
        x <- trimws(gsub('[^a-z ]', ' ', gsub('-', ' ', x))); x <- sub(' (jr|sr|ii|iii|iv)$', '', x); sub('.* ', '', x) }
    vv <- fread(file.path(Sys.getenv('SML_SCRATCH', '/ssd2/bonica/sml_v2'), 'dl/HSall_members.csv'))[chamber == 'House' & party_code %in% c(100, 200),
        .(congress, icpsr, st = state_abbrev, pty = fifelse(party_code == 100, 'D', 'R'), np = nokken_poole_dim1, nv = nominate_number_of_votes, last = lastname(sub(',.*', '', bioname)),
          first = tolower(iconv(sub(' .*', '', trimws(sub('^[^,]*,', '', bioname))), 'UTF-8', 'ASCII//TRANSLIT')))]
    vv <- vv[!duplicated(paste(congress, icpsr))]
    ## V3_NP_CUM (default on; 0 = that Congress only): vote-weighted mean of the member's scores through that Congress (career to date, still no later votes)
    if(Sys.getenv('V3_NP_CUM', '1') == '1') vv[order(congress), np := { w <- fifelse(is.na(np), 0, fcoalesce(as.numeric(nv), 0)); cumsum(w * fcoalesce(np, 0)) / cumsum(w) }, by = icpsr]
    vu <- vv[, if(.N == 1) .SD, by = .(congress, st, pty, last)]
    np.of <- function(icp, nm, pty){
        ic <- suppressWarnings(as.integer(fifelse(grepl('^[0-9]+$', icp), substr(icp, 1, nchar(icp) - 4), NA_character_)))
        cg <- (H$cycle - 1788) / 2
        out <- fcoalesce(vv$np[match(paste(cg, ic), paste(vv$congress, vv$icpsr))],
                         vu$np[match(paste(cg, H$state, pty, lastname(nm)), paste(vu$congress, vu$st, vu$pty, vu$last))])
        ## V3_FILL2: incumbents still unmatched (two same-surname members in the delegation: the Maloneys, the Rooneys, the
        ## Grijalvas; compound or married surnames: 'arenholz, ashley hinson', 'walorski swihart'): any name token as the
        ## surname, then the first name to break ties, kept only when exactly one member fits
        p0 <- pty
        if(FILL2) for(i in which(is.na(out) & H$inc == fifelse(p0 == 'D', 1, -1))){
            tk <- strsplit(trimws(gsub('[^a-z ]', ' ', tolower(iconv(nm[i], 'UTF-8', 'ASCII//TRANSLIT')))), ' +')[[1]]
            m <- vv[congress == cg[i] & st == H$state[i] & pty == p0 & last %in% tk]
            if(nrow(m) > 1) m <- m[first %in% tk]
            if(nrow(m) == 1) out[i] <- m$np
        }
        out
    }
    H[, `:=`(dem.np = np.of(dem.icpsr, dem.name, 'D'), rep.np = np.of(rep.icpsr, rep.name, 'R'))]
    H[inc != 1, dem.np := NA_real_]; H[inc != -1, rep.np := NA_real_]
    H[, `:=`(dem.np.ext = dem.np - mean(dem.np, na.rm = TRUE), rep.np.ext = rep.np - mean(rep.np, na.rm = TRUE)), by = cycle]
    cat(sprintf('Nokken-Poole: %.1f%% of incumbents matched\n', 100 * H[inc != 0, mean(!is.na(fifelse(inc == 1, dem.np, rep.np)))]))
    rm(vv, vu)
}

## expert ratings as of Oct 9 (Cook, Inside Elections, Sabato; mean on a -1..1 scale).
## Before 2020 the House tables list competitive races only: an unlisted race is
## Safe for the incumbent party (open seats: by lean).
USE_RATING <- Sys.getenv('V3_RATING', '1') == '1'
rt <- rate.file(file.path(PROJ, 'data/expert_ratings/house_ratings.csv'))[rater %in% c('cook', 'inside', 'sabato')]
## 2026: our dossier rating (written before the election) can join Cook/Inside/Sabato as a fourth rater for the
## competitive races (data/dossiers/2026). Off since 2026-10-21 (V3_OWNRATE=1 to restore): a rater that exists only in
## 2026, runs +0.31 categories toward D on its 83 races, and was written with the model's p.dem visible.
OWN.MAP <- c('Safe R' = -3, 'Likely R' = -2, 'Lean R' = -1, 'Tilt R' = -.5, 'Toss-up' = 0, 'Tilt D' = .5, 'Lean D' = 1, 'Likely D' = 2, 'Safe D' = 3)
own.ratings <- function(){
    f <- file.path(PROJ, 'data/dossiers/2026/dossier_2026.csv')
    if(Sys.getenv('V3_OWNRATE', '0') != '1' || !file.exists(f)) return(data.table(distcyc = character(0), rating_num = numeric(0)))
    d <- fread(f); d[!is.na(OWN.MAP[own_rating]), .(distcyc, rater = 'dossier', rating_num = OWN.MAP[own_rating])]
}
rt <- rbind(rt[, .(distcyc, rater, rating_num)], own.ratings(), fill = TRUE)
rt <- rt[, .(rating = mean(rating_num, na.rm = TRUE) / 3), by = distcyc]
H[, rating := rt$rating[match(rn, rt$distcyc)]]
H[cycle >= 2006 & cycle <= 2018 & is.na(rating), rating := fifelse(inc != 0, inc, sign(lean))]

## district demographics (ACS 5-year, lines of each cycle; data/acs/README.md)
ACS.F <- c('pct_hispanic', 'pct_black_nh', 'pct_asian_nh', 'pct_ba_plus', 'pct_white_nh_noncollege', 'log_density',
           'log_income', 'pct_over65', 'pct_foreign_born')
acs.file <- file.path(PROJ, 'data/acs/district_demographics.csv')
if(file.exists(acs.file)){
    acs <- fread(acs.file)[, `:=`(log_density = log1p(pop_density), log_income = log(median_hh_income))]
    k <- match(H$rn, acs$distcyc)
    for(v in ACS.F) set(H, j = v, value = acs[[v]][k])
    H[, log_income := log_income - median(log_income, na.rm = TRUE), by = cycle]   ## income is nominal by vintage
} else ACS.F <- character(0)

## lagged down-ballot partisanship from precinct returns (src/precinct_downballot_v3.R):
## district lean in Senate/governor/row-office and state-legislative races, relative to
## the state, from the previous cycle on the same lines (2020 votes re-aggregated to 2022 lines)
pb.file <- file.path(PROJ, 'data/precinct/district_downballot.csv')
PB.F <- character(0)
if(USE_PB && file.exists(pb.file)){
    pb <- fread(pb.file)
    lag.src <- data.table(cycle = c(2018, 2020, 2022, 2024, 2026), year = c(2016, 2018, 2020, 2022, 2022),
                          lines_year = c(2016, 2018, 2022, 2022, 2022))
    changed <- list(`2018` = 'PA', `2020` = 'NC', `2022` = character(0), `2024` = c('AL','GA','LA','NC','NY'),
                    `2026` = c('AL','GA','LA','NC','NY','CA','FL','OH','TN','TX','UT'))
    H[, pbkey := NA_character_]
    for(i in seq_len(nrow(lag.src))){
        cy <- lag.src$cycle[i]
        H[cycle == cy & !(state %in% changed[[as.character(cy)]]),
          pbkey := paste(lag.src$year[i], lag.src$lines_year[i], state, as.integer(sub('.*_', '', rn)))]
    }
    k <- match(H$pbkey, pb[, paste(year, lines_year, state, dnum)])
    H[, `:=`(pb.down = pb$down[k], pb.stleg = pb$lean_stleg[k])]
    ## ticket-splitting vs president: down-ballot lean minus the district's state-relative presidential lean
    stp <- fread(file.path(CP_DATA <- CPD, 'dk_pres_vs_state_level_1980_2020.csv'))
    stp[, po := state.abb[match(State, state.name)]]
    st.tp <- function(yr, po) mapply(function(y, p) { i <- match(p, stp$po); if(!paste0('dem_', y) %in% names(stp)) return(NA_real_)
        d <- stp[[paste0('dem_', y)]][i]; d / (d + stp[[paste0('rep_', y)]][i]) }, yr, po)
    H[, pres.st := st.tp(ref, state)]
    H[ref == 2024, pres.st := NA_real_]          ## 2024 statewide shares not in that file
    H[, pb.split := pb.down - (dist.pres.vs - pres.st)]
    PB.F <- c('pb.down', 'pb.stleg', 'pb.split')
}

## V3_FB: lagged full-ballot partisanship (src/precinct_split.R + src/precinct_fb_lag.R; previous election, this
## cycle's lines) relative to the state, minus the district's presidential lean relative to the state.
## Positive = Democrats run ahead of the top of the ticket down-ballot. NA before 2018 and in 2026 redrawn states.
if(Sys.getenv('V3_FB', '0') == '1'){
    fbl <- fread(file.path(PROJ, 'data/precinct/district_fb_lag.csv')); sps <- fread(file.path(PROJ, 'data/precinct/state_pres_share.csv'))
    H[, fb.gap := fbl$fb.rel[match(rn, fbl$distcyc)] - (dist.pres.vs - sps$dshare[match(paste(ref, state), paste(sps$year, sps$st))])]
    cat(sprintf('full-ballot gap: %s\n', paste(H[cycle >= 2018 & (scored | contested26), sprintf('%d %.0f%%', cycle, 100 * mean(!is.na(fb.gap))), by = cycle]$V1, collapse = ', ')))
    PB.F <- c(PB.F, 'fb.gap')
}

## ballot composition (the "many coattails"): what else is on the state's ballot, as of Oct 9
BAL.F <- character(0)
if(USE_BALLOT){
    abb <- function(x) fifelse(nchar(x) == 2, x, state.abb[match(x, state.name)])
    gv <- fread(file.path(PROJ, 'data/ballot_context/governor_races.csv'))[, state := abb(state)]
    gv <- gv[, .(gov.rating = mean(rating_num, na.rm = TRUE) / 3), by = .(cycle, state)]
    srt2 <- fread(file.path(PROJ, 'data/expert_ratings/senate_ratings.csv'))[rater %in% c('cook', 'inside', 'sabato')]
    srt2 <- srt2[, .(sen.rating = mean(rating_num, na.rm = TRUE) / 3), by = .(cycle, state)]
    bm <- fread(file.path(PROJ, 'data/ballot_context/ballot_measures.csv'))[, state := abb(state)]
    bm <- bm[, .(abortion.measure = as.numeric(any(topic == 'abortion')), n.measures = as.numeric(.N)), by = .(cycle, state)]
    H[, `:=`(gov.rating = gv$gov.rating[match(paste(cycle, state), paste(gv$cycle, gv$state))],
             sen.rating = srt2$sen.rating[match(paste(cycle, state), paste(srt2$cycle, srt2$state))],
             abortion.measure = fcoalesce(bm$abortion.measure[match(paste(cycle, state), paste(bm$cycle, bm$state))], 0),
             n.measures = fcoalesce(bm$n.measures[match(paste(cycle, state), paste(bm$cycle, bm$state))], 0))]
    H[, `:=`(has.gov = as.numeric(!is.na(gov.rating)), has.sen = as.numeric(!is.na(sen.rating)))]
    ## top-of-ticket lean relative to the state's partisanship: a Dem-favored race in a red state
    ## signals a pro-D ballot environment beyond the district's own lean
    H[, st.lean := (dist.pres.vs - lean) - .5]                      ## approx. state-level presidential share - .5
    H[, `:=`(gov.vs.state = gov.rating - 6 * st.lean, sen.vs.state = sen.rating - 6 * st.lean)]
    BAL.F <- c('has.gov', 'has.sen', 'gov.rating', 'sen.rating', 'gov.vs.state', 'sen.vs.state', 'abortion.measure', 'n.measures')
}

## primary results (all held before the Oct 9 forecast date, except Louisiana's November
## jungle vote, which is excluded): party primary turnout and nominees' primary strength.
## FEC results file through 2022, state SOS returns for 2024.
PRIM.F <- character(0)
if(USE_PRIM){
    fr0 <- fread(file.path(CPD, 'fec_election_results/_derived/federal_election_results.csv'))
    pr1 <- fr0[seat == 'federal:house' & stage == 'primary' & cycle %in% 2010:2022 & party %in% c('D', 'R'),
               .(cycle, state = state_po, dnum = suppressWarnings(as.integer(sub('^[A-Z]{2}', '', district))), party, votes)]
    sos <- fread(file.path(PROJ, 'data/full_primary_data_from_sos.csv'))[seat == 'federal:house' & cycle == 2024 & stage == 'primary']
    pr2 <- sos[party %in% c(100, 200), .(cycle, state = toupper(state), dnum = suppressWarnings(as.integer(district)),
                                         party = fifelse(party == 100, 'D', 'R'), votes)]
    pr <- rbind(pr1, pr2)[state != 'LA' & !is.na(votes)]
    pr[is.na(dnum) | dnum == 0, dnum := 1L]
    ag <- pr[, .(tot = sum(votes), top = max(votes)), by = .(cycle, state, dnum, party)]
    pw <- dcast(ag, cycle + state + dnum ~ party, value.var = c('tot', 'top'))
    pw[, `:=`(prim.dshare = tot_D / (tot_D + tot_R), d.nom.share = top_D / tot_D, r.nom.share = top_R / tot_R)]
    k <- match(paste(H$cycle, H$state, as.integer(sub('.*_', '', H$rn))), pw[, paste(cycle, state, dnum)])
    H[, `:=`(prim.dshare = pw$prim.dshare[k], d.nom.share = pw$d.nom.share[k], r.nom.share = pw$r.nom.share[k])]
    ## partisan primary turnout relative to the district's lean; top-two (CA, WA) kept separately
    H[, prim.rel := prim.dshare - dist.pres.vs]
    H[, prim.top2 := fifelse(state %in% c('CA', 'WA'), prim.dshare - dist.pres.vs, NA_real_)]
    H[state %in% c('CA', 'WA'), prim.rel := NA_real_]
    PRIM.F <- strsplit(Sys.getenv('V3_PRIM_VARS', 'prim.rel,prim.top2,d.nom.share,r.nom.share'), ',')[[1]]
}

## third-party candidates on the general-election ballot (presence is known before the
## election; their votes are not used). Left: Green/socialist; right: Libertarian/
## Constitution and similar; other: independents and minor parties. Fusion lines (a D or
## R nominee's own Conservative/Working Families line, as in NY) are excluded by name.
TP.F <- character(0)
if(USE_TP){
    fr1 <- fread(file.path(CPD, 'fec_election_results/_derived/federal_election_results.csv'))[
        seat == 'federal:house' & stage == 'general' & cycle %in% 2010:2024]
    fr1[, dnum := pmax(suppressWarnings(as.integer(sub('^[A-Z]{2}', '', district))), 1L)]
    nm <- function(x) gsub('[^a-z]', '', tolower(sub(',.*', '', x)))         ## surname
    maj <- fr1[party %in% c('D', 'R'), .(sur = unique(nm(candidate_name))), by = .(cycle, state_po, dnum)]
    tp <- fr1[!(party %in% c('D', 'R')) & !(party_raw %in% c('W', 'WRI', 'Write-in')) & !is.na(candidate_name)]
    tp <- tp[!paste(cycle, state_po, dnum, nm(candidate_name)) %in% maj[, paste(cycle, state_po, dnum, sur)]]
    pr <- toupper(tp$party_raw)
    tp[, side := fcase(party == 'G' | grepl('GRE|GREEN|SOC|PEACE|PFP|LEFT|WFP|PROG', pr), 'left',
                       party == 'L' | grepl('LIB|CON|IAP|UST|AMERICAN INDEPENDENT|AIP|REF|TAX', pr), 'right',
                       default = 'other')]
    tw <- dcast(tp[, .N, by = .(cycle, state_po, dnum, side)], cycle + state_po + dnum ~ side, value.var = 'N', fill = 0)
    for(v in c('left', 'right', 'other')) if(!v %in% names(tw)) tw[, (v) := 0]
    k <- match(paste(H$cycle, H$state, as.integer(sub('.*_', '', H$rn))), tw[, paste(cycle, state_po, dnum)])
    H[, `:=`(tp.left = fcoalesce(as.numeric(tw$left[k] > 0), 0), tp.right = fcoalesce(as.numeric(tw$right[k] > 0), 0),
             tp.other = fcoalesce(as.numeric(tw$other[k] > 0), 0))]
    ## 2026 ballots from the Oct 9 Wikipedia parse (data/2026/scripts/build_third_party_2026.py); the sides follow
    ## the coding above. Independents standing in for a major-party slot (AK-1 Hill, CA-6 Kiley) are dropped by name.
    tp26f <- file.path(PROJ, 'data/2026/house_third_party_2026.csv')
    if(file.exists(tp26f)){
        t26 <- fread(tp26f)
        sur26 <- function(x){ x <- trimws(gsub('[^A-Za-z ]', ' ', sub('\\(.*', '', x))); x <- sub(' +(jr|sr|ii|iii|iv)$', '', tolower(x)); sub('.* ', '', trimws(x)) }
        slot <- H[cycle == FORECAST, .(rn, s1 = sur26(dem.name), s2 = sur26(rep.name))]
        t26[, sur := sur26(name)]
        t26 <- t26[!paste(distcyc, sur) %in% c(slot[, paste(rn, s1)], slot[, paste(rn, s2)])]
        t26w <- dcast(t26[, .N, by = .(distcyc, side)], distcyc ~ side, value.var = 'N', fill = 0)
        for(v in c('left', 'right', 'other')) if(!v %in% names(t26w)) t26w[, (v) := 0]
        k26 <- match(H$rn, t26w$distcyc)
        for(v in c('left', 'right', 'other')) H[, (paste0('tp.', v)) := fifelse(cycle == FORECAST, fcoalesce(as.numeric(t26w[[v]][k26] > 0), 0), get(paste0('tp.', v)))]
        cat(sprintf('2026 third-party ballots: %d candidates in %d districts (left %d, right %d, other %d districts)\n', nrow(t26), uniqueN(t26$distcyc),
                    sum(t26w$left > 0), sum(t26w$right > 0), sum(t26w$other > 0)))
    } else H[cycle == FORECAST, c('tp.left', 'tp.right', 'tp.other') := NA_real_]   ## 2026 ballots not collected
    TP.F <- c('tp.left', 'tp.right', 'tp.other')
}

## district elasticity to the national swing: national cycle effect x district traits.
## The cycle effect (env) is the actual estimate for training cycles and the prediction
## for the test cycle (set inside fit.2stage), so the interactions are built there.
EL.TRAITS <- c('abs.lean', 'pct_hispanic', 'pct_ba_plus', 'pct_white_nh_noncollege', 'log_density', 'pb.split')
H[, abs.lean := abs(lean)]
if(USE_TOP2){ t2 <- fread(file.path(PROJ, 'data/top2_primary.csv')); H[, prim2 := t2$prim_dshare[match(rn, t2$distcyc)]] }
## presidential trend before the cycle: lean(ref) - lean(ref - 4) on the cycle's lines (0 where not available; midterms only)
if(USE_TREND || USE_TREND_FEAT){ tf <- fread(TREND_FILE)
    ## 2026 redrawn states (AL CA FL LA NC OH TN TX UT): no 2020 presidential vote is published on the new lines, so the
    ## file has no rows for them. The 2020 share on the new lines is approximated from the 2024 lines' 2020 share (the
    ## 2024 rows' dist.pres.vs), weighted by the census-block population crosswalk (build_incumbent_overlap.R); the trend is
    ## then 2024 share - that 2020 share, plus the file's own national offset.
    xw.f <- file.path(Sys.getenv('SML_SCRATCH', '/ssd2/bonica/sml_v2'), 'redistricting/district_crosswalks.rds')
    p26 <- fread(file.path(PROJ, 'data/2026/district_pres_vs_2026.csv'))
    if(file.exists(xw.f)){
        off <- merge(tf, p26[!is.na(dem_pres_vs_2020)], by = 'distcyc')[, mean(pres_trend - (dem_pres_vs_2024 - dem_pres_vs_2020))]
        old20 <- H[cycle == 2024, .(stab = state, old.dist = as.integer(sub('.*_', '', rn)), p20 = dist.pres.vs)]
        n20 <- merge(readRDS(xw.f)[cycle == FORECAST], old20, by = c('stab', 'old.dist'))[, .(p20 = sum(share * p20) / sum(share)), by = .(stab, new.dist)]
        n20[, distcyc := sprintf('%d_%s_%d', FORECAST, stab, new.dist)]
        add <- merge(n20, p26[, .(distcyc, p24 = dem_pres_vs_2024)], by = 'distcyc')[!distcyc %in% tf[!is.na(pres_trend), distcyc], .(distcyc, stab, pres_trend = p24 - p20 + off, source = 'crosswalk')]
        add[, stab := NULL]
        tf <- rbind(tf[!distcyc %in% add$distcyc], add, fill = TRUE)
        cat(sprintf('2026 trend for redrawn states from the block crosswalk: %d districts (national offset %+.4f)\n', nrow(add), off))
    }
    H[, trend.x := tf$pres_trend[match(rn, tf$distcyc)]]
    ## V3_TREND_FILL (default on, adopted 2026-10-23): districts the trend file lacks (all of 2012; CA/FL/NC/VA 2014-16; FL 2018-20; NJ/VA/TN/... 2022; at-large
    ## states): lean(ref) - lean(ref - 4) on the cycle's lines, with the ref - 4 share carried from the cycle-4-years-earlier
    ## rows (whose dist.pres.vs is that election) onto the current lines through the census-block crosswalks in between
    if(Sys.getenv('V3_TREND_FILL', '1') == '1' && file.exists(xw.f)){
        XW <- readRDS(xw.f)[, .(cycle, stab, new.dist, old.dist, share)]
        pv <- H[, .(cycle, stab = state, d = as.integer(sub('.*_', '', rn)), p = dist.pres.vs)]
        carry <- function(from, to, st){
            m <- pv[cycle == from & stab == st & !is.na(p), .(d, p)]
            for(c2 in seq(from + 2, to, 2)){ x <- XW[cycle == c2 & stab == st]
                if(nrow(x) && nrow(m) > 1) m <- merge(x, m, by.x = 'old.dist', by.y = 'd')[, .(p = sum(share * p) / sum(share)), by = .(d = new.dist)] }
            m[, cycle := to][, stab := st][]
        }
        need <- unique(H[cycle >= 2012 & is.na(trend.x) & !is.na(dist.pres.vs), .(cycle, state)])
        pp <- rbindlist(lapply(seq_len(nrow(need)), function(j) carry(need$cycle[j] - 4, need$cycle[j], need$state[j])))
        k <- match(paste(H$cycle, H$state, as.integer(sub('.*_', '', H$rn))), paste(pp$cycle, pp$stab, pp$d))
        H[, trend.fill := (dist.pres.vs - nat.pres[as.character(ref)]) - (pp$p[k] - nat.pres[as.character(ref - 4)])]
        cat(sprintf('trend fill from the crosswalks: %d of %d missing districts (2012+)\n', H[cycle >= 2012 & is.na(trend.x) & !is.na(trend.fill), .N], H[cycle >= 2012 & is.na(trend.x), .N]))
        H[cycle >= 2012 & is.na(trend.x), trend.x := trend.fill][, trend.fill := NULL]
    }
    ## V3_QA: in 2026 redrawn states without a 2020 vote on the new lines, both fills above carry old districts' 2020 shares
    ## onto the new lines, which gives cracked new districts fake trends (UT-1 +.26: the new Salt Lake seat inherits the old
    ## R districts' 2020 share; LA-2 +.13). Use the state's 2020 -> 2024 shift instead (mean of the new lines' 2024 shares
    ## minus the old lines' 2020 shares; equal-population districts, plus the trend file's national offset)
    if(QA && exists('off')){
        rd <- p26[map_changed %in% c(TRUE, 'True') & is.na(dem_pres_vs_2020), .(distcyc, stab = sub('^[0-9]+_([A-Z]+)_.*', '\\1', distcyc), p24 = dem_pres_vs_2024)]
        st <- merge(rd[, .(p24 = mean(p24)), by = stab], H[cycle == FORECAST - 2, .(p20 = mean(dist.pres.vs)), by = .(stab = state)], by = 'stab')
        rd[, tr := st$p24[match(stab, st$stab)] - st$p20[match(stab, st$stab)] + off]
        H[rd, on = .(rn = distcyc), trend.x := i.tr]
        cat(sprintf('QA: 2026 redrawn-state trend set to the state shift: %d districts (%s)\n', nrow(rd), paste(unique(rd$stab), collapse = ' ')))
    }
    H[, `:=`(trend = fcoalesce(trend.x, 0), trend.mid = trend.x * (cycle %% 4 != 0))]
    ## idiosyncratic trend: residual of the trend on district demographics, within cycle. The demographic
    ## (structural) part reverted in 2014/2018 midterms but not in 2022; the residual reverted in all three.
    if(TREND_IDIO && length(ACS.F)){
        ok <- !is.na(H$trend.x) & complete.cases(H[, ACS.F, with = FALSE])
        H[, trend := 0]
        H[ok, trend := residuals(lm(reformulate(ACS.F, 'trend.x'), .SD)), by = cycle]
    }
}

## optional donor-network features: SVD of the as-of-cutoff contest x donor matrix
N_SVD <- as.integer(Sys.getenv('V3_SVD', 0))
SVD.F <- character(0)
if(N_SVD > 0){
    U <- readRDS(file.path(SCR, 'diff_cm_asof_svd30.rds'))[, 1:N_SVD, drop = FALSE]
    SVD.F <- colnames(U)
    H[, (SVD.F) := as.data.table(U[match(rn, rownames(U)), , drop = FALSE])]
}
## external feature blocks (FEC report summaries, independent expenditures, donor features, redistricting overlap, ...):
## src/features_v3.R joins them onto H behind V3_* flags and returns their names in EXT.F (character(0) when all off)
EXT.F <- character(0)
if(file.exists(file.path(PROJ, 'src/features_v3.R'))) source(file.path(PROJ, 'src/features_v3.R'), local = TRUE)
## candidate blocks (donor profiles, background from bios): src/features_cand_v3.R
if(file.exists(file.path(PROJ, 'src/features_cand_v3.R'))) source(file.path(PROJ, 'src/features_cand_v3.R'), local = TRUE)
STAGE2 <- c(EXT.F, if(USE_TREND_FEAT) c('trend.x', 'trend.mid'), if(USE_SPAM_FEAT) c('fr.dem', 'fr.rep', 'fr.edge'), SVD.F, if(USE_ACS) ACS.F, PB.F, BAL.F, PRIM.F, TP.F, if(USE_RATING) 'rating', if(USE_E78) c('e78.d', 'e78.r'), if(USE_DISB) c('d.adv.sh', 'r.adv.sh', 'd.poll.any', 'r.poll.any'), 'lean', 'inc', 'midterm', 'pres.party', money,
            'dem.prev.over', 'rep.prev.over', if(USE_PREVOOS) c('dem.prev.oos', 'rep.prev.oos'), if(USE_DPREV) 'dist.prev.resid', if(USE_COMP) c('dem.comp', 'rep.comp', 'dem.comp.ext', 'rep.comp.ext'), if(USE_NP) c('dem.np', 'rep.np', 'dem.np.ext', 'rep.np.ext'), 'dem.ip', 'rep.ip', 'dem.ext', 'rep.ext',
            'quality.chall.dem', 'quality.chall.rep', if(USE_STLEG) c('dem.stleg', 'rep.stleg', 'dem.statewide', 'rep.statewide'), 'dem.female', 'rep.female',
            'num.cands.in.primary.dem', 'num.cands.in.primary.rep', 'top2')
## donors/pacs.prev.success.rate are left out: they come from the CFscore run's
## full-cycle contribution matrix (late money that follows likely winners is in
## past cycles but not in 2026), and dropping them did not hurt the backtest.

###############################################################################
## Models. Each takes training rows and test rows and returns test predictions of y.
###############################################################################
RESID_MODEL <- Sys.getenv('V3_RESID_MODEL', 'avg4')
## wide pool for the fuzzy forest: every numeric variable the codebook times as known before the election
WIDE.F <- local({ cb <- file.path(PROJ, 'data/compiled/codebook.csv')
    if(!file.exists(cb)) character(0) else {
        cb <- fread(cb)[office == 'house' & timing == 'pre' & type %in% c('numeric', 'integer') & pct_missing < 60]
        setdiff(intersect(cb$variable, names(H)), c('cycle', 'y', 'dem.cand.vs', 'scored', 'contested26', 'dr')) } })
INC_TREND <- Sys.getenv('V3_INC_TREND', '0') == '1'
rec.w <- function(cyc, T, tau) if(is.infinite(tau)) rep(1, length(cyc)) else exp(-(T - cyc) / tau)

## (a) benchmark: one linear model with the environment as covariates
fit.lin <- function(tr, te, T, tau = Inf){
    f <- y ~ lean + inc + gb + appr + mid.pen
    m <- lm(f, tr, weights = rec.w(tr$cycle, T, tau))
    predict(m, te)
}

## stage 1: national cycle effects from ALL earlier cycles (waves are rare, so the
## environment model needs every cycle), then cycle effect ~ environment.
## District slopes (lean, incumbency) are refit on the training window with
## recency weights, since nationalization has raised the lean slope and cut
## the incumbency effect over time.
## ridge toward a prior by data augmentation: OLS on the data plus one pseudo-row per slope (x_j = sqrt(lambda),
## y = sqrt(lambda) x prior_j), so the result is an ordinary lm and predict() works. lambda from the grid by
## leave-one-row-out error (0 = plain OLS). Slopes without a prior entry shrink toward 0; the intercept is free.
ridge.lm <- function(f, d, prior, grid = c(0, .002, .005, .01, .02, .05, .1, .2, .5, 1)){
    yv <- all.vars(f)[1]; xv <- setdiff(all.vars(f), yv); d <- as.data.table(d)[, c(yv, xv), with = FALSE]
    aug <- function(dd, lam){ if(lam == 0) return(dd)
        p <- rbindlist(lapply(xv, function(v){ r <- as.list(setNames(rep(0, length(xv)), xv)); r[[v]] <- sqrt(lam); r[[yv]] <- sqrt(lam) * fcoalesce(prior[v], 0); as.data.table(r) }), use.names = TRUE)
        rbind(dd, p[, c(yv, xv), with = FALSE]) }
    loo <- sapply(grid, function(lam) sqrt(mean(sapply(seq_len(nrow(d)), function(i){ m <- lm(f, aug(d[-i], lam)); (d[[yv]][i] - predict(m, d[i]))^2 }))))
    lam <- grid[which.min(loo)]
    m <- lm(f, aug(d, lam)); attr(m, 'lambda') <- lam; attr(m, 'loo') <- setNames(loo, grid); m
}
## cycle-level variables the cycle-effect regression may use (gb.mom/gb.final: V3_DRIFT; spec/mny: V3_NOPOLL)
NAT.V <- c('gb', 'appr', 'mid.pen', 'trump', 'umid', 'gb.mom', 'gb.final', 'spec', 'mny', 'mnyraw')
stage1 <- function(all, tr, T, tau, f2 = fe ~ gb + appr + mid.pen){
    m <- lm(y ~ 0 + lean + inc + factor(cycle), all)
    fe <- data.table(cycle = as.numeric(sub('factor\\(cycle\\)', '', grep('cycle', names(coef(m)), value = TRUE))),
                     fe = coef(m)[grep('cycle', names(coef(m)))])
    cyc <- unique(all[, intersect(c('cycle', NAT.V), names(all)), with = FALSE])[fe, on = 'cycle']
    ## V3_DRIFT: model the Oct 1 -> election-day drift of the generic ballot on earlier cycles (drift ~ momentum since
    ## Labor Day, shrunk toward 0 by leave-one-cycle-out), and feed the drift-adjusted ballot to the cycle-effect fit.
    ## The adjustment is applied to every cycle (training rows get their own predicted drift, not the realized one).
    if(DRIFT && all(c('gb.mom', 'gb.final') %in% names(cyc))){
        dd <- cyc[!is.na(gb.final) & !is.na(gb.mom)][, drift := gb.final - gb]
        b <- ridge.lm(drift ~ gb.mom, dd, prior = c(gb.mom = 0))
        cyc[, gb := gb + predict(b, cyc)]
    }
    ## V3_NAT_SHRINK: the cycle-effect regression shrunk toward the structural relation (share effect = .5 x the
    ## ballot margin, a 1-pt midterm penalty against the president's party), penalty chosen by leave-one-cycle-out
    if(sum(complete.cases(cyc[, intersect(all.vars(f2), names(cyc)), with = FALSE])) < 4) f2 <- fe ~ mid.pen   ## V3_NOPOLL: too few cycles with specials
    m2 <- if(NAT_SHRINK) ridge.lm(f2, cyc, prior = c(gb = .5, mid.pen = -.01)) else lm(f2, cyc)
    tr <- copy(tr)[, fe := fe$fe[match(cycle, fe$cycle)]][, w := rec.w(cycle, T, tau)]
    if(EL_LINEAR){
        ## linear elasticity: the cycle effect scales with district traits,
        ## y - fe = a*lean + b*inc + fe * (g . (Z - mean Z))
        Zc <- sapply(EL.LIN, function(v) tr[[v]] - mean(tr[[v]], na.rm = TRUE)); Zc[is.na(Zc)] <- 0
        A <- cbind(lean = tr$lean, inc = tr$inc, tr$fe * Zc)
        ms <- lm.wfit(A, tr$y - tr$fe, w = tr$w)
        cf <- ms$coefficients; cf[is.na(cf)] <- 0
        mu <- sapply(EL.LIN, function(v) mean(tr[[v]], na.rm = TRUE))
        return(list(m2 = m2, a = cf[1], b = cf[2], g = cf[-(1:2)], mu = mu, fe = fe))
    }
    if(INC_TREND){
        ## incumbency effect with a linear time trend, extrapolated to cycle T
        ms <- lm(I(y - fe) ~ 0 + lean + inc + inc:I(cycle - T), tr, weights = w)
        return(list(m2 = m2, a = coef(ms)['lean'], b = coef(ms)['inc'], fe = fe))
    }
    if(INC_COMP){
        ## V3_INC_COMP: the incumbency effect grows with district safety (inc x |lean|), estimated jointly with
        ## lean rather than patched after the fit (the post-fit incumbency-by-margin layer); vulnerable
        ## incumbents in competitive seats then get less than the average incumbency bonus.
        ## V3_LEAN3: a cubic lean term, since the residual model bends the linear lean slope back at both
        ## extremes (partial dependence 2026-10-21: +1.8 pts in deep-red seats, -1 in blue ones)
        fo <- if(LEAN3) I(y - fe) ~ 0 + lean + I(lean^3) + inc + inc:abs(lean) else I(y - fe) ~ 0 + lean + inc + inc:abs(lean)
        if(INC_ENV) fo <- update(fo, ~ . + inc:I(fe * pres.party))   ## V3_INC_ENV: fe is the realized cycle effect in training
        if(LEAN_TREND) fo <- update(fo, ~ . + lean:I(cycle - T))      ## V3_LEAN_TREND: the lean slope with a linear time trend, extrapolated to T (nationalization)
        ms <- lm(fo, tr, weights = w)
        return(list(m2 = m2, a = coef(ms)['lean'], b = coef(ms)['inc'], c = coef(ms)['inc:abs(lean)'], a3 = if(LEAN3) coef(ms)['I(lean^3)'] else NULL,
                    e = if(INC_ENV) coef(ms)['inc:I(fe * pres.party)'] else NULL, at = if(LEAN_TREND) coef(ms)['lean:I(cycle - T)'] else NULL, T = T, fe = fe))
    }
    ms <- lm(I(y - fe) ~ 0 + lean + inc, tr, weights = w)
    list(m2 = m2, a = coef(ms)['lean'], b = coef(ms)['inc'], fe = fe)
}
## predicted cycle effect for cycle cyc from cycles < cyc only, as cyc received it when it was the test cycle
## (the cycle-effect regression uses every earlier cycle, so the training window does not matter). NA with
## fewer than 3 earlier cycles. Memoized per run.
PGAP_OOS <- Sys.getenv('V3_PGAP_OOS', '1') == '1'   ## training poll gap against the cycle's out-of-sample environment
.fe.oos <- new.env()
fe.oos <- function(cyc, all, f2){
    key <- paste(cyc, paste(deparse(f2), collapse = ''))
    if(is.null(.fe.oos[[key]])){
        .fe.oos[[key]] <- tryCatch({
            e <- all[cycle < cyc]
            if(uniqueN(e$cycle) < 3) NA_real_ else {
                s <- stage1(e, e[cycle >= min(MIN_TRAIN, cyc - 8)], cyc, Inf, f2)
                as.numeric(predict(s$m2, unique(all[cycle == cyc, intersect(NAT.V, names(all)), with = FALSE]))[1])
            }}, error = function(x) NA_real_)
    }
    .fe.oos[[key]]
}
base1 <- function(s1, d, fe){
    out <- s1$a * d$lean + s1$b * d$inc
    if(!is.null(s1$c)) out <- out + s1$c * d$inc * abs(d$lean)     ## V3_INC_COMP: incumbency x district safety
    if(!is.null(s1$a3)) out <- out + s1$a3 * d$lean^3               ## V3_LEAN3: cubic lean
    if(!is.null(s1$e)) out <- out + s1$e * d$inc * fe * d$pres.party   ## V3_INC_ENV
    if(!is.null(s1$at)) out <- out + s1$at * d$lean * (d$cycle - s1$T) ## V3_LEAN_TREND (0 for the test cycle itself)
    ## V3_LOGIT_ENV: the cycle effect as a uniform shift in log-odds (x4 matches the share-scale shift at 50-50),
    ## so a wave moves competitive seats by the full amount and safe seats by less; else a uniform share shift
    out <- if(LOGIT_ENV) plogis(qlogis(pmin(pmax(.5 + out, .02), .98)) + 4 * fe) - .5 else out + fe
    if(!is.null(s1$g)){
        Zc <- sapply(names(s1$mu), function(v) d[[v]] - s1$mu[[v]]); Zc[is.na(Zc)] <- 0
        out <- out + fe * as.numeric(matrix(Zc, nrow(d)) %*% s1$g)
    }
    out
}

## tr: all earlier scored rows; the district stage uses cycles >= min.train
fit.2stage <- function(tr, te, T, tau = Inf, use.gbm = TRUE, nrounds = 400, depth = 3, eta = .03,
                       f2 = fe ~ gb + appr + mid.pen, feats = STAGE2, center = FALSE, min.train = 0){
    trw <- tr[cycle >= min(min.train, T - 8)]
    s1 <- stage1(tr, trw, T, tau, f2)
    tr.fe <- s1$fe$fe[match(trw$cycle, s1$fe$cycle)]
    te.fe <- predict(s1$m2, te)
    ## V3_FE_ORACLE=1 (hindsight demonstration): the test cycle's realized cycle effect (mean outcome net of the lean and
    ## incumbency slopes over its scored races) replaces the predicted one, i.e. the size of the swing is known exactly
    if(Sys.getenv('V3_FE_ORACLE') == '1' && T < FORECAST && sum(!is.na(te$y)) > 10)
        te.fe <- rep(mean((te$y - base1(s1, te, 0))[!is.na(te$y)]), nrow(te))
    r <- trw$y - base1(s1, trw, tr.fe)
    pred <- base1(s1, te, te.fe)
    if(USE_EL){
        ## elasticity: env x (trait - mean), so districts can respond more or less to the national swing
        tr.env <- tr.fe - mean(s1$fe$fe); te.env <- te.fe - mean(s1$fe$fe)
        trw <- copy(trw); te <- copy(te)
        for(v in intersect(EL.TRAITS, names(trw))){
            mu <- mean(trw[[v]], na.rm = TRUE)
            set(trw, j = paste0('el.', v), value = tr.env * (trw[[v]] - mu))
            set(te, j = paste0('el.', v), value = te.env * (te[[v]] - mu))
        }
        feats <- c(feats, paste0('el.', intersect(EL.TRAITS, names(trw))))
    }
    if(POLLFEAT){
        ## poll average minus the stage-1 base (NA when unpolled: xgboost routes it, the others impute + flag).
        ## Training base (V3_PGAP_OOS, default): each training cycle's own out-of-sample predicted environment,
        ## built the same way as the test cycle's. With V3_PGAP_OOS=0 the realized environment (y - r): the
        ## training gaps then hold only district-level poll information while the test gaps also carry the
        ## national model's error, which the trees read as district signal.
        trw <- copy(trw); te <- copy(te)
        tr.base <- trw$y - r
        if(PGAP_OOS){
            uc <- unique(trw$cycle); fo <- sapply(uc, function(c) fe.oos(c, tr, f2))
            tr.base <- base1(s1, trw, fcoalesce(fo[match(trw$cycle, uc)], tr.fe))
        }
        trw[, `:=`(pgap = poll.x - tr.base, pneff = log1p(fcoalesce(neff.x, 0)))]
        te[, `:=`(pgap = poll.x - pred, pneff = log1p(fcoalesce(neff.x, 0)))]
        feats <- c(feats, 'pgap', 'pneff')
        if(PPART){
            ## partisan share of the poll weight, and the gap carried by internals only (NA when unpolled)
            trw[, `:=`(ppart = part.x, pgap.part = pgap * part.x)]; te[, `:=`(ppart = part.x, pgap.part = pgap * part.x)]
            feats <- c(feats, 'ppart', 'pgap.part')
        }
    }
    if(use.gbm){
        Xtr <- as.matrix(trw[, ..feats]); Xte <- as.matrix(te[, ..feats])
        g.xgb <- function(){
            w.env <- if(ENVW > 0) exp(-abs(tr.fe - te.fe[1]) / ENVW) else 1   ## V3_ENVW: cycles with an environment like T's weigh more
            dtr <- xgb.DMatrix(Xtr, label = r, weight = rec.w(trw$cycle, T, tau) * w.env)
            prm <- list(objective = 'reg:squarederror', eta = eta, max_depth = depth,
                        subsample = .8, colsample_bytree = .8, min_child_weight = 10, nthread = 8)
            if(MONO){
                ## residual should not fall as the Democrat's money, rating or track record improve
                mono <- fcase(feats %in% c('rating', 'm.tot', 'm.ind', 'm.q6', 'm.cash', 'm.donors', 'dem.prev.over'), 1L,
                              feats %in% c('rep.prev.over'), -1L, default = 0L)
                prm$monotone_constraints <- paste0('(', paste(mono, collapse = ','), ')')
            }
            g <- xgb.train(params = prm, data = dtr, nrounds = nrounds, verbose = 0)
            predict(g, Xte)
        }
        ## random forest and ridge need complete inputs: median-impute plus missing flags
        imp <- function(M, ref) { for(j in seq_len(ncol(M))) M[is.na(M[, j]), j] <- fcoalesce(median(ref[, j], na.rm = TRUE), 0); M }
        g.rf <- function(){
            rf <- ranger::ranger(x = imp(Xtr, Xtr), y = r, num.trees = 500, min.node.size = 25,
                                 mtry = max(2, floor(ncol(Xtr) / 3)), num.threads = 8, seed = 1)
            predict(rf, imp(Xte, Xtr))$predictions
        }
        g.ridge <- function(){
            na.f <- function(M) { z <- is.na(M[, colSums(is.na(Xtr)) > 0, drop = FALSE]) * 1; colnames(z) <- paste0(colnames(z), '.na'); z }
            A <- cbind(imp(Xtr, Xtr), na.f(Xtr)); B <- cbind(imp(Xte, Xtr), na.f(Xte))
            cv <- glmnet::cv.glmnet(A, r, alpha = 0, nfolds = 5)
            as.numeric(predict(cv, B, s = 'lambda.min'))
        }
        g.gam <- function(){
            A <- as.data.table(imp(Xtr, Xtr)); B <- as.data.table(imp(Xte, Xtr))
            ## smooth terms only for continuous inputs not dominated by one value (e.g. zero spending)
            nl <- names(A)[sapply(A, function(z) uniqueN(z) > 30 && max(tabulate(match(z, unique(z)))) / length(z) < .3)]
            lin <- setdiff(names(A), nl)
            fo <- as.formula(paste('r ~', paste(c(sprintf('s(%s, k = 5)', nl), lin), collapse = ' + ')))
            A[, r := r]
            gm <- tryCatch(mgcv::bam(fo, data = A, select = TRUE, discrete = TRUE, nthreads = 8), error = function(e){
                ## fall back to smooths only for inputs with plenty of distinct values
                nl2 <- nl[sapply(A[, ..nl], function(z) uniqueN(round(z, 3)) > 100)]
                fo2 <- as.formula(paste('r ~', paste(c(sprintf('s(%s, k = 5)', nl2), setdiff(names(A), c(nl2, 'r'))), collapse = ' + ')))
                mgcv::bam(fo2, data = A, select = TRUE, nthreads = 8) })
            as.numeric(predict(gm, B))
        }
        ## bake-off learners (2026-10-21; V3_RESID_MODEL = bart, gpb, ff, or avg4+<one of them>)
        g.bart <- function(){                        ## Bayesian additive regression trees, posterior mean
            set.seed(1)
            b <- dbarts::bart(imp(Xtr, Xtr), r, imp(Xte, Xtr), ntree = 200, ndpost = 1000, nskip = 500, verbose = FALSE)
            as.numeric(b$yhat.test.mean)
        }
        g.gpb <- function(){                         ## boosting + random intercepts: D candidate, R candidate, state
            grp <- function(d) { k <- function(v, p) fifelse(is.na(v) | v == '', paste0(p, seq_len(nrow(d)), '_', d$rn), v)
                cbind(dc = k(d$dem.rid.static, 'nd'), rc = k(d$rep.rid.static, 'nr'), st = d$state) }
            gm <- gpboost::GPModel(group_data = grp(trw), likelihood = 'gaussian')
            ds <- gpboost::gpb.Dataset(Xtr, label = r)
            bst <- gpboost::gpb.train(data = ds, gp_model = gm, nrounds = 300, verbose = 0,
                                      params = list(learning_rate = .03, max_depth = 3, num_leaves = 8, min_data_in_leaf = 20,
                                                    feature_fraction = .8, num_threads = 8))   ## GPBoost does not allow bagging
            pr <- predict(bst, data = Xte, group_data_pred = grp(te), predict_var = FALSE, pred_latent = FALSE)
            as.numeric(pr$response_mean)
        }
        g.ff <- function(){                          ## fuzzy forest on the wide pool of pre-election variables
            wf <- intersect(WIDE.F, names(trw)); wf <- wf[sapply(trw[, ..wf], function(z) is.numeric(z) && uniqueN(z, na.rm = TRUE) > 1)]
            W <- as.matrix(trw[, ..wf]); Wt <- as.matrix(te[, ..wf])
            W <- imp(W, W); Wt <- imp(Wt, as.matrix(trw[, ..wf]))
            keep <- apply(W, 2, sd) > 0; W <- W[, keep, drop = FALSE]; Wt <- Wt[, keep, drop = FALSE]
            ## modules: hierarchical clustering on 1 - |correlation| (WGCNA's cor() clashes with this R build)
            mm <- cutree(hclust(as.dist(1 - abs(cor(W))), method = 'average'), h = .5)
            sm <- table(mm); mm[mm %in% names(sm)[sm < 2]] <- max(mm) + 1   ## singletons pooled into one module
            f <- fuzzyforest::ff(as.data.frame(W), r, module_membership = mm, num_processors = 1,
                                 screen_params = fuzzyforest::screen_control(keep_fraction = .25, ntree_factor = 3, min_ntree = 300),
                                 select_params = fuzzyforest::select_control(number_selected = 30, ntree_factor = 3, min_ntree = 300),
                                 final_ntree = 500)
            as.numeric(predict(f$final_rf, as.data.frame(Wt)[, f$feature_list$feature_name, drop = FALSE]))
        }
        comps <- NULL
        if(RESID_MODEL == 'avg4'){
            comps <- cbind(xgb = g.xgb(), rf = g.rf(), ridge = g.ridge(), gam = g.gam())
            g.te <- rowMeans(comps)
        } else if(grepl('^avg4\\+', RESID_MODEL)){
            extra <- sub('^avg4\\+', '', RESID_MODEL)
            comps <- cbind(xgb = g.xgb(), rf = g.rf(), ridge = g.ridge(), gam = g.gam(),
                           x = switch(extra, bart = g.bart(), gpb = g.gpb(), ff = g.ff()))
            g.te <- rowMeans(comps)
        } else g.te <- switch(RESID_MODEL, xgb = g.xgb(), rf = g.rf(), ridge = g.ridge(), gam = g.gam(),
                              avg = (g.xgb() + g.rf()) / 2, avg3 = (g.xgb() + g.rf() + g.ridge()) / 3,
                              bart = g.bart(), gpb = g.gpb(), ff = g.ff())
        base <- pred
        pred <- pred + if(center) g.te - mean(g.te) else g.te
        attr(pred, 'comp') <- comps; attr(pred, 'base') <- base
    }
    attr(pred, 'env') <- te.fe - mean(s1$fe$fe)      ## predicted environment relative to the average cycle
    pred
}

###############################################################################
## Backtest harness
###############################################################################
score <- function(p, d) {
    cmp <- abs(d$lean) < .08
    data.table(n = nrow(d), rmse = sqrt(mean((p - d$y)^2)), acc = mean(sign(p) == sign(d$y)),
               miss = sum(sign(p) != sign(d$y)), acc.comp = mean((sign(p) == sign(d$y))[cmp]),
               n.comp = sum(cmp), seat.err = sum(p > 0) - sum(d$y > 0),
               nat.err = mean(p) - mean(d$y))
}
backtest <- function(fit, label, ...){
    rbindlist(lapply(BT_CYCLES, function(T){
        tr <- H[scored & cycle < T]; te <- H[scored & cycle == T]
        cbind(model = label, cycle = T, score(fit(tr, te, T, ...), te))
    }))
}
summ <- function(bt) bt[, .(rmse = sqrt(mean(rmse^2)), acc = sum(n * acc) / sum(n), misses = sum(miss),
                            acc.comp = sum(n.comp * acc.comp) / sum(n.comp), mae.seat = mean(abs(seat.err)),
                            rmse.nat = sqrt(mean(nat.err^2))), by = model]

###############################################################################
## District polls: sponsor-adjusted, recency-weighted average as of Oct 9,
## blended with the fundamentals prediction. Sponsor bias and the blend
## parameters are estimated from earlier cycles only.
###############################################################################
POLL_CUTOFF <- Sys.getenv('V3_POLL_CUTOFF', ASOF)   ## hindsight experiment: e.g. 11-01 for polls through the campaign
USE_PQUAL <- Sys.getenv('V3_PQUAL', '0') == '1'   ## weight polls by 538 pollster rating
PARTISAN_W <- as.numeric(Sys.getenv('V3_PARTISAN_W', .5))   ## weight on partisan-sponsored polls
PW_TUNE <- Sys.getenv('V3_PW_TUNE', '0') == '1'  ## tune the partisan weight, and a discount for districts with only partisan polls, on earlier cycles
BLEND_FROM <- as.numeric(Sys.getenv('V3_BLEND_FROM', 2012))  ## first cycle used to tune the poll blend
polls <- fread(file.path(PROJ, 'data/polls/house_district_polls.csv'))
## 2026: NYT poll files as the primary source plus Oct 18 Wikipedia/VoteHub polls NYT lacks
## (data/polls/scripts/fetch_nyt_polls.sh, merge_nyt_2026.R); the Oct 18 file alone if the merge is absent
p26f <- file.path(PROJ, 'data/polls/house_district_polls_2026_current.csv')
if(!file.exists(p26f)) p26f <- file.path(PROJ, 'data/polls/house_district_polls_2026_oct18.csv')
if(ASOF != '10-09' && file.exists(p26f)) polls <- rbind(polls[cycle != FORECAST], fread(p26f), fill = TRUE)
## V3_QA: drop district polls of a different matchup (a poll candidate who is not the slot's nominee: 2022 AK-1's Peltola v
## Begich ranked-choice head-to-heads when the final round was against Palin; a pre-primary 2018 MS-3 poll). Names match if a
## name token (3+ letters) of one appears in the other's letters-only form (married names, O'Brien, missing spaces).
if(QA){
    lt <- function(x) gsub('[^a-z]', '', tolower(iconv(x, 'UTF-8', 'ASCII//TRANSLIT')))
    tk <- function(x) { t <- strsplit(trimws(gsub('[^a-z ]', ' ', tolower(iconv(x, 'UTF-8', 'ASCII//TRANSLIT')))), ' +')[[1]]; t[nchar(t) >= 3 & !t %in% c('jr', 'sr', 'iii')] }
    nm.ok <- function(a, b) is.na(a) | a == '' | is.na(b) | b == '' | mapply(function(x, y) any(vapply(tk(y), grepl, TRUE, lt(x), fixed = TRUE)) || any(vapply(tk(x), grepl, TRUE, lt(y), fixed = TRUE)), a, b)
    k <- match(polls$distcyc, H$rn)
    bad <- !is.na(k) & !(nm.ok(polls$dem_cand, H$dem.name[k]) & nm.ok(polls$rep_cand, H$rep.name[k]))
    cat(sprintf('QA: %d district polls of a different matchup dropped (%s)\n', sum(bad), paste(unique(polls$distcyc[bad]), collapse = ' ')))
    polls <- polls[!bad]
}
## CA-6 (Pan vs Kiley as D vs R): general-election head-to-heads from data/2026/la_ca6_polls_2026.csv.
## The September poll's sponsor backs Kiley, so it counts as R-sponsored.
ca6p <- file.path(PROJ, 'data/2026/la_ca6_polls_2026.csv')
if(file.exists(ca6p)){
    cp <- fread(ca6p)[distcyc == '2026_CA_6' & grepl('head to head', notes) & party %in% c('D', 'I')]
    cp <- dcast(cp, distcyc + pollster + end_date + sample_size + population + sponsor ~ party, value.var = 'share')
    polls <- rbind(polls, cp[, .(cycle = as.integer(FORECAST), state = 'CA', district = 6L, distcyc, pollster, end_date = as.IDate(end_date),
                                 sample_size = as.integer(sample_size), population, dem_pct = D, rep_pct = I, dem_cand = 'Richard Pan',
                                 rep_cand = 'Kevin Kiley', partisan_sponsor = fifelse(grepl('supports Kiley', sponsor), 'R', ''),
                                 source = 'data/2026/la_ca6_polls_2026.csv')], fill = TRUE)
}
polls <- polls[!is.na(dem_pct) & !is.na(rep_pct)]
polls[, `:=`(p2 = dem_pct / (dem_pct + rep_pct) - .5,
             days = as.numeric(poll.ref(cycle) - end_date),
             sponsor = fifelse(partisan_sponsor %in% c('D', 'R'), partisan_sponsor, 'N'),
             n = fifelse(is.na(sample_size), 400, pmin(sample_size, 1500)))]
polls <- polls[days >= 0]
## V3_POLL_MAXAGE: drop polls that ended more than this many days before the as-of date. The 1998-2016 file starts in
## early September (<= ~45 days before Oct 18); the 2018+ and 2026 files reach back a year or more.
POLL_MAXAGE <- as.numeric(Sys.getenv('V3_POLL_MAXAGE', 'Inf')); polls <- polls[days <= POLL_MAXAGE]
## pollster quality: 538 predictive plus-minus (lower = better) from the ratings
## version published before each cycle; unrated pollsters get a below-average weight
pkey <- function(x) gsub(' (inc|llc|co|corp|polling|research|strategies|group|associates|analytics|the)( |$)', ' ',
                         gsub('[^a-z ]', ' ', tolower(x))) |> gsub(pattern = ' +', replacement = ' ') |> trimws()
pr.dir <- file.path(PROJ, 'data/polls/historical/538/pollster_ratings')
pr.ver <- c(`2016` = 'v2016', `2018` = 'v2018', `2020` = 'v2020', `2022` = 'v2021', `2024` = 'v2023', `2026` = 'v2023')
prt <- rbindlist(lapply(names(pr.ver), function(cy){
    r <- fread(file.path(pr.dir, pr.ver[[cy]], 'pollster-ratings.csv'))
    data.table(cycle = as.integer(cy), pk = pkey(r$Pollster), ppm = suppressWarnings(as.numeric(r$`Predictive Plus-Minus`)))
}))[!is.na(ppm)][!duplicated(paste(cycle, pk))]
polls[, ppm := prt$ppm[match(paste(cycle, pkey(pollster)), paste(prt$cycle, prt$pk))]]
polls[, q := fifelse(is.na(ppm), .8, pmin(1.6, pmax(.4, exp(-ppm / 2))))]
if(!USE_PQUAL) polls[, q := 1]
polls[, y := H$y[match(distcyc, H$rn)]]
polls <- polls[distcyc %in% H$rn[H$scored | H$contested26]]

## mean poll error by sponsor, relative to nonpartisan polls, from cycles < T
sponsor.bias <- function(T){
    e <- polls[cycle < T & !is.na(y) & days <= 60, .(err = mean(p2 - y)), by = sponsor]
    b <- setNames(e$err - e$err[e$sponsor == 'N'], e$sponsor)
    b[c('D', 'R', 'N')] <- fcoalesce(b[c('D', 'R', 'N')], 0); b
}
## pw: weight on a partisan poll; po: multiplies n.eff where every poll is partisan
poll.avg <- function(cyc, hl, bias, pw = PARTISAN_W, po = 1){
    d <- polls[cycle == cyc][, adj := p2 - bias[sponsor]]
    d[, w := 0.5^(days / hl) * sqrt(n / 600) * fifelse(sponsor == 'N', 1, pw) * q]
    ## part.sh: share of the poll weight from partisan-sponsored polls (1 = internals only)
    d[, .(poll = sum(w * adj) / sum(w), n.eff = sum(w) * fifelse(all(sponsor != 'N'), po, 1), part.sh = sum(w[sponsor != 'N']) / sum(w)), by = distcyc]
}
## Current-cycle demographic shift: regress (poll - fundamentals) on demographics
## across polled districts (ridge, weighted by poll volume) and apply the fitted
## slopes to every district. Captures group swings visible in this cycle's polls
## (e.g. Hispanic districts polling ahead of fundamentals) for unpolled seats.
PD.F <- c('pct_hispanic', 'pct_black_nh', 'pct_ba_plus', 'pct_white_nh_noncollege', 'log_density')
demo.shift <- function(d, pa, lambda = 2){
    k <- match(d$rn, pa$distcyc); pol <- !is.na(k) & complete.cases(d[, ..PD.F])
    if(sum(pol) < 20) return(rep(0, nrow(d)))
    Z <- as.matrix(d[, ..PD.F]); mu <- colMeans(Z[pol, ], na.rm = TRUE); sdv <- apply(Z, 2, sd, na.rm = TRUE)
    Zs <- sweep(sweep(Z, 2, mu), 2, sdv, '/')
    r <- pa$poll[k[pol]] - d$pf[pol]; w <- pa$n.eff[k[pol]]
    A <- cbind(1, Zs[pol, ]); W <- diag(w / mean(w))
    b <- solve(t(A) %*% W %*% A + diag(c(0, rep(lambda, ncol(Zs)))), t(A) %*% W %*% r)
    sh <- as.numeric(Zs %*% b[-1]); sh[is.na(sh)] <- 0
    sh
}

## blend: p + n.eff / (n.eff + k) * (poll - p)
blend <- function(p, pa, k) { b <- pa$n.eff / (pa$n.eff + k); fifelse(is.na(pa$poll), p, p + b * (pa$poll - p)) }
if(POLLFEAT){
    ## as-of poll averages for the residual-model features (30-day half-life; sponsor bias from earlier cycles)
    pf.all <- rbindlist(lapply(sort(unique(polls$cycle)), function(c) poll.avg(c, 30, sponsor.bias(c))))
    H[, `:=`(poll.x = pf.all$poll[match(rn, pf.all$distcyc)], neff.x = pf.all$n.eff[match(rn, pf.all$distcyc)],
             part.x = pf.all$part.sh[match(rn, pf.all$distcyc)])]
}

###############################################################################
## Final pipeline: OOS predictions for every cycle, rolling poll blend,
## uncertainty from earlier cycles' errors, calibration, 2026 forecast
###############################################################################
F.FINAL <- setdiff(STAGE2, c('midterm', 'pres.party'))
if(Sys.getenv('V3_DROP') != '') F.FINAL <- setdiff(F.FINAL, strsplit(Sys.getenv('V3_DROP'), ',')[[1]])
## training window (first cycle used) and recency half-life, in cycles; set via env for experiments
MIN_TRAIN <- as.numeric(Sys.getenv('V3_MIN_TRAIN', 2012))
TAU       <- as.numeric(Sys.getenv('V3_TAU', Inf))
H[, trump := as.numeric(cycle %in% c(2016, 2020, 2024))]
## V3_APPR: presidential net approval (Oct 1, signed toward D) in the cycle-effect regression
F2 <- if(USE_TRUMP) fe ~ gb + mid.pen + trump else if(Sys.getenv('V3_APPR') == '1') fe ~ gb + mid.pen + appr else fe ~ gb + mid.pen
## V3_UND: undecided/other share of the Oct 1 generic-ballot polls (30-day mean, data/polls/gb_polls_1998_2026.csv)
## x toward the out-party x midterm: do undecideds break against the president's party in midterms?
if(Sys.getenv('V3_UND') == '1'){
    gp <- fread(file.path(PROJ, 'data/polls/gb_polls_1998_2026.csv'))[, end := as.IDate(end)]
    un <- gp[, { d <- as.IDate(paste0(cycle[1], '-10-01')); .(und = mean((100 - dem - rep)[end <= d & end > d - 30])) }, by = cycle]
    H[, umid := un$und[match(cycle, un$cycle)] / 100 * fifelse(dem.pres == 1, -1, 1) * midterm]
    F2 <- fe ~ gb + mid.pen + umid
}
## V3_NOPOLL: a model with no polls anywhere (experiment, 2026-10-21). Run with V3_POLLFEAT=0 V3_RATING=0 V3_PDEMO=0
## (district polls, expert ratings and the demographic poll shift off); this flag replaces the generic ballot in the
## cycle-effect regression with non-poll national readings: mid = the midterm penalty alone; spec = + the mean
## special-election swing of the cycle (data/specials/house_specials_by_cycle.csv, 2006+; earlier cycles fall back
## to mid); mny = + the national money edge (mean log D/R receipts ratio over the cycle's contested seats, money
## as of the cutoff); both = + spec + mny. The error scales then widen on their own: the national scale is the
## RMS of the earlier cycles' mean out-of-sample errors.
NOPOLL <- Sys.getenv('V3_NOPOLL', '0')
if(NOPOLL != '0'){
    sp <- fread(file.path(PROJ, 'data/specials/house_specials_by_cycle.csv'))
    H[, spec := sp$mean_swing[match(cycle, sp$cycle)]]
    ## mnyraw: the raw mean of the log receipts ratio (mixes the parties' incumbent counts into the reading; 79 misses).
    ## mny: the cycle effect of the money ratio net of lean and incumbency, the money analogue of the vote cycle effect
    ## (pooled slopes, no outcomes involved).
    H[, mnyraw := mean(m.tot[scored | contested26], na.rm = TRUE), by = cycle]
    mm <- coef(lm(m.tot ~ 0 + lean + inc + factor(cycle), H[scored | contested26]))
    ce <- mm[grep('factor\\(cycle\\)', names(mm))]; names(ce) <- sub('factor\\(cycle\\)', '', names(ce))
    H[, mny := as.numeric(ce[as.character(cycle)])]
    F2 <- switch(NOPOLL, mid = fe ~ mid.pen, spec = fe ~ mid.pen + spec, mny = fe ~ mid.pen + mny, mnyraw = fe ~ mid.pen + mnyraw,
                 both = fe ~ mid.pen + spec + mny, stop('V3_NOPOLL must be mid, spec, mny, mnyraw or both'))
    polls <- polls[0]   ## no district polls at all: no poll features, no blend, no poll-status error scale, no demographic shift
    cat('no-poll national model:', deparse(F2), '\n'); print(unique(H[cycle >= 2006, .(cycle, mid.pen, spec = round(spec, 3), mny = round(mny, 3), mnyraw = round(mnyraw, 3))])[order(cycle)])
}
## V3_XGB_DEPTH / V3_XGB_ROUNDS: boosting depth (interaction order) and rounds; defaults 4 and 800
fund <- function(T, te) fit.2stage(H[scored & cycle < T], te, T, f2 = F2, min.train = MIN_TRAIN,
                                   nrounds = as.integer(Sys.getenv('V3_XGB_ROUNDS', 800)), depth = as.integer(Sys.getenv('V3_XGB_DEPTH', 4)), feats = F.FINAL, tau = TAU)
GRID <- if(PW_TUNE) CJ(hl = c(14, 30, 60, 120), k = c(.25, .5, 1, 2, 4), pw = c(.1, .25, .5, 1), po = c(.25, .5, 1)) else
    CJ(hl = c(14, 30, 60, 120), k = c(.25, .5, 1, 2, 4), pw = PARTISAN_W, po = 1)
if(POLLFEAT) GRID <- unique(rbind(GRID, copy(GRID)[, k := 1e6]))   ## k = 1e6: polls only through the model
## no outside poll blend: polls enter only through the residual model (adopted 2026-10-21 on the contest-fixed
## data: 2018-24 .02517/49/.02367 -> .02471/48/.02365, 2006-24 misses 156 -> 151). V3_BLEND_OFF=0 for the light blend.
if(POLLFEAT && Sys.getenv('V3_BLEND_OFF', '1') == '1') GRID <- GRID[k == 1e6]
tune.blend <- function(oos, cycles){
    GRID[, .(sse = sum(sapply(cycles, function(c){
        r <- oos[cycle == c]; pa <- poll.avg(c, hl, sponsor.bias(c), pw, po)[match(r$rn, distcyc)]
        sum((blend(r$pf, pa, k) - r$y)^2) }))), by = .(hl, k, pw, po)][which.min(sse)]
}
## append one row per run to <OUT>/model_scoreboard.csv (2018-24 and all backtest cycles), with:
##  - a block-bootstrap SE (resample cycles with replacement, then races within each cycle) on rmse and Brier
##  - competitive-only metrics (|pp| < .1: n.comp, misses.comp, brier.comp), where flags actually differ
##  - paired deltas (this run minus baseline, same races and same bootstrap draws) against a baseline run's OOS
##    predictions: V3_BASE_OOS=<rds>, default the production <chamber>_oos.rds when V3_OUT is an experiment dir
## Rows written before 2026-10-21 lack the new columns (the file is rewritten with fill).
log.score <- function(chamber, bt, config, B = 400, extra = NULL){   ## extra: a one-row data.table of further columns (e.g. seat metrics)
    bt <- as.data.table(bt)[!is.na(y) & !is.na(pp) & !is.na(prob)]
    ## cmp: the competitive set; paired deltas pass the baseline's set so both runs are scored on the same races
    mets <- function(d, pp = d$pp, prob = d$prob, cmp = abs(pp) < .1){ sq <- (prob - (d$y > 0))^2; ms <- sign(pp) != sign(d$y)
        c(rmse = sqrt(mean((pp - d$y)^2)), misses = sum(ms), brier = mean(sq), n.comp = sum(cmp), misses.comp = sum(ms[cmp]), brier.comp = mean(sq[cmp])) }
    boot <- function(d, f){
        cyc <- split(seq_len(nrow(d)), d$cycle)
        seed <- if(exists('.Random.seed', globalenv())) get('.Random.seed', globalenv()) else NULL; set.seed(7)
        s <- replicate(B, { ii <- unlist(lapply(sample(cyc, length(cyc), replace = TRUE), function(j) j[sample.int(length(j), replace = TRUE)])); f(d[ii]) })
        if(!is.null(seed)) assign('.Random.seed', seed, globalenv())
        apply(s, 1, sd)
    }
    prod.out <- normalizePath(file.path(PROJ, 'output_v3'), mustWork = FALSE)
    bf <- Sys.getenv('V3_BASE_OOS', if(normalizePath(OUT, mustWork = FALSE) != prod.out) file.path(prod.out, paste0(tolower(chamber), '_oos.rds')) else '')
    base <- NULL
    if(bf != '' && file.exists(bf) && 'rn' %in% names(bt)){
        b0 <- readRDS(bf); if(!is.data.frame(b0)) b0 <- b0$oos
        if(is.data.frame(b0) && all(c('rn', 'y', 'pp', 'prob') %in% names(b0))){
            b0 <- as.data.table(b0)[!is.na(y) & !is.na(pp) & !is.na(prob), .(rn, pp0 = pp, prob0 = prob)]
            base <- merge(bt, b0, by = 'rn')
        } else warning('baseline ', bf, ' has no oos table with rn/y/pp/prob; no paired deltas')
    }
    one <- function(d, suf){
        m <- mets(d); se <- boot(d, mets)
        out <- c(n = nrow(d), m, se.rmse = se[['rmse']], se.brier = se[['brier']], se.brier.comp = se[['brier.comp']])
        if(!is.null(base)){
            bd <- base[rn %in% d$rn]; dm <- function(z){ cb <- abs(z$pp0) < .1; mets(z, cmp = cb) - mets(z, z$pp0, z$prob0, cmp = cb) }
            dl <- dm(bd); dse <- boot(bd, dm)
            out <- c(out, d.rmse = dl[['rmse']], d.rmse.se = dse[['rmse']], d.misses = dl[['misses']], d.brier = dl[['brier']], d.brier.se = dse[['brier']],
                     d.misses.comp = dl[['misses.comp']], d.brier.comp = dl[['brier.comp']], d.brier.comp.se = dse[['brier.comp']], n.base = nrow(bd))
        }
        r <- as.data.table(as.list(round(out, 5))); setnames(r, paste0(names(r), '.', suf)); r
    }
    r <- cbind(data.table(time = format(Sys.time(), '%Y-%m-%d %H:%M'), chamber = chamber,
                          tag = Sys.getenv('V3_TAG', 'default'), config = paste0(config, '; cutoff=', CUTOFF, if(POLLS.LATEST) '+polls_latest', if(GB_TREND != '0') paste0('; gb_trend=', GB_TREND))),
               one(bt[cycle >= 2018], '1824'), one(bt, 'all'),
               ## hold-out reading: tune and decide on 2018-22, check on 2024 (the columns exist so a decision can ignore 2024)
               one(bt[cycle %in% 2018:2022], '1822')[, .SD, .SDcols = patterns('^(n|misses|brier|brier\\.comp|d\\.misses|d\\.brier)\\.')],
               one(bt[cycle == 2024], '2024')[, .SD, .SDcols = patterns('^(n|misses|brier|brier\\.comp|d\\.misses|d\\.brier)\\.')],
               first.cycle = min(bt$cycle), base = if(is.null(base)) '' else bf)
    if(!is.null(extra)) r <- cbind(r, extra)
    cat(sprintf('%s 2018-24: rmse %.4f (se %.4f), misses %d, brier %.4f (se %.4f); competitive |pp|<.1: n %d, misses %d, brier %.4f (se %.4f)\n',
                chamber, r$rmse.1824, r$se.rmse.1824, r$misses.1824, r$brier.1824, r$se.brier.1824, r$n.comp.1824, r$misses.comp.1824, r$brier.comp.1824, r$se.brier.comp.1824))
    if(!is.null(base)) for(w in c('1824', 'all')) cat(sprintf('  vs baseline (%s, %s): d.rmse %+.5f (se %.5f), d.misses %+d, d.brier %+.5f (se %.5f); competitive d.misses %+d, d.brier %+.5f (se %.5f) [%s]\n',
                w, r[[paste0('n.base.', w)]], r[[paste0('d.rmse.', w)]], r[[paste0('d.rmse.se.', w)]], r[[paste0('d.misses.', w)]], r[[paste0('d.brier.', w)]], r[[paste0('d.brier.se.', w)]],
                r[[paste0('d.misses.comp.', w)]], r[[paste0('d.brier.comp.', w)]], r[[paste0('d.brier.comp.se.', w)]], bf))
    ## hold-out discipline (2026-10-21): decide on 2018-22, then check 2024
    if(!is.null(base) && all(c('d.misses.1822', 'd.misses.2024') %in% names(r)))
        cat(sprintf('  DECISION 2018-22: d.misses %+d, d.brier %+.5f (se %.5f), competitive d.brier %+.5f | HOLD-OUT 2024: d.misses %+d, d.brier %+.5f (se %.5f)\n',
                    r$d.misses.1822, r$d.brier.1822, r$d.brier.se.1822, r$d.brier.comp.1822, r$d.misses.2024, r$d.brier.2024, r$d.brier.se.2024))
    f <- file.path(OUT, 'model_scoreboard.csv')
    if(file.exists(f)) r <- rbind(fread(f, colClasses = list(character = c('time', 'chamber', 'tag', 'config'))), r, fill = TRUE)
    fwrite(r, f)
}
house.config <- function() sprintf('resid=%s min_train=%s tau=%s rating=%s svd=%s money_offset=%s drop=%s; scales: nat=%s x%s dscale_n=%d%s',
                                   RESID_MODEL, MIN_TRAIN, TAU, USE_RATING, N_SVD, MONEY_OFFSET, Sys.getenv('V3_DROP'),
                                   NAT_MODE, NAT_SCALE, DSCALE_N, paste0(if(DSCALE_ROLL) ' roll', if(NOPOLL != '0') paste0(' nopoll=', NOPOLL)))

DF <- 10        ## t degrees of freedom for district errors
NAT_DF <- as.numeric(Sys.getenv('V3_NAT_DF', 5))     ## t degrees of freedom for the national shock (Inf = normal). Default 5 since 2026-10-21 by the user's decision: a heavier national tail for 2026, no backtest cost (CRPS 3.05 vs 3.06)
SCALE_D <- .8   ## district error scale (tuned on the backtest Brier score)
## national error scale: multiplier on the RMS of earlier cycles' out-of-sample national errors. 1.5 (tuned on
## seat-interval coverage in an earlier version) until 2026-10-21; now 1: with 1.5 the 50% seat interval covered
## 10 of 10 cycles and the 90% width was 34 seats against a largest miss of 8. See Model_v3_notes.md, "Error scales".
NAT_SCALE <- as.numeric(Sys.getenv('V3_NAT_SCALE', 1))
## V3_DSCALE_N: the district error SD comes from the last N backtest cycles only (0 = all cycles since 2002).
## The model's district error has halved since 2010, so the all-cycle SD overstates it for recent cycles
## (2018-24: favorites at .7-.95 won 95%). Default 4 since 2026-10-21 (two midterms and two presidential cycles).
## The state share and the error-size model keep all cycles. The Senate pins its own values.
DSCALE_N <- as.integer(Sys.getenv('V3_DSCALE_N', 4))
## V3_DSCALE_ROLL: replace the fixed SCALE_D with a multiplier re-chosen each cycle to minimise the Brier score
## on earlier backtest cycles (normal approximation, each with its own scales), as the Senate does
DSCALE_ROLL <- Sys.getenv('V3_DSCALE_ROLL', '0') == '1'
## V3_NAT_MODE: rms (default) = NAT_SCALE x RMS of earlier cycles' out-of-sample national errors of the final
## predictions; loo = leave-one-cycle-out error of the stage-1 cycle-effect regression (fe ~ gb + mid.pen) on
## the cycles before T, no multiplier
NAT_MODE <- Sys.getenv('V3_NAT_MODE', 'rms')
nat.loo <- function(T){
    all <- H[scored & cycle < T]
    s <- stage1(all, all[cycle >= min(MIN_TRAIN, T - 8)], T, Inf, F2)
    cyc <- unique(all[, intersect(c('cycle', NAT.V), names(all)), with = FALSE])[s$fe, on = 'cycle']
    e <- sapply(seq_len(nrow(cyc)), function(i) cyc$fe[i] - predict(lm(F2, cyc[-i]), cyc[i]))
    sqrt(mean(e^2))
}
## error scales from earlier cycles: national (mean error) and district (by poll status)
err.scales <- function(oos, cycles){
    e <- oos[cycle %in% cycles]
    nat <- e[, .(ne = mean(pp - y)), by = cycle]
    e[, de := pp - y - nat$ne[match(cycle, nat$cycle)]]
    ed <- if(DSCALE_N > 0) e[cycle %in% tail(sort(unique(e$cycle)), DSCALE_N)] else e
    sd.d <- if(DSCALE_ROLL) 1 else SCALE_D
    d.nopoll <- sd(ed[!(has.poll)]$de) * sqrt((DF - 2) / DF)
    d.poll <- if(sum(ed$has.poll) >= 30) sd(ed[(has.poll)]$de) * sqrt((DF - 2) / DF) else d.nopoll
    ## share of district error variance shared within state-cycle (correlated misses)
    vc <- tryCatch(as.data.frame(lme4::VarCorr(lme4::lmer(de ~ 1 + (1 | sc), e[, sc := paste(cycle, state)]))),
                   error = function(x) NULL)
    rho <- if(is.null(vc)) 0 else min(.5, vc$vcov[1] / sum(vc$vcov))
    hfit <- NULL
    if(HETERO && all(c('same.lines', 'top2') %in% names(e))){
        ## district-specific error size: log |district error| on race characteristics
        hx <- function(d) d[, .(lpoll = log1p(fcoalesce(n.eff, 0)), top2 = fcoalesce(as.numeric(top2), 0), open = as.numeric(inc == 0),
                                redist = as.numeric(!same.lines), close = pmin(abs(pp), .2),
                                rgap = if(USE_RATING) abs(fcoalesce(rating, 0) - pmax(-1, pmin(1, pp / .1))) else 0)]
        hd <- cbind(hx(e), la = log(abs(e$de) + .003))
        hfit <- list(m = lm(la ~ ., hd), hx = hx)
        hfit$norm <- sqrt(mean(exp(2 * (fitted(hfit$m) - mean(fitted(hfit$m))))))
        hfit$mu <- mean(fitted(hfit$m))
    }
    corr <- NULL
    if(USE_CORR){
        ## district errors shared by state, by Census division, and along demographic dimensions (a random
        ## slope per cycle on each standardized trait), as shares of the district error variance
        zf <- PD.F; mu <- sapply(zf, function(v) mean(e[[v]], na.rm = TRUE)); sdv <- sapply(zf, function(v) sd(e[[v]], na.rm = TRUE))
        for(j in seq_along(zf)) e[, (paste0('z', j)) := fcoalesce((get(zf[j]) - mu[j]) / sdv[j], 0)]
        e[, rc := paste(cycle, DIVISION[state])]
        fm <- as.formula(paste('de ~ 1 + (1 | sc) + (1 | rc) + (0 +', paste0('z', seq_along(zf), collapse = ' + '), '|| cycle)'))
        vc <- tryCatch(as.data.frame(lme4::VarCorr(lme4::lmer(fm, e))), error = function(x) NULL)
        if(!is.null(vc)){
            V <- sum(vc$vcov)
            tau <- setNames(vc$vcov[match(paste0('z', seq_along(zf)), vc$var1)] / V, zf); tau[is.na(tau)] <- 0
            corr <- list(rho.s = vc$vcov[vc$grp == 'sc'] / V, rho.r = vc$vcov[vc$grp == 'rc'] / V, tau = tau, mu = mu, sdv = sdv)
            rho <- corr$rho.s
        }
    }
    nat.sc <- if(NAT_MODE == 'loo') nat.loo(max(cycles) + 2) else NAT_SCALE * sqrt(mean(nat$ne^2))
    list(nat = nat.sc, d.poll = sd.d * d.poll, d.nopoll = sd.d * d.nopoll, rho = rho, hfit = hfit, corr = corr)
}
## district error SD per race under the scales (poll status x error-size model), for the normal-approximation tuning
sd.h <- function(d, sc){ sdi <- fifelse(d$has.poll, sc$d.poll, sc$d.nopoll)
    if(!is.null(sc$hfit)) sdi <- sdi * exp(predict(sc$hfit$m, sc$hfit$hx(d)) - sc$hfit$mu) / sc$hfit$norm; sdi }
## CRPS of an empirical distribution x against the outcome y
crps <- function(x, y){ x <- sort(x); n <- length(x); mean(abs(x - y)) - sum((2 * seq_len(n) - n - 1) * x) / n^2 }
## Census divisions (regional shocks)
DIVISION <- c(CT = 1, ME = 1, MA = 1, NH = 1, RI = 1, VT = 1, NJ = 2, NY = 2, PA = 2, IL = 3, IN = 3, MI = 3, OH = 3, WI = 3,
              IA = 4, KS = 4, MN = 4, MO = 4, NE = 4, ND = 4, SD = 4, DE = 5, DC = 5, FL = 5, GA = 5, MD = 5, NC = 5, SC = 5, VA = 5, WV = 5,
              AL = 6, KY = 6, MS = 6, TN = 6, AR = 7, LA = 7, OK = 7, TX = 7, AZ = 8, CO = 8, ID = 8, MT = 8, NV = 8, NM = 8, UT = 8, WY = 8,
              AK = 9, CA = 9, HI = 9, OR = 9, WA = 9)
## nat.z: optional standard-normal national draws shared across chambers
simulate <- function(d, sc, nsim = 20000, nat.z = NULL){
    if(!is.null(nat.z)) nsim <- length(nat.z)
    sdi <- fifelse(d$has.poll, sc$d.poll, sc$d.nopoll)
    if(!is.null(sc$hfit)) sdi <- sdi * exp(predict(sc$hfit$m, sc$hfit$hx(d)) - sc$hfit$mu) / sc$hfit$norm
    ## V3_NAT_DF: t-distributed national shock (Inf = normal); shared draws are mapped through their quantiles so
    ## the House and Senate still move together
    nz <- if(is.null(nat.z)) rnorm(nsim) else nat.z
    if(is.finite(NAT_DF)) nz <- qt(pnorm(nz), NAT_DF) * sqrt((NAT_DF - 2) / NAT_DF)
    nat <- nz * sc$nat
    st <- as.integer(factor(d$state))
    zs <- matrix(rnorm(max(st) * nsim), max(st))[st, , drop = FALSE]      ## state shocks
    idio <- matrix(rt(nrow(d) * nsim, DF), nrow(d)) * sqrt((DF - 2) / DF)
    if(is.null(sc$corr)) err <- sdi * (sqrt(sc$rho) * zs + sqrt(1 - sc$rho) * idio) / sqrt((DF - 2) / DF) else {
        cr <- sc$corr; rg <- as.integer(factor(DIVISION[d$state]))
        zr <- matrix(rnorm(max(rg) * nsim), max(rg))[rg, , drop = FALSE]          ## division shocks
        Z <- sapply(names(cr$tau), function(v) fcoalesce((d[[v]] - cr$mu[v]) / cr$sdv[v], 0))
        fac <- Z %*% (matrix(rnorm(length(cr$tau) * nsim), length(cr$tau)) * sqrt(cr$tau))   ## similar-district swings
        idio.sh <- pmax(1 - cr$rho.s - cr$rho.r - as.numeric(Z^2 %*% cr$tau), .2)
        err <- sdi * (sqrt(cr$rho.s) * zs + sqrt(cr$rho.r) * zr + fac + sqrt(idio.sh) * idio) / sqrt((DF - 2) / DF)
    }
    ## V3_SIM_EL: competitive seats take more of the national shock (the elasticity layer's logic inside the simulation)
    fac <- if(SIM_EL > 0) { cp <- pmax(4 * (.5 + d$pp) * (.5 - d$pp), 0); pmax(1 + SIM_EL * (cp / mean(cp) - 1), .2) } else rep(1, nrow(d))
    sims <- outer(d$pp, rep(1, nsim)) + outer(fac, nat) + err
    list(prob = rowMeans(sims > 0), seats = colSums(sims > 0))
}
## shared national draws for the 2026 forecast (House and Senate use the same ones)
nat.z.2026 <- function(){
    f <- file.path(OUT, 'national_z_2026.rds')
    if(!file.exists(f)) { set.seed(2026); saveRDS(rnorm(20000), f) }
    readRDS(f)
}

if(sys.nframe() == 0){
    oos <- rbindlist(lapply(c(seq(2002, 2024, 2), FORECAST), function(T){
        te <- H[(if(T == FORECAST) contested26 else scored) & cycle == T]
        p <- fund(T, te)
        te[, `:=`(pf = as.numeric(p), env.hat = as.numeric(attr(p, 'env')))]
        if(!is.null(attr(p, 'comp'))) te[, c('c.xgb', 'c.rf', 'c.ridge', 'c.gam', 'c.base') := c(as.data.table(attr(p, 'comp')[, 1:4]), list(attr(p, 'base')))]
        te
    }))
    ## learned stacking weights for the four residual models (non-negative, fit on earlier cycles)
    if(STACK && 'c.xgb' %in% names(oos)){
        cc <- c('c.xgb', 'c.rf', 'c.ridge', 'c.gam')
        for(T in c(BT_CYCLES, FORECAST)){
            e <- oos[cycle < T & cycle >= 2012]
            if(uniqueN(e$cycle) < 2) next
            A <- as.matrix(e[, ..cc]); t <- e$y - e$c.base
            sol <- quadprog::solve.QP(crossprod(A) + diag(1e-8, 4), crossprod(A, t), diag(4), rep(0, 4))
            w <- pmax(sol$solution, 0)
            i <- which(oos$cycle == T)
            oos[i, pf := c.base + as.numeric(as.matrix(oos[i, ..cc]) %*% w)]
            if(T == FORECAST) cat('stacking weights (xgb, rf, ridge, gam):', round(w, 2), '\n')
        }
    }
    oos[, `:=`(pp = NA_real_, has.poll = FALSE, prob = NA_real_, pf0 = pf, pf.fund = pf)]
    if(USE_TREND) oos[, `:=`(tr.w = 0, tr.f = 0)]
    if(USE_INCC) oos[, `:=`(ic.a = 0, ic.g = 0)]
    if(USE_TOP2) oos[, `:=`(t2.w = 0, t2.g = 0)]
    seat.dist <- list(); blend.par <- list()
    for(T in c(BT_CYCLES, FORECAST)){
        bp <- tune.blend(oos, seq(max(2002, min(BLEND_FROM, T - 8)), T - 2, 2)); blend.par[[as.character(T)]] <- bp
        pa <- poll.avg(T, bp$hl, sponsor.bias(T), bp$pw, bp$po)
        i <- which(oos$cycle == T); k <- match(oos$rn[i], pa$distcyc)
        if(STATE_SHIFT && file.exists(file.path(OUT, 'senate_oos.rds'))){
            ## Senate polls minus Senate fundamentals (both out of sample) give a state-level
            ## signal; a share of it, tuned on earlier cycles, shifts that state's House seats
            so.s <- readRDS(file.path(OUT, 'senate_oos.rds'))$oos[!is.na(poll), .(cycle, state, sr = poll - pf)]
            so.s <- so.s[, .(sr = mean(sr)), by = .(cycle, state)]
            sr.of <- function(c) { j <- which(oos$cycle == c); fcoalesce(so.s$sr[match(paste(c, oos$state[j]), paste(so.s$cycle, so.s$state))], 0) }
            pastS <- if(T >= 2014) intersect(seq(2012, T - 2, 2), unique(oos$cycle)) else integer(0)
            sgrid <- c(0, .1, .2, .3, .5)
            sseS <- if(!length(pastS)) rep(0, length(sgrid)) else sapply(sgrid, function(ws) sum(sapply(pastS, function(c){
                j <- which(oos$cycle == c); pc <- poll.avg(c, bp$hl, sponsor.bias(c), bp$pw, bp$po); kc <- match(oos$rn[j], pc$distcyc)
                sum((blend(oos$pf0[j] + ws * sr.of(c), pc[kc], bp$k) - oos$y[j])^2) })))
            ws <- if(length(pastS)) sgrid[which.min(sseS)] else 0
            oos[i, `:=`(state.sh = sr.of(T), state.w = ws)]
            oos[i, `:=`(pf0 = pf0 + ws * state.sh, pf = pf0 + ws * state.sh)]
        }
        if(USE_EL2){
            ## swing elasticity: competitive seats move more with the national environment than a
            ## uniform shift implies. Shift = predicted environment x (comp / cycle mean - 1), with
            ## comp = 4p(1-p) of the fundamentals prediction. kappa = 0 is uniform swing, 1 the same swing
            ## on the log-odds scale; tuned on earlier cycles. With EL2_SAFE (default) only seats less
            ## competitive than the cycle mean move: the backtest gain was all in safe seats, and moving
            ## toss-ups shifted seat counts without improving their accuracy.
            el.of <- function(c) { j <- which(oos$cycle == c); cp <- pmax(4 * (.5 + oos$pf.fund[j]) * (.5 - oos$pf.fund[j]), 0)
                                   s <- cp / mean(cp) - 1; oos$env.hat[j] * (if(EL2_SAFE) pmin(s, 0) else s) }
            pastE <- intersect(seq(2002, T - 2, 2), unique(oos$cycle)); pastE <- pastE[pastE >= as.numeric(Sys.getenv('V3_EL_FROM', 2002))]   ## V3_EL_FROM (experiment 2026-10-23): first cycle the elasticity is tuned on
            pcE <- lapply(pastE, function(c) { j <- which(oos$cycle == c); pc <- poll.avg(c, bp$hl, sponsor.bias(c), bp$pw, bp$po)
                                               list(j = j, pa = pc[match(oos$rn[j], pc$distcyc)], sh = el.of(c)) })
            kgrid <- seq(0, 4, .25)
            sseE <- if(length(pcE)) sapply(kgrid, function(kp) sum(sapply(pcE, function(z) sum((blend(oos$pf.fund[z$j] + kp * z$sh, z$pa, bp$k) - oos$y[z$j])^2)))) else rep(0, length(kgrid))
            ## at least 3 earlier cycles: tuned on two (the 2006 backtest: 2002, 2004) kappa jumped between 0 and the grid
            ## maximum 4 on small perturbations (2026-10-21), and that cycle's miss then fed every later cycle's national scale
            kp <- if(length(pastE) >= 3) kgrid[which.min(sseE)] else 0
            if(T == FORECAST) cat(sprintf('swing elasticity kappa for 2026: %.2f (predicted environment %+.3f)\n', kp, oos$env.hat[i[1]]))
            oos[i, `:=`(el.sh = el.of(T), el.k = kp)]
            oos[i, `:=`(pf0 = pf0 + kp * el.sh, pf = pf0 + kp * el.sh)]
        }
        if(USE_TREND){
            ## presidential trend (lean(ref) - lean(ref - 4)): midterm House results lag the latest pres
            ## realignment (weight < 0). Midterms only (pres-year weights did not carry over). Not in close
            ## races: there the polls, money and candidates already carry it (trend slope of the error ~0)
            ## and shifting them added misses. Weight tuned on earlier midterms.
            safe.of <- function(c) { j <- which(oos$cycle == c); cp <- 4 * (.5 + oos$pf.fund[j]) * (.5 - oos$pf.fund[j])
                                     if(is.na(TREND_MIN)) as.numeric(cp < mean(cp)) else as.numeric(abs(oos$pf.fund[j]) >= TREND_MIN) }
            oos[i, tr.f := trend * (T %% 4 != 0) * safe.of(T)]
            pastT <- intersect(seq(T - 4, 2002, -4), unique(oos[tr.f != 0]$cycle))
            pcT <- lapply(pastT, function(c) { j <- which(oos$cycle == c); pc <- poll.avg(c, bp$hl, sponsor.bias(c), bp$pw, bp$po)
                                               list(j = j, pa = pc[match(oos$rn[j], pc$distcyc)], base = oos$pf0[j] - oos$tr.w[j] * oos$tr.f[j]) })
            tgrid <- seq(-1, 1, .05)
            tw <- if(!length(pastT)) 0 else tgrid[which.min(sapply(tgrid, function(tw)
                sum(sapply(pcT, function(z) sum((blend(z$base + tw * oos$tr.f[z$j], z$pa, bp$k) - oos$y[z$j])^2)))))]
            if(T == FORECAST) cat(sprintf('presidential trend weight for 2026: %.2f (tuned on %s)\n', tw, paste(pastT, collapse = ', ')))
            oos[i, tr.w := tw]
            oos[i, `:=`(pf0 = pf0 + tr.w * tr.f, pf = pf0 + tr.w * tr.f)]
        }
        if(USE_INCC){
            ## incumbency by margin: in the backtest, incumbents in races within 10 pts did ~1 pt worse than
            ## forecast (both parties, 9 of 10 cycles) and those in safe seats better. One incumbency slope
            ## fits both. Shift = inc x (a + g x |fundamentals margin|), a and g tuned on earlier cycles.
            im <- function(j) pmin(abs(oos$pf.fund[j]), .4)
            pastI <- intersect(seq(2002, T - 2, 2), unique(oos$cycle[oos$cycle >= 2006]))
            pcI <- lapply(pastI, function(c) { j <- which(oos$cycle == c); pc <- poll.avg(c, bp$hl, sponsor.bias(c), bp$pw, bp$po)
                                               list(j = j, pa = pc[match(oos$rn[j], pc$distcyc)], m = im(j), inc = oos$inc[j],
                                                    base = oos$pf0[j] - oos$inc[j] * (oos$ic.a[j] + oos$ic.g[j] * im(j))) })
            G <- CJ(a = seq(-.02, .01, .0025), g = seq(-.05, .15, .01))
            G[, sse := if(length(pastI) < 2) 0 else sapply(seq_len(.N), function(r) sum(sapply(pcI, function(z)
                sum((blend(z$base + z$inc * (a[r] + g[r] * z$m), z$pa, bp$k) - oos$y[z$j])^2))))]
            ic <- if(length(pastI) >= 2) G[which.min(sse)] else data.table(a = 0, g = 0)
            if(T == FORECAST) cat(sprintf('incumbency by margin for 2026: a = %.4f, g = %.3f (shift at a toss-up %+.2f pts, at a 30-pt margin %+.2f)\n',
                                          ic$a, ic$g, 100 * ic$a, 100 * (ic$a + ic$g * .3)))
            oos[i, `:=`(ic.a = ic$a, ic.g = ic$g)]
            oos[i, `:=`(pf0 = pf0 + inc * (ic.a + ic.g * im(i)), pf = pf0 + inc * (ic.a + ic.g * im(i)))]
        }
        if(USE_TOP2){
            ## top-two primary: the district's primary D share minus the forecast, centered on the cycle's mean
            ## gap (the primary electorate's overall lean differs from November's). Weight tuned on earlier cycles.
            g.of <- function(j) { g <- (oos$prim2[j] - .5) - oos$pf0[j]; ok <- !is.na(g); g[ok] <- g[ok] - mean(g[ok]); fcoalesce(g, 0) }
            oos[i, t2.g := g.of(i)]
            pastP <- intersect(seq(2002, T - 2, 2), unique(oos[t2.g != 0 & cycle >= 2012]$cycle))
            pcP <- lapply(pastP, function(c) { j <- which(oos$cycle == c); pc <- poll.avg(c, bp$hl, sponsor.bias(c), bp$pw, bp$po)
                                               list(j = j, pa = pc[match(oos$rn[j], pc$distcyc)], base = oos$pf0[j] - oos$t2.w[j] * oos$t2.g[j], g = oos$t2.g[j]) })
            wgrid2 <- seq(0, 1, .05)
            w2 <- if(!length(pastP)) 0 else wgrid2[which.min(sapply(wgrid2, function(w)
                sum(sapply(pcP, function(z) sum((blend(z$base + w * z$g, z$pa, bp$k) - oos$y[z$j])^2)))))]
            if(T == FORECAST) cat(sprintf('top-two primary weight for 2026: %.2f (tuned on %s; %d districts with a primary)\n', w2, paste(pastP, collapse = ', '), sum(oos$t2.g[i] != 0)))
            oos[i, t2.w := w2]
            oos[i, `:=`(pf0 = pf0 + t2.w * t2.g, pf = pf0 + t2.w * t2.g)]
        }
        if(PDEMO && length(ACS.F)){
            ## weight on the demographic shift, tuned on earlier cycles (2012+)
            sh.of <- function(c) { j <- which(oos$cycle == c); demo.shift(copy(oos[j])[, pf := pf0], poll.avg(c, bp$hl, sponsor.bias(c), bp$pw, bp$po)) }
            past <- if(T >= 2014) intersect(seq(2012, T - 2, 2), unique(oos$cycle)) else integer(0)
            wgrid <- c(0, .25, .5, .75, 1)
            sse <- if(!length(past)) rep(0, length(wgrid)) else sapply(wgrid, function(wt) sum(sapply(past, function(c){
                j <- which(oos$cycle == c); pc <- poll.avg(c, bp$hl, sponsor.bias(c), bp$pw, bp$po); kc <- match(oos$rn[j], pc$distcyc)
                sum((blend(oos$pf0[j] + wt * sh.of(c), pc[kc], bp$k) - oos$y[j])^2) })))
            wt <- if(length(past)) wgrid[which.min(sse)] else 0
            ## scenario override for the forecast cycle only (e.g. full weight on the 2026 demographic signal)
            if(T == FORECAST && Sys.getenv('V3_DEMO_W26') != '') wt <- as.numeric(Sys.getenv('V3_DEMO_W26'))
            oos[i, `:=`(demo.sh = sh.of(T), demo.w = wt)]
            oos[i, pf := pf0 + wt * demo.sh]
        }
        oos[i, `:=`(pp = blend(pf, pa[k], bp$k), has.poll = !is.na(k), poll = pa$poll[k], n.eff = pa$n.eff[k])]
        if(OPENTRAIT){
            ## V3_OPENTRAIT: candidate traits the residual trees under-use in open seats (2026-10-26 miss analysis): the
            ## Democrat's moderation relative to the Republican (|ext| R - |ext| D, CFscore distance from the party mean) and a
            ## Hispanic candidate (D - R). Slopes from earlier cycles' final errors in competitive open seats, within-cycle
            ## demeaned (the layer cannot move a cycle's average), applied to this cycle's competitive open seats.
            oos[i, `:=`(tr.mod = -(abs(fcoalesce(dem.ext, 0)) - abs(fcoalesce(rep.ext, 0))),
                        tr.hisp = fcoalesce(as.numeric(dem.hisp), 0) - fcoalesce(as.numeric(rep.hisp), 0), pp.nt = pp, ot.sh = 0)]
            pt <- oos[cycle < T & cycle >= 2006 & !is.na(y) & inc == 0 & abs(pp.nt) < .1]
            if(uniqueN(pt$cycle) >= 3){
                pt[, `:=`(e = (y - pp.nt) - mean(y - pp.nt), m = tr.mod - mean(tr.mod), h = tr.hisp - mean(tr.hisp)), by = cycle]
                b <- coef(lm(e ~ 0 + m + h, pt)); b[is.na(b)] <- 0
                sel <- i[oos$inc[i] == 0 & abs(oos$pp[i]) < .1]
                if(length(sel) > 1){
                    sh <- b[['m']] * (oos$tr.mod[sel] - mean(oos$tr.mod[sel])) + b[['h']] * (oos$tr.hisp[sel] - mean(oos$tr.hisp[sel]))
                    oos[sel, `:=`(ot.sh = sh, pp = pp + sh)]
                }
                if(T == FORECAST) cat(sprintf('open-seat trait layer for 2026: moderation %+.4f, Hispanic %+.4f per unit (from %d races); %d seats shifted, mean |shift| %.2f pts\n',
                                              b[['m']], b[['h']], nrow(pt), length(sel), 100 * mean(abs(oos$ot.sh[sel]))))
            }
        }
        if(L2LAYER != '' && 'l2.marg' %in% names(oos)){
            ## within-cycle z-scores (dob_gap districts excluded and never shifted); errors and terms demeaned within cycle,
            ## so the layer cannot move a cycle's average; slopes from earlier cycles' final errors (2012+, >= 3 cycles);
            ## shifts capped at +-2 pts
            if(!'l2.zm' %in% names(oos)){
                oos[, ok.l2 := !is.na(l2.marg) & !is.na(l2.po) & !(l2.gap %in% TRUE)]
                oos[(ok.l2), `:=`(l2.zm = (l2.marg - mean(l2.marg)) / sd(l2.marg), l2.zp = (l2.po - mean(l2.po)) / sd(l2.po)), by = cycle]
            }
            oos[i, `:=`(pp.l2 = pp, l2.sh = 0, l2.ta = fcoalesce(l2.zm * env.hat, 0), l2.tb = fcoalesce(l2.zp * as.numeric(cycle %% 4 == 2), 0))]
            tv <- switch(L2LAYER, A = 'l2.ta', B = 'l2.tb', C = c('l2.ta', 'l2.tb'))
            pt <- oos[cycle < T & cycle >= 2012 & !is.na(y) & ok.l2 %in% TRUE & !is.na(pp.l2)]
            if(uniqueN(pt$cycle) >= 3){
                pt[, e := (y - pp.l2) - mean(y - pp.l2), by = cycle]; for(v in tv) pt[, (v) := get(v) - mean(get(v)), by = cycle]
                b <- coef(lm(reformulate(tv, 'e', intercept = FALSE), pt)); b[is.na(b)] <- 0
                sel <- i[oos$ok.l2[i] %in% TRUE]
                if(length(sel) > 1){
                    X <- as.matrix(oos[sel, ..tv]); X <- sweep(X, 2, colMeans(X))
                    sh <- pmin(pmax(as.numeric(X %*% b[tv]), -.02), .02)
                    oos[sel, `:=`(l2.sh = sh, pp = pp + sh)]
                }
                if(T == FORECAST) cat(sprintf('L2 layer %s for 2026: slopes %s; %d seats, mean |shift| %.2f pts\n', L2LAYER,
                                              paste(sprintf('%s %+.4f', tv, b[tv]), collapse = ', '), length(sel), 100 * mean(abs(oos$l2.sh[sel]))))
            }
        }
    }
    for(T in c(seq(2002, 2004, 2))) { i <- which(oos$cycle == T); oos[i, pp := pf] }
    if(SCANDAL){
        ## dossiers (close races 2018-24, competitive 2026): scandal known by Oct 9, by party.
        ## Effect on the model's error is estimated from earlier cycles' dossier races only.
        dz <- rbindlist(lapply(c(2018, 2020, 2022, 2024, 2026), function(y)
            fread(file.path(PROJ, sprintf('data/dossiers/%d/dossier_%d.csv', y, y)), colClasses = 'character')), fill = TRUE)
        dz <- dz[chamber == 'House'][!duplicated(distcyc)]
        dz[, known := is.na(scandal_known_by_oct9) | scandal_known_by_oct9 %in% c('TRUE', 'True', '') | grepl('^2026', distcyc)]
        dz[, `:=`(d.sc = as.numeric(scandal_party %in% c('D', 'both') & known), r.sc = as.numeric(scandal_party %in% c('R', 'both') & known))]
        oos[, `:=`(in.dos = rn %in% dz$distcyc, d.sc = fcoalesce(dz$d.sc[match(rn, dz$distcyc)], 0), r.sc = fcoalesce(dz$r.sc[match(rn, dz$distcyc)], 0))]
        oos[, pp.nosc := pp]
        for(T in c(2020, 2022, 2024, FORECAST)){
            e <- oos[in.dos & cycle < T & cycle >= 2018]
            b <- coef(lm(I(y - pp.nosc) ~ d.sc + r.sc, e))
            b[is.na(b)] <- 0
            i <- which(oos$cycle == T)
            oos[i, pp := pp.nosc + SCANDAL_SHRINK * (b['d.sc'] * d.sc + b['r.sc'] * r.sc)]
            if(T == FORECAST) cat(sprintf('scandal adjustment for 2026: D scandal %+.2f pts, R scandal %+.2f pts (x%.1f shrink); races flagged: %d\n',
                                          100 * b['d.sc'], 100 * b['r.sc'], SCANDAL_SHRINK, oos[i][d.sc + r.sc > 0, .N]))
        }
    }
    ## recalibration: predictions are too spread out (actual ~ predicted slope < 1).
    ## Shrink each cycle's predictions with y ~ pp fit on earlier cycles' OOS predictions.
    if(RECAL){
        oos[, pp.raw := pp]
        for(T in c(BT_CYCLES, FORECAST)){
            cal <- lm(y ~ pp.raw, oos[cycle < T & cycle >= max(2002, T - 2 * RECAL_N)])
            i <- which(oos$cycle == T); oos[i, pp := predict(cal, oos[i])]
        }
    }
    scs <- list()
    for(T in c(BT_CYCLES, FORECAST)){
        ## scales from cycles that get the post-fit layers (2006 on; 2002/04 are stage-2 output only, and their national
        ## errors swing with small perturbations: 2002 +1.0 -> +2.0 pts on 2026-10-21), once two such cycles exist
        ## V3_SCALE_FROM (experiment 2026-10-23; default 2006): first cycle of the error-scale window once 2 cycles past it exist
        sf <- as.numeric(Sys.getenv('V3_SCALE_FROM', 2006))
        sc <- err.scales(oos, if(T >= sf + 4) seq(sf, T - 2, 2) else seq(2002, T - 2, 2)); scs[[as.character(T)]] <- sc   ## raw scales (before any multiplier)
        if(DSCALE_ROLL){
            ## multiplier on the district scale, re-chosen each cycle to minimise the Brier score on earlier backtest
            ## cycles (normal approximation, each past cycle under its own raw scales); SCALE_D until 2 cycles exist
            past <- BT_CYCLES[BT_CYCLES < T]; m <- SCALE_D
            if(length(past) >= 2){ g <- seq(.4, 1.4, .05)
                br <- sapply(g, function(m) sum(sapply(past, function(c){ d <- oos[cycle == c & !is.na(y)]; s0 <- scs[[as.character(c)]]
                    p <- pnorm(d$pp / sqrt(s0$nat^2 + (m * sd.h(d, s0))^2)); sum((p - (d$y > 0))^2) })))
                m <- g[which.min(br)] }
            sc$d.poll <- sc$d.poll * m; sc$d.nopoll <- sc$d.nopoll * m; sc$mult <- m
            if(T == FORECAST) cat(sprintf('district error-scale multiplier for 2026: %.2f\n', m))
        }
        i <- which(oos$cycle == T); sm <- simulate(oos[i], sc, nat.z = if(T == FORECAST) nat.z.2026())
        if(T == FORECAST) sc26 <- sc
        if(T == FORECAST) cat(sprintf('2026 error scales: national %.3f, district %.3f/%.3f (polled/not), state share %.2f\n',
                                      sc$nat, sc$d.poll, sc$d.nopoll, sc$rho))
        if(T == FORECAST && !is.null(sc$corr)) cat(sprintf('2026 correlation shares: state %.2f, division %.2f, demographic %s\n',
                                      sc$corr$rho.s, sc$corr$rho.r, paste(sprintf('%s %.3f', names(sc$corr$tau), sc$corr$tau), collapse = ', ')))
        oos[i, prob := sm$prob]; seat.dist[[as.character(T)]] <- sm$seats
    }
    if(CLASS){
        ## logistic stacking of the simulation probability with race-level signals, fit on earlier cycles
        oos[, prob.sim := prob]
        lg <- function(p) qlogis(pmin(pmax(p, .001), .999))
        for(T in c(BT_CYCLES[BT_CYCLES >= 2014], FORECAST)){
            e <- oos[cycle < T & cycle >= 2012 & !is.na(y)]
            m <- glm(I(y > 0) ~ lg(prob.sim) + fcoalesce(rating, 0) + has.poll + fcoalesce(as.numeric(top2), 0), binomial, e)
            i <- which(oos$cycle == T)
            oos[i, prob := predict(m, oos[i], type = 'response')]
        }
    }
    bt <- oos[cycle %in% BT_CYCLES]
    ## backtest summary: fundamentals vs with polls; Brier; seat interval coverage
    sm <- bt[, .(n = .N, rmse.fund = sqrt(mean((pf - y)^2)), rmse = sqrt(mean((pp - y)^2)),
                 miss.fund = sum(sign(pf) != sign(y)), miss = sum(sign(pp) != sign(y)),
                 brier = mean((prob - (y > 0))^2), pred.seats = round(sum(prob)), act.seats = sum(y > 0)), by = cycle]
    sm[, `:=`(lo90 = sapply(cycle, function(c) quantile(seat.dist[[as.character(c)]], .05)),
              hi90 = sapply(cycle, function(c) quantile(seat.dist[[as.character(c)]], .95)),
              lo50 = sapply(cycle, function(c) quantile(seat.dist[[as.character(c)]], .25)),
              hi50 = sapply(cycle, function(c) quantile(seat.dist[[as.character(c)]], .75)),
              ## seat-count calibration: PIT of the actual count in the simulated distribution, and CRPS
              pit = sapply(seq_len(.N), function(j) mean(seat.dist[[as.character(cycle[j])]] <= act.seats[j])),
              crps = sapply(seq_len(.N), function(j) crps(seat.dist[[as.character(cycle[j])]], act.seats[j])))]
    ## uncontested seats (no major-party opponent) are certain calls the models do not fit: counted in the all-race accuracy
    ## (n.all, acc.all, brier.all) and added to the seat totals so they match the chamber. Seat coverage, PIT and CRPS are
    ## unchanged by that fixed shift (they were computed above on the modeled races).
    unc <- H[cycle %in% BT_CYCLES & !(rn %in% bt$rn) & (uncontested.rep %in% 1 | uncontested.dem %in% 1),
             .(unc.d = sum(uncontested.rep %in% 1), unc.r = sum(uncontested.dem %in% 1)), by = cycle]
    sm[unc, on = 'cycle', `:=`(unc.d = i.unc.d, unc.r = i.unc.r)]; sm[is.na(unc.d), `:=`(unc.d = 0L, unc.r = 0L)]
    sm[, `:=`(n.all = n + unc.d + unc.r, acc.all = 1 - miss / (n + unc.d + unc.r), brier.all = brier * n / (n + unc.d + unc.r),
              pred.seats = pred.seats + unc.d, act.seats = act.seats + unc.d, lo90 = lo90 + unc.d, hi90 = hi90 + unc.d,
              lo50 = lo50 + unc.d, hi50 = hi50 + unc.d)]
    print(sm)
    seat.m <- sm[, .(cov90 = sum(act.seats >= lo90 & act.seats <= hi90), cov50 = sum(act.seats >= lo50 & act.seats <= hi50), n = .N,
                     crps = mean(crps), width90 = mean(hi90 - lo90), seat.mae = mean(abs(pred.seats - act.seats)),
                     crps.1824 = mean(crps[cycle >= 2018]), cov90.1824 = sum((act.seats >= lo90 & act.seats <= hi90)[cycle >= 2018]),
                     cov50.1824 = sum((act.seats >= lo50 & act.seats <= hi50)[cycle >= 2018]), width90.1824 = mean((hi90 - lo90)[cycle >= 2018]))]
    cat(sprintf('SEATS: 90%% coverage %d/%d, 50%% coverage %d/%d, mean CRPS %.2f, mean 90%% width %.1f, seat MAE %.1f; 2018-24: CRPS %.2f, 90%% %d/4, 50%% %d/4, width %.1f; PIT %s\n',
                seat.m$cov90, seat.m$n, seat.m$cov50, seat.m$n, seat.m$crps, seat.m$width90, seat.m$seat.mae,
                seat.m$crps.1824, seat.m$cov90.1824, seat.m$cov50.1824, seat.m$width90.1824, paste(sprintf('%.2f', sm$pit), collapse = ' ')))
    print(bt[, .(rmse.fund = sqrt(mean((pf - y)^2)), rmse = sqrt(mean((pp - y)^2)), misses = sum(sign(pp) != sign(y)),
                 acc = mean(sign(pp) == sign(y)), brier = mean((prob - (y > 0))^2))])
    r <- bt[cycle >= 2018, .(rmse = sqrt(mean((pp - y)^2)), misses = sum(sign(pp) != sign(y)),
                             brier = mean((prob - (y > 0))^2), seat.mae = mean(abs(sum(prob) - sum(y > 0))))]
    cat(sprintf('RECENT2018_24 rmse=%.4f misses=%d brier=%.4f\n', r$rmse, r$misses, r$brier))
    log.score('House', bt, house.config(), extra = seat.m[, .(crps.seat = round(crps, 3), cov90.seat = cov90, cov50.seat = cov50, width90.seat = round(width90, 1),
                                                           crps.seat.1824 = round(crps.1824, 3), cov90.seat.1824 = cov90.1824, cov50.seat.1824 = cov50.1824)])
    cal <- bt[, .(n = .N, mean.prob = mean(prob), dem.won = mean(y > 0)), by = .(bin = cut(prob, include.lowest = TRUE, c(0, .05, .15, .3, .5, .7, .85, .95, 1)))][order(bin)]
    print(cal)
    ## calibration by era: the pooled table hides the recent-era spread
    print(bt[prob > .05 & prob < .95, .(n = .N, mean.prob = round(mean(prob), 3), dem.won = round(mean(y > 0), 3)),
             by = .(era = fifelse(cycle >= 2018, '2018-24', '2006-16'), bin = cut(prob, c(.05, .3, .5, .7, .95)))][order(era, bin)])
    fwrite(sm, file.path(OUT, 'house_backtest_by_cycle.csv')); fwrite(cal, file.path(OUT, 'house_calibration.csv'))
    saveRDS(list(oos = oos, seat.dist = seat.dist, blend.par = blend.par), file.path(OUT, 'house_oos.rds'))
    if(Sys.getenv('V3_TAG') != '') saveRDS(oos, file.path(OUT, sprintf('oos_%s.rds', Sys.getenv('V3_TAG'))))

    ## 2026 forecast: contested seats from the model; the rest called directly
    f <- H[cycle == FORECAST]
    f <- merge(f, oos[cycle == FORECAST, .(rn, pf, pp, prob, poll, n.eff)], by = 'rn', all.x = TRUE)
    f[, call.basis := fcase(contested26, 'model',
                            nominee.unresolved, 'LA jungle primary: presidential vote',
                            uncontested.rep == 1 & !is.na(dem.name), 'no Republican on the ballot',
                            uncontested.dem == 1 & !is.na(rep.name), 'no Democrat on the ballot',
                            default = 'other')]
    f[call.basis == 'no Republican on the ballot', prob := 1]
    f[call.basis == 'no Democrat on the ballot', prob := 0]
    ## Louisiana (Nov 3 jungle primary, Dec 12 runoff): fundamentals from the
    ## national/lean/incumbency stage, error from the 2026 scales
    s1.26 <- stage1(H[scored & cycle < FORECAST], H[scored & cycle < FORECAST & cycle >= MIN_TRAIN], FORECAST, TAU, F2)
    fe26 <- predict(s1.26$m2, unique(H[cycle == FORECAST, intersect(NAT.V, names(H)), with = FALSE]))[1]
    sd26 <- sqrt(sc26$nat^2 + sc26$d.nopoll^2)
    f[call.basis == 'LA jungle primary: presidential vote',
      `:=`(call.basis = 'LA jungle primary: fundamentals (party vote share; runoffs historically won by R)',
           prob = pnorm((s1.26$a * lean + s1.26$b * inc + (if(is.null(s1.26$c)) 0 else s1.26$c) * inc * abs(lean) + (if(is.null(s1.26$a3)) 0 else s1.26$a3) * lean^3 + fe26) / sd26))]
    stopifnot(!anyNA(f$prob))
    fixed <- f[call.basis != 'model']
    nsim <- length(seat.dist[[as.character(FORECAST)]])
    unc <- fixed[prob > 0 & prob < 1]
    dem.seats <- seat.dist[[as.character(FORECAST)]] + sum(fixed$prob == 1) +
                 if(nrow(unc)) colSums(matrix(rbinom(nrow(unc) * nsim, 1, rep(unc$prob, nsim)), nrow(unc))) else 0
    print(f[call.basis != 'model' & prob > 0 & prob < 1, .(rn, call.basis, dist.pres.vs, p = round(prob, 3))])
    f[, `:=`(pred.dvs = round(.5 + fcoalesce(pp, NA_real_), 4), fund.dvs = round(.5 + pf, 4),
             poll.dvs = round(.5 + poll, 4), p.dem = round(prob, 3),
             rating = cut(prob, c(-Inf, .05, .2, .4, .6, .8, .95, Inf),
                          c('Safe R', 'Likely R', 'Lean R', 'Toss-up', 'Lean D', 'Likely D', 'Safe D')))]
    out <- f[, .(distcyc = rn, dem.name, rep.name, inc.status, dist.pres.vs, fund.dvs, poll.dvs, n.polls.asof,
                 pred.dvs, p.dem, rating, call.basis, forecast.note)][order(-p.dem)]
    out[distcyc == '2026_CA_6', rep.name := 'Kevin Kiley (I, ex-R incumbent)']
    fwrite(out, file.path(OUT, 'house_2026_forecast.csv'))
    q <- quantile(dem.seats, c(.05, .5, .95))
    ## conformal check: 90% interval from the backtest's seat errors (actual - simulated median)
    se <- sapply(BT_CYCLES, function(c) { s0 <- seat.dist[[as.character(c)]]; sum(oos[cycle == c]$y > 0) - median(s0) })
    qc <- quantile(abs(se), .9)
    cat(sprintf('conformal 90%% seat interval (from %d backtest cycles): %d-%d\n', length(se), round(q[2] - qc), round(q[2] + qc)))
    cat(sprintf('\n2026 House: median D seats %d (90%%: %d-%d); P(D majority, 218+) = %.2f\n',
                as.integer(round(q[2])), as.integer(round(q[1])), as.integer(round(q[3])), mean(dem.seats >= 218)))
    print(out[, .N, by = rating][order(rating)])
    saveRDS(dem.seats, file.path(OUT, 'house_2026_seat_sims.rds'))
}
