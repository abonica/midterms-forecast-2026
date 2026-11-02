###############################################################################
## features_v3.R: external feature blocks for forecast_v3.R, sourced right before STAGE2 is built.
## Each block is behind a V3_* flag (all off by default), joins its file onto H by distcyc or by
## (rid, cycle) for each slot, builds D/R contrasts, and appends the feature names to EXT.F.
## Money-type contrasts are centered within cycle like the m.* features. Files and build scripts:
##   data/fec/fec_asof_<MMDD>.csv          src/build_fec_asof.R         FEC Form 3 report summaries, as of the cutoff
##   data/ie/ie_asof_<MMDD>.csv            src/build_ie_asof.R          independent expenditures for/against, as of the cutoff
##   data/donor/donor_features_<MMDD>.csv  src/build_donor_features.R   smart money, crossover donors, momentum
##   data/redistricting/incumbent_overlap.csv  src/build_incumbent_overlap.R  share of the district the incumbent held before
###############################################################################
MMDD <- gsub('-', '', CUTOFF.MD)                      ## '0930' for the production money cutoff
ctr <- function(v) H[, (v) := get(v) - median(get(v)[scored | contested26], na.rm = TRUE), by = cycle]   ## center within cycle
slot.join <- function(f, cols, prefix){
    ## join a (rid, cycle) file onto both slots: <prefix>.dem.<col>, <prefix>.rep.<col>
    d <- fread(f)
    for(s in c('dem', 'rep')){
        k <- match(paste(H[[paste0(s, '.rid.static')]], H$cycle), paste(d$rid, d$cycle))
        for(v in cols) set(H, j = paste(prefix, s, v, sep = '.'), value = d[[v]][k])
    }
}

## V3_FEC: money levels from FEC report summaries instead of DIME transaction sums (consistent across eras, no
## conduit or PAC double count), plus unitemized receipts and debts, which DIME lacks
if(Sys.getenv('V3_FEC', '0') == '1'){
    ff <- file.path(PROJ, sprintf('data/fec/fec_asof_%s.csv', MMDD))
    if(file.exists(ff)){
        slot.join(ff, c('receipts', 'indiv', 'unitemized', 'pac', 'party', 'cand', 'disb', 'cash', 'debts'), 'fec')
        H[, `:=`(f.tot = ld(fec.dem.receipts, fec.rep.receipts), f.ind = ld(fec.dem.indiv, fec.rep.indiv),
                 f.unit = ld(fec.dem.unitemized, fec.rep.unitemized), f.pac = ld(fec.dem.pac, fec.rep.pac),
                 f.cash = ld(fec.dem.cash, fec.rep.cash), f.debt = ld(fec.dem.debts, fec.rep.debts),
                 f.disb = ld(fec.dem.disb, fec.rep.disb))]
        FEC.F <- c('f.tot', 'f.ind', 'f.unit', 'f.pac', 'f.cash', 'f.debt', 'f.disb')
        for(v in FEC.F) ctr(v)
        ## V3_FEC_REPLACE=1: the FEC levels replace the DIME m.tot/m.ind/m.pac/m.cash/m.disb (dropped from STAGE2 by name)
        if(Sys.getenv('V3_FEC_REPLACE', '0') == '1') money <- setdiff(money, c('m.tot', 'm.ind', 'm.pac', 'm.cash', 'm.disb'))
        EXT.F <- c(EXT.F, FEC.F)
        cat(sprintf('FEC summaries: %s; coverage %s\n', basename(ff), paste(H[cycle >= 2004 & (scored | contested26), sprintf('%d %.0f%%', cycle, 100 * mean(!is.na(f.tot))), by = cycle]$V1, collapse = ', ')))
    } else warning('V3_FEC: no file ', ff)
}

## V3_IE: independent expenditures (super PACs, parties' IE arms) for and against each candidate, as of the cutoff.
## ie.net: pro-D outside money (for D + against R) vs pro-R (for R + against D), log ratio; ie.tot: log total IE in the race
if(Sys.getenv('V3_IE', '0') == '1'){
    fi <- file.path(PROJ, sprintf('data/ie/ie_asof_%s.csv', MMDD))
    if(file.exists(fi)){
        slot.join(fi, c('support', 'oppose'), 'ie')
        H[, `:=`(ie.net = ld(fcoalesce(ie.dem.support, 0) + fcoalesce(ie.rep.oppose, 0), fcoalesce(ie.rep.support, 0) + fcoalesce(ie.dem.oppose, 0)),
                 ie.tot = log1p(fcoalesce(ie.dem.support, 0) + fcoalesce(ie.rep.oppose, 0) + fcoalesce(ie.rep.support, 0) + fcoalesce(ie.dem.oppose, 0)))]
        H[cycle < 2010, c('ie.net', 'ie.tot') := NA_real_]      ## no IE file before 2010
        ctr('ie.net')
        EXT.F <- c(EXT.F, 'ie.net', 'ie.tot')
        cat(sprintf('IE: %s; races with any IE %s\n', basename(fi), paste(H[cycle >= 2010 & (scored | contested26), sprintf('%d %.0f%%', cycle, 100 * mean(ie.tot > 0, na.rm = TRUE)), by = cycle]$V1, collapse = ', ')))
    } else warning('V3_IE: no file ', fi)
}

## V3_PLACEBO10 (experiment 2026-10-23, off): a column that is 0 from 2010 on and NA before. Any feature that is NA
## before 2010 (no IE file) also hands the residual models this era split, which alone moved 2012 RMSE .0384 -> .0327
## and seat CRPS 2.82 -> 2.57 (late-IE deep dive), so NA-before-2010 features are judged against this placebo
if(Sys.getenv('V3_PLACEBO10', '0') == '1'){ H[, placebo10 := fifelse(cycle >= 2010, 0, NA_real_)]; EXT.F <- c(EXT.F, 'placebo10') }
## V3_ERA10 (experiment 2026-10-23, off): the same split as a named 0/1 indicator (cycle >= 2010: post-Citizens United
## money, the conduit era, nationalization), so each learner sees it as a value rather than through its NA handling
if(Sys.getenv('V3_ERA10', '0') == '1'){ H[, era10 := as.numeric(cycle >= 2010)]; EXT.F <- c(EXT.F, 'era10') }

## V3_LIE (experiment 2026-10-23, off): late-IE component features as of the Oct 18 forecast (disseminated and received
## by Oct 18; blank-id lines mapped; $SML_SCRATCH/exp_lateie_deep_2026-10-23/build_ie_lines.R -> ie_feat_oct18.csv).
## V3_LIE=<comma list of columns>; NA before 2010 (no IE file), so compare with V3_PLACEBO10
LIE <- Sys.getenv('V3_LIE', '')
if(LIE != ''){
    lf <- Sys.getenv('V3_LIE_FILE', file.path(Sys.getenv('SML_SCRATCH', '/ssd2/bonica/sml_v2'), 'exp_lateie_deep_2026-10-23/ie_feat_oct18.csv'))
    lv <- strsplit(LIE, ',')[[1]]
    d <- fread(lf); k <- match(H$rn, d$distcyc)
    for(v in lv){ set(H, j = v, value = fcoalesce(as.numeric(d[[v]][k]), 0)); H[cycle < 2010, (v) := NA_real_] }
    EXT.F <- c(EXT.F, lv)
    cat(sprintf('LIE: %s from %s\n', LIE, basename(lf)))
}

## V3_DONOR: leak-free donor features from the DIME transactions (data/donor/README.md)
if(Sys.getenv('V3_DONOR', '0') == '1'){
    fd <- file.path(PROJ, sprintf('data/donor/donor_features_%s.csv', MMDD))
    if(file.exists(fd)){
        slot.join(fd, c('smart.money.w', 'smart.sh.hist', 'cross.sh.n', 'mom.q3q2', 'mom.new.sh', 'n.unq.donors.cf', 'party.net.sh'), 'dn')
        H[, `:=`(dn.smart = dn.dem.smart.money.w - dn.rep.smart.money.w,        ## donors' track record, D minus R
                 dn.cross = dn.dem.cross.sh.n - dn.rep.cross.sh.n,              ## crossover donor share, D minus R
                 dn.mom = dn.dem.mom.q3q2 - dn.rep.mom.q3q2,                    ## Q3 vs Q2 donor growth, D minus R
                 dn.pnet = dn.dem.party.net.sh - dn.rep.party.net.sh,
                 ## CFscore reliability: extremity weighted by how many scored donors it rests on
                 dem.ext.w = dem.ext * pmin(1, log1p(fcoalesce(as.numeric(dn.dem.n.unq.donors.cf), 0)) / log1p(200)),
                 rep.ext.w = rep.ext * pmin(1, log1p(fcoalesce(as.numeric(dn.rep.n.unq.donors.cf), 0)) / log1p(200)))]
        DN.F <- c('dn.smart', 'dn.cross', 'dn.mom', 'dn.pnet', 'dem.ext.w', 'rep.ext.w')
        for(v in c('dn.smart', 'dn.cross', 'dn.mom', 'dn.pnet')) ctr(v)
        EXT.F <- c(EXT.F, DN.F)
        cat(sprintf('donor features: %s; smart-money coverage %s\n', basename(fd), paste(H[cycle >= 2004 & (scored | contested26), sprintf('%d %.0f%%', cycle, 100 * mean(!is.na(dn.smart))), by = cycle]$V1, collapse = ', ')))
    } else warning('V3_DONOR: no file ', fd)
}

## V3_MTHRESH: money as a threshold. The partial dependence (2026-10-21) is flat across the middle of every money
## ratio and moves only when one side out-raises the other by about 5:1 (|log ratio| > 1.6). 'add' appends the
## lopsided part of the main ratios (sign x excess over 1.6) and a 0/1 flag for each side being out-raised 5:1;
## 'replace' also drops the raw m.tot, m.ind, m.disb and m.q6 ratios, leaving only the thresholded versions
MT <- Sys.getenv('V3_MTHRESH', '0')
if(MT %in% c('add', 'replace')){
    lop <- function(m, k = 1.6) sign(m) * pmax(abs(m) - k, 0)
    H[, `:=`(m.tot.lop = lop(m.tot), m.ind.lop = lop(m.ind), m.q6.lop = lop(m.q6), m.disb.lop = lop(m.disb),
             dem.unfunded = as.numeric(m.tot < -1.6), rep.unfunded = as.numeric(m.tot > 1.6))]
    MT.F <- c('m.tot.lop', 'm.ind.lop', 'm.q6.lop', 'm.disb.lop', 'dem.unfunded', 'rep.unfunded')
    if(MT == 'replace') money <- setdiff(money, c('m.tot', 'm.ind', 'm.disb', 'm.q6'))
    EXT.F <- c(EXT.F, MT.F)
    cat(sprintf('money thresholds (%s): %.0f%% of scored races lopsided beyond 5:1\n', MT, 100 * mean(abs(H$m.tot[H$scored]) > 1.6, na.rm = TRUE)))
}

## V3_SCANDAL_FEAT: scandals as residual-model features, from the dossiers (data/dossiers/<cycle>, close races
## 2018-24 and competitive 2026): sc.d / sc.r = 1 when that side's candidate had a scandal known by the cutoff
## (scandal_known_by_oct9), 0 for the other 2018+ races, NA before 2018 (no dossiers: unknown, not "none").
## Scandals that broke after Oct 9 are left at 0. Dossier counts: 2018 20 known, 2020 2, 2022 11, 2024 6, 2026 6.
## Adopted 2026-10-21 by the user's decision (default on): neutral on the backtest (2018-24 40 misses, Brier +.0001;
## 2006-24 -1 miss); the overlay's hand-set scandal shifts are zeroed so a scandal is not counted twice
## OFF since 2026-10-21 (bug review): the 2018-24 dossier races were chosen after the election (contest_list_2018_2024.csv,
## why = close_result / miss / big_error) and every other race is coded 0, so the flag learns 'scandal and the race went
## badly' partly from how races were picked; the 2026 dossiers are picked before the election. The overlay carries the
## House scandal shifts again. A leak-free version needs scandals coded for every race by a pre-election rule.
if(Sys.getenv('V3_SCANDAL_FEAT', '0') == '1'){
    dz <- rbindlist(lapply(c(2018, 2020, 2022, 2024, 2026), function(y){ f <- file.path(PROJ, sprintf('data/dossiers/%d/dossier_%d.csv', y, y))
        if(file.exists(f)) fread(f, colClasses = 'character') else NULL }), fill = TRUE)
    dz <- dz[chamber == 'House'][!duplicated(distcyc)]
    dz[, known := scandal_known_by_oct9 %in% c('TRUE', 'True')]   ## a missing date is not 'known by Oct 9'
    k <- match(H$rn, dz$distcyc)
    H[, `:=`(sc.d = fifelse(cycle >= 2018, fcoalesce(as.numeric(dz$scandal_party[k] %in% c('D', 'both') & dz$known[k]), 0), NA_real_),
             sc.r = fifelse(cycle >= 2018, fcoalesce(as.numeric(dz$scandal_party[k] %in% c('R', 'both') & dz$known[k]), 0), NA_real_))]
    EXT.F <- c(EXT.F, 'sc.d', 'sc.r')
    cat(sprintf('scandal features: %s\n', paste(H[cycle >= 2018 & (scored | contested26), sprintf('%d D %d R %d', cycle, sum(sc.d), sum(sc.r)), by = cycle]$V1, collapse = ', ')))
}

## V3_CES: voters' ideological placements of the candidates from the CCES/CES pre-election wave (data/ces/README.md).
##  insample: this cycle's placements (interviews through Oct 18) for both candidates: perceived extremity relative to
##            the party's mean placement that cycle (positive = further from the center than the typical nominee),
##            share of respondents able to place the candidate (name recognition), and placement minus the district's
##            mean self-placement. A hindsight test (the CES is released a year after the election; 2024 has incumbents
##            only and 2026 is NA), so it can inform but not feed the forecast.
##  lag:      the candidate's most recent earlier placement (2026: incumbents' 2024 member-item placement), relative to
##            the party mean of that cycle; leak-free and available for 2026.
CES <- Sys.getenv('V3_CES', '0')
if(CES %in% c('insample', 'lag')){
    cp <- fread(file.path(PROJ, 'data/ces/ces_perceptions.csv'))
    pm <- cp[item == 'cand' & !is.na(mean_place_oct18) & n_placed_oct18 >= 10, .(pmean = mean(mean_place_oct18)), by = .(cycle, party)]
    pm <- rbind(pm, cp[item == 'member' & cycle == 2024 & !is.na(mean_place_oct18), .(pmean = mean(mean_place_oct18)), by = .(cycle, party)])
    if(CES == 'insample'){
        cc <- cp[!is.na(rid) & rid != '' & !is.na(mean_place_oct18) & n_placed_oct18 >= 5][order(item != 'cand')][!duplicated(paste(cycle, rid))]
        cc[, pmean := pm$pmean[match(paste(cycle, party), paste(pm$cycle, pm$party))]]
        cc[, `:=`(ext = fifelse(party == 'D', pmean - mean_place_oct18, mean_place_oct18 - pmean), rec = share_placed_oct18,
                  dist = fifelse(party == 'D', self_mean_oct18 - mean_place_oct18, mean_place_oct18 - self_mean_oct18))]   ## dist: how far the candidate sits from the district, toward the party's side
        for(s in c('dem', 'rep')){ k <- match(paste(H[[paste0(s, '.rid.static')]], H$cycle), paste(cc$rid, cc$cycle))
            set(H, j = paste0('ces.', s, '.ext'), value = cc$ext[k]); set(H, j = paste0('ces.', s, '.rec'), value = cc$rec[k]); set(H, j = paste0('ces.', s, '.dist'), value = cc$dist[k]) }
        CES.F <- c('ces.dem.ext', 'ces.rep.ext', 'ces.dem.rec', 'ces.rep.rec', 'ces.dem.dist', 'ces.rep.dist')
    } else {
        lg <- fread(file.path(PROJ, 'data/ces/ces_perceptions_lag.csv'))[!is.na(lag_mean_place)]
        lg[, pmean := pm$pmean[match(paste(lag_cycle, party), paste(pm$cycle, pm$party))]]
        lg[, ext := fifelse(party == 'D', pmean - lag_mean_place, lag_mean_place - pmean)]
        for(s in c('dem', 'rep')){ k <- match(paste(H[[paste0(s, '.rid.static')]], H$cycle), paste(lg$rid, lg$cycle))
            set(H, j = paste0('ces.', s, '.ext'), value = lg$ext[k]); set(H, j = paste0('ces.', s, '.rec'), value = lg$lag_share_placed[k]) }
        CES.F <- c('ces.dem.ext', 'ces.rep.ext', 'ces.dem.rec', 'ces.rep.rec')
    }
    H[, `:=`(ces.dem.has = as.numeric(!is.na(ces.dem.ext)), ces.rep.has = as.numeric(!is.na(ces.rep.ext)))]
    CES.F <- c(CES.F, 'ces.dem.has', 'ces.rep.has')
    EXT.F <- c(EXT.F, CES.F)
    cat(sprintf('CES perceptions (%s): coverage of scored/contested slots %s\n', CES, paste(H[cycle >= 2006 & (scored | contested26), sprintf('%d %.0f%%', cycle, 100 * mean(c(ces.dem.has, ces.rep.has))), by = cycle]$V1, collapse = ', ')))
}

## V3_HISP: Hispanic candidates (PROVISIONAL surname list, to be replaced by the Census surname file). Diagnostic
## 2026-10-21: with cycle effects, a Hispanic Republican in a 75% Hispanic district beats the forecast by ~1.8 pts
## relative to a non-Hispanic Republican (interaction t = -4). Features: each side's flag and its product with the
## district's Hispanic share (ACS), so the trees see the co-ethnic effect where it applies.
## Adopted 2026-10-21 (default on): with the Census coding, 2018-24 Brier .0208 -> .0206, RMSE -.0002 (se .0001), seat CRPS
## 2.71 -> 2.67, misses 40 -> 42 (2006-24 one fewer); the 2024 hold-out cycle is unchanged on misses and slightly better on
## Brier. The user's observation that Hispanic Republicans beat the forecast is the motivation; 2026 moves CA-22, TX-34,
## TX-15 and FL-27 toward the Republican.
## V3_QC_DIME: prior elected office from DIME's candidate records (state legislative, statewide, judicial and local winners or
## incumbents, same bonica_rid) added to the quality-challenger flag, which otherwise rests on prior Congress/governor service
## and Vote Smart bio text (1.5-2.3% of non-incumbent nominees coded in 2012-26; DIME raises it to ~12%). Only offices won in
## an earlier cycle count.
if(Sys.getenv('V3_QC_DIME', '0') == '1'){
    ce <- new.env(); load(file.path(Sys.getenv('SML_SCRATCH', '/ssd2/bonica/sml_v2'), 'cands_79_26_pinned_20260922.rda'), ce)
    cd <- as.data.table(ce$cands)[(winner %in% 'W' | Incum_Chall %in% 'I') & grepl('^(state|local):', seat) & !grepl('committee', seat),
                                  .(first = min(as.integer(cycle))), by = bonica_rid]; rm(ce)
    for(s in c('dem', 'rep')){
        f <- cd$first[match(H[[paste0(s, '.rid.static')]], cd$bonica_rid)]; v <- paste0('quality.chall.', s)
        set(H, j = v, value = pmax(fcoalesce(as.numeric(H[[v]]), 0), as.numeric(!is.na(f) & f < H$cycle)))
    }
    cat(sprintf('quality challengers with DIME prior office: D %.1f%%, R %.1f%% of non-incumbent nominees (2012+)\n',
                100 * H[cycle >= 2012 & (scored | contested26) & inc != 1, mean(quality.chall.dem)], 100 * H[cycle >= 2012 & (scored | contested26) & inc != -1, mean(quality.chall.rep)]))
}

if(Sys.getenv('V3_HISP', '1') == '1' && 'pct_hispanic' %in% names(H)){
    hs <- c('garcia','rodriguez','martinez','hernandez','lopez','gonzalez','gonzales','perez','sanchez','ramirez','torres','flores','rivera','gomez','diaz','cruz','morales','reyes','gutierrez','ortiz','chavez','ruiz','alvarez','castillo','jimenez','vasquez','vazquez','moreno','romero','herrera','medina','aguilar','garza','castro','vargas','fernandez','guzman','munoz','mendoza','salazar','delgado','pena','rios','ortega','santiago','luna','gimenez','cuellar','salinas','cisneros','vega','soto','nunez','molina','carrillo','contreras','sandoval','guerrero','estrada','maldonado','dominguez','acosta','cabrera','campos','espinoza','figueroa','juarez','lara','mejia','miranda','navarro','padilla','pacheco','rojas','serrano','trujillo','valdez','velasquez','villarreal','zamora','marquez','barrera','cardenas','ayala','ochoa','rangel','hinojosa','benavides','cantu','villegas','villanueva','salas','solis','arroyo','beltran','montoya','orozco','quintero','calderon','cortez','duran','escobar','galvan','huerta','macias','mata','mercado','nava','ponce','robles','rosales','saenz','suarez','tapia','valencia','velez','zuniga','aguirre','andrade','avila','bautista','bravo','camacho','carrasco','cervantes','davila','esparza','fuentes','gallegos','ibarra','jaramillo','lozano','marin','montes','olivares','palacios','quinones','rocha','rubio','trevino','ventura','alvarado','arias','barajas','bustamante','cano','cordova','elizondo','franco','gallardo','guevara','ledesma','lugo','mendez','murillo','nieto','olvera','peralta','ramos','rendon','rosas','sierra','tovar','vela','villa','zavala','alonso','aranda','baca','cabral','casillas','castaneda','coronado','escamilla','garibay','jasso','lerma','lujan','meza','negrete','oliva','pantoja','quiroz','reyna','saldana','segura','terrazas','urbina','valle','vidal','yanez','ciscomani','grijalva','barragan','correa','valadao','curbelo','diaz-balart','balart','gonzalez-colon','gallego','espaillat','velazquez','ocasio-cortez','menendez','sires','roybal-allard','napolitano','costa')
    sur <- function(x){ x <- tolower(sub('\\(.*', '', iconv(x, 'UTF-8', 'ASCII//TRANSLIT')));   ## transliterate first: 2026 names carry accents (Sánchez)
    x <- sub(',.*', '', x); x <- trimws(gsub('[^a-z -]', '', x)); x <- sub(' (jr|sr|ii|iii|iv)$', '', x); sub('.* ', '', x) }
    ## Census 2010 surname file ($SML_SCRATCH/dl/census_surnames/Names_2010Census.csv, fetched 2026-10-21): share of
    ## people with the surname who are Hispanic; a candidate is coded Hispanic at >= 50% (hyphenated names: either part)
    cf <- file.path(Sys.getenv('SML_SCRATCH', '/ssd2/bonica/sml_v2'), 'dl/census_surnames/Names_2010Census.csv')
    if(file.exists(cf)){
        cs <- fread(cf, select = c('name', 'pcthispanic'))[, `:=`(nm = tolower(name), p = suppressWarnings(as.numeric(pcthispanic)) / 100)]
        ph <- function(x){ s <- sur(x); parts <- strsplit(s, '-', fixed = TRUE)
            sapply(seq_along(s), function(i){ v <- cs$p[match(c(s[i], parts[[i]]), cs$nm)]; if(all(is.na(v))) NA_real_ else max(v, na.rm = TRUE) }) }
        H[, `:=`(dem.hisp.p = ph(dem.name), rep.hisp.p = ph(rep.name))]
        ## hand overrides where the surname misleads (user's call, 2026-10-21): Valadao is Portuguese by surname but
        ## runs and is perceived as Latino in the Central Valley
        ovr <- c('valadao')
        H[, `:=`(dem.hisp = as.numeric(fcoalesce(dem.hisp.p, 0) >= .5 | sur(dem.name) %in% ovr), rep.hisp = as.numeric(fcoalesce(rep.hisp.p, 0) >= .5 | sur(rep.name) %in% ovr))]
        cat('Hispanic coding from the Census surname file, with overrides:', paste(ovr, collapse = ', '), '\n')
    } else H[, `:=`(dem.hisp = as.numeric(sur(dem.name) %in% hs), rep.hisp = as.numeric(sur(rep.name) %in% hs))]
    H[, `:=`(dem.hisp.x = dem.hisp * fcoalesce(pct_hispanic, 0), rep.hisp.x = rep.hisp * fcoalesce(pct_hispanic, 0))]
    EXT.F <- c(EXT.F, 'dem.hisp', 'rep.hisp', 'dem.hisp.x', 'rep.hisp.x')
    cat(sprintf('Hispanic candidates coded: D %d, R %d in 2012-26 scored/contested slots\n', H[cycle >= 2012 & (scored | contested26), sum(dem.hisp)], H[cycle >= 2012 & (scored | contested26), sum(rep.hisp)]))
}

## V3_RATING_MOVE: movement of the expert rating since Labor Day (data/expert_ratings/house_ratings_laborday.csv,
## built 2026-10-21 from the Wikipedia revisions in force on Labor Day; README_laborday.md). rating.move = the rating
## the model uses (Oct 18) minus the Labor Day rating, both the Cook/Inside/Sabato mean on the -1..1 scale, with
## unlisted pre-2020 races filled as Safe for the incumbent party as for the main rating. 2026 Missouri rows are
## NA: at Labor Day they were rated on the mid-decade map that was later struck.
if(Sys.getenv('V3_RATING_MOVE', '0') == '1' && 'rating' %in% names(H)){
    fl <- file.path(PROJ, 'data/expert_ratings/house_ratings_laborday.csv')
    if(file.exists(fl)){
        rl <- fread(fl)[rater %in% c('cook', 'inside', 'sabato')][, .(rating.ld = mean(rating_num, na.rm = TRUE) / 3), by = distcyc]
        H[, rating.ld := rl$rating.ld[match(rn, rl$distcyc)]]
        H[cycle >= 2006 & cycle <= 2018 & is.na(rating.ld), rating.ld := fifelse(inc != 0, inc, sign(lean))]
        H[, rating.move := rating - rating.ld]
        H[cycle == FORECAST & state == 'MO', rating.move := NA_real_]
        EXT.F <- c(EXT.F, 'rating.move')
        cat(sprintf('rating movement since Labor Day: %s\n', paste(H[cycle >= 2006 & (scored | contested26) & !is.na(rating.move), sprintf('%d moved %d', cycle, sum(abs(rating.move) > .01)), by = cycle]$V1, collapse = ', ')))
    } else warning('V3_RATING_MOVE: no file ', fl)
}

## Candidate background (V3_BIO) and donor profiles (V3_DPROF) are in src/features_cand_v3.R, sourced after this file.

## V3_OVERLAP: share of the district's population the incumbent represented before (1 where the lines did not
## change); signed for the incumbent's side, so the feature is inc x (1 - overlap) = how much of the seat is new
## Adopted 2026-10-21 (default on): 2018-24 41 -> 40 misses, Brier unchanged; 2006-24 -1 miss; seat CRPS 3.03 -> 3.02 (marginal)
if(Sys.getenv('V3_OVERLAP', '1') == '1'){
    fo <- file.path(PROJ, 'data/redistricting/incumbent_overlap.csv')
    if(file.exists(fo)){
        ov <- fread(fo); k <- match(H$rn, ov$distcyc)
        H[, overlap := ov$overlap[k]]
        H[inc == 0, overlap := 1]
        H[, inc.new := inc * (1 - fcoalesce(overlap, 1))]
        EXT.F <- c(EXT.F, 'inc.new')
        cat(sprintf('incumbent overlap: %d incumbents below .9 (%s)\n', sum(H$overlap < .9 & H$inc != 0, na.rm = TRUE),
                    paste(H[inc != 0 & !is.na(overlap) & overlap < .9, .N, by = cycle][order(cycle), sprintf('%d: %d', cycle, N)], collapse = ', ')))
    } else warning('V3_OVERLAP: no file ', fo)
}
