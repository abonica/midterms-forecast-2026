# Codebook

One row per released variable, by office. `y` is the Democratic two-party vote share minus 0.5. `rn` is the contest key
(`2026_CA_13`; Senate `2026_GA_S`, second race in a state-cycle `_S2`; governor `_G`). 2026 rows have no outcome.

**Timing.** `pre`: known before the election, as of the forecast date (money through Sep 30, generic ballot Oct 1, polls and
ratings through Oct 18). `outcome`: the result. `model`: the model's out-of-sample forecast (each past cycle predicted from
earlier cycles only). `id`: keys and names.

**Use.** The model component that uses the variable, or `descriptive`.

## House (`data/house.csv`, 81 variables)

**Keys**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `rn` | Contest key: cycle_state_district (House), cycle_state_S (Senate), cycle_state_G (governor), cycle_state_chamber_district (state legislature) | id | descriptive | 0% |
| `cycle` | Election year | id | descriptive | 0% |
| `state` | State postal code | id | descriptive | 0% |
| `dem.name` | D (R) nominee name | id | descriptive | 5% |
| `rep.name` | D (R) nominee name | id | descriptive | 6% |
| `dem.rid.static` | D (R) nominee DIME recipient id without cycle | id | descriptive | 5% |
| `rep.rid.static` | D (R) nominee DIME recipient id without cycle | id | descriptive | 6% |
| `dem.fec.cand.id` | D (R) nominee FEC candidate id | id | descriptive | 2% |
| `rep.fec.cand.id` | D (R) nominee FEC candidate id | id | descriptive | 2% |

**Incumbency and candidates**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `inc.status` | D nominee status: I incumbent, C challenger, O open | pre | descriptive | 0% |
| `quality.chall.dem` | D (R) nominee has held elected office (Congress, governor, or office in the Vote Smart bio) | pre | residual model | 0% |
| `quality.chall.rep` | D (R) nominee has held elected office (Congress, governor, or office in the Vote Smart bio) | pre | residual model | 0% |
| `rep.female` | D (R) nominee is a woman | pre | residual model | 0% |
| `dem.female` | D (R) nominee is a woman | pre | residual model | 0% |
| `inc` | Incumbency: +1 D incumbent, -1 R incumbent, 0 open (state leg.: the nominee won the seat last time) | pre | residual model; linear base | 0% |
| `dem.prev.over` | D (R) nominee's overperformance in their last general (House, Senate or governor): vote share - lean - cycle mean, own-party sign | pre | residual model | 53% |
| `rep.prev.over` | D (R) nominee's overperformance in their last general (House, Senate or governor): vote share - lean - cycle mean, own-party sign | pre | residual model | 51% |
| `dem.hisp` | D (R) nominee coded Hispanic: surname share >= 50% (surname-based inference) | pre | residual model | 0% |
| `rep.hisp` | D (R) nominee coded Hispanic: surname share >= 50% (surname-based inference) | pre | residual model | 0% |
| `dem.hisp.x` | (dem\|rep).hisp x the district Hispanic population share (ACS) | pre | residual model | 0% |
| `rep.hisp.x` | (dem\|rep).hisp x the district Hispanic population share (ACS) | pre | residual model | 0% |

**District partisanship**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `dist.pres.vs` | D two-party presidential share in the district (state for Senate/governor) in the reference election, on the cycle lines | pre | descriptive | 0% |
| `lean` | dist.pres.vs minus the national D two-party presidential share of the reference election | pre | residual model; linear base | 0% |
| `abs.lean` | Absolute value of lean | pre | post-fit layer / poll input | 0% |
| `trend.x` | District presidential trend: lean(ref) - lean(ref - 4) on the cycle lines; where the trend file lacks a district, the ref - 4 share is carried onto the cycle lines through the census-block crosswalks (rougher across full redistrictings) | pre | input to trend (post-fit layer); as a feature: flag V3_TREND_FEAT, off | 50% |
| `trend` | Trend used by the post-fit trend layer: trend.x net of its demographic fit within cycle (0 where missing; 2026 redrawn states: 2020 share on the new lines from the census-block crosswalk) | pre | post-fit layer / poll input | 0% |

**Money: receipts**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `total.dem` | D (R) nominee total receipts through the money cutoff (Sep 30 of the election year); joint-fundraising memo copies dropped, and a House/Senate switcher counts only the contest office committee. A non-incumbent nominee with no money records (no FEC committee) is 0 here and in the other receipt, spending, donor and cash columns (2004 on) | pre | descriptive | 0% |
| `total.rep` | D (R) nominee total receipts through the money cutoff (Sep 30 of the election year); joint-fundraising memo copies dropped, and a House/Senate switcher counts only the contest office committee. A non-incumbent nominee with no money records (no FEC committee) is 0 here and in the other receipt, spending, donor and cash columns (2004 on) | pre | descriptive | 4% |

**National**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `midterm` | Midterm cycle | pre | descriptive | 0% |
| `pres.party` | President is a Democrat (dem.pres 1/0; pres.party +1 D / -1 R) | pre | descriptive | 0% |
| `gb` | Generic ballot D-R margin, Oct 1, as a proportion (2026: NYT 30-day mean) | pre | national stage | 0% |
| `mid.pen` | Midterm penalty: midterm x president party (+1 = D president in a midterm) | pre | national stage | 0% |

**Sample flags**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `scored` | Row is a genuine D-vs-R contest with a two-party share in (.05, .95) and a lean (scored) / both nominees named and distinct (dr): the backtest sample | pre | descriptive | 0% |
| `contested26` | 2026 row is a D-vs-R contest that the model forecasts | pre | descriptive | 0% |
| `uncontested.dem` | No D (R) nominee on the ballot | pre | descriptive | 0% |
| `uncontested.rep` | No D (R) nominee on the ballot | pre | descriptive | 0% |
| `nominee.unresolved` | 2026: no settled D-vs-R matchup (LA jungle, CA-6 before the fix) | pre | descriptive | 94% |

**Outcome**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `dem.cand.vs` | D two-party vote share of the general election (outcome) | outcome | descriptive | 6% |
| `y` | D two-party vote share minus .5 (modeling outcome; y also for governor and state legislature) | outcome | outcome | 6% |

**Ideology**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `dem.ip` | D (R) nominee CFscore (DIME; party-cycle mean when missing) | pre | residual model | 0% |
| `rep.ip` | D (R) nominee CFscore (DIME; party-cycle mean when missing) | pre | residual model | 0% |
| `dem.ext` | CFscore minus the party-cycle mean (extremity within party) | pre | residual model | 0% |
| `rep.ext` | CFscore minus the party-cycle mean (extremity within party) | pre | residual model | 0% |
| `dem.np` | Incumbent Nokken-Poole DW-NOMINATE dim 1, vote-weighted career to date through the Congress before the election (incumbents only) | pre | residual model | 56% |
| `rep.np` | Incumbent Nokken-Poole DW-NOMINATE dim 1, vote-weighted career to date through the Congress before the election (incumbents only) | pre | residual model | 55% |
| `dem.np.ext` | (dem\|rep).np centered within cycle | pre | residual model | 56% |
| `rep.np.ext` | (dem\|rep).np centered within cycle | pre | residual model | 55% |

**Primaries**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `top2` | Top-two or jungle primary state (WA 2008+, CA 2012+, LA) | pre | residual model | 0% |
| `num.cands.in.primary.dem` | Candidates in the D (R) primary | pre | residual model | 0% |
| `num.cands.in.primary.rep` | Candidates in the D (R) primary | pre | residual model | 0% |

**Money: model features**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `m.tot` | Money: log(D + 25k) - log(R + 25k) of total receipts, centered within cycle | pre | residual model | 4% |
| `m.ind` | Same, individual contributions | pre | residual model | 4% |
| `m.pac` | Same, PAC contributions | pre | residual model | 4% |
| `m.donors` | Same, donor counts (offset 250) | pre | residual model | 4% |
| `m.disb` | Same, total spending | pre | residual model | 28% |
| `m.cash` | Same, cash on hand | pre | residual model | 28% |
| `m.q6` | Same, Apr-Jun election-year receipts | pre | residual model | 0% |
| `m.self` | Same, self-funding | pre | residual model | 4% |
| `m.pol` | Same, political spending (pacwatch vendor coding: ads, voter contact, field); NA (not zero) when none is coded on > $200k disbursed, 2006 on | pre | residual model | 25% |
| `m.frs` | Same, fundraising spending (list rental, digital fundraising vendors); NA under the same rule as m.pol | pre | residual model | 25% |

**Ratings and ballot**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `rating` | Expert rating mean of Cook, Inside Elections, Sabato as of Oct 9, -1 Safe R to +1 Safe D; pre-2020 House unlisted races set safe for the incumbent party / by lean | pre | residual model | 31% |

**Demographics**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `pct_hispanic` | Hispanic share of the population (ACS 5-year, cycle lines) | pre | post-fit layers: trend residual and demographic poll shift; as features: flag V3_ACS, off | 50% |
| `pct_black_nh` | Non-Hispanic Black share (ACS) | pre | post-fit layers: trend residual and demographic poll shift; as features: flag V3_ACS, off | 50% |
| `pct_asian_nh` | Non-Hispanic Asian share (ACS) | pre | post-fit layer: trend residual; as a feature: flag V3_ACS, off | 50% |
| `pct_ba_plus` | Adults 25+ with a BA or more (ACS) | pre | post-fit layers: trend residual and demographic poll shift; as features: flag V3_ACS, off | 50% |
| `pct_white_nh_noncollege` | Non-Hispanic white adults without a BA (ACS) | pre | post-fit layers: trend residual and demographic poll shift; as features: flag V3_ACS, off | 50% |
| `log_density` | Log population density (ACS) | pre | post-fit layers: trend residual and demographic poll shift; as features: flag V3_ACS, off | 50% |
| `log_income` | Log median household income (ACS) | pre | post-fit layer: trend residual; as a feature: flag V3_ACS, off | 50% |
| `pct_over65` | Share aged 65+ (ACS) | pre | post-fit layer: trend residual; as a feature: flag V3_ACS, off | 50% |
| `pct_foreign_born` | Foreign-born share (ACS) | pre | post-fit layer: trend residual; as a feature: flag V3_ACS, off | 50% |

**District**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `inc.new` | inc x (1 - overlap): how much of the seat is new to the incumbent, signed for the incumbent party | pre | residual model | 0% |

**Donors**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `dp.cap100.d.x` | Donor-profile contrast, D minus R, standardized within cycle (maxed-out share, under-$25 share, small-gift share, donors over 65, first-time donors, giving per donor; data/v2/donor_profiles) | pre | residual model | 60% |
| `dp.cap25.x` | Donor-profile contrast, D minus R, standardized within cycle (maxed-out share, under-$25 share, small-gift share, donors over 65, first-time donors, giving per donor; data/v2/donor_profiles) | pre | residual model | 60% |
| `dp.small.x` | Donor-profile contrast, D minus R, standardized within cycle (maxed-out share, under-$25 share, small-gift share, donors over 65, first-time donors, giving per donor; data/v2/donor_profiles) | pre | residual model | 60% |
| `dp.age65.x` | Donor-profile contrast, D minus R, standardized within cycle (maxed-out share, under-$25 share, small-gift share, donors over 65, first-time donors, giving per donor; data/v2/donor_profiles) | pre | residual model | 64% |
| `dp.new.x` | Donor-profile contrast, D minus R, standardized within cycle (maxed-out share, under-$25 share, small-gift share, donors over 65, first-time donors, giving per donor; data/v2/donor_profiles) | pre | residual model | 65% |
| `dp.gpd.x` | Donor-profile contrast, D minus R, standardized within cycle (maxed-out share, under-$25 share, small-gift share, donors over 65, first-time donors, giving per donor; data/v2/donor_profiles) | pre | residual model | 60% |

**Polls**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `poll.x` | District poll average used inside the model: through Oct 18, 30-day half-life, sponsor-bias adjusted, y scale | pre | post-fit layer / poll input | 84% |
| `neff.x` | Effective poll volume of poll.x (recency- and size-weighted) | pre | post-fit layer / poll input | 84% |

**Model output**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `oos.pp` | Final out-of-sample prediction of y (after the poll blend) | model | out-of-sample forecast | 28% |
| `oos.prob` | Out-of-sample D win probability | model | out-of-sample forecast | 38% |

## Senate (`data/senate.csv`, 38 variables)

**Keys**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `rn` | Contest key: cycle_state_district (House), cycle_state_S (Senate), cycle_state_G (governor), cycle_state_chamber_district (state legislature) | id | descriptive | 0% |
| `cycle` | Election year | id | descriptive | 0% |
| `state` | State postal code | id | descriptive | 0% |
| `dem.name` | D (R) nominee name | id | descriptive | 3% |
| `rep.name` | D (R) nominee name | id | descriptive | 1% |
| `dem.rid.static` | D (R) nominee DIME recipient id without cycle | id | descriptive | 3% |
| `rep.rid.static` | D (R) nominee DIME recipient id without cycle | id | descriptive | 1% |
| `dem.fec.cand.id` | D (R) nominee FEC candidate id | id | descriptive | 4% |
| `rep.fec.cand.id` | D (R) nominee FEC candidate id | id | descriptive | 3% |

**Incumbency and candidates**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `inc.status` | D nominee status: I incumbent, C challenger, O open | pre | descriptive | 0% |
| `quality.chall.dem` | D (R) nominee has held elected office (Congress, governor, or office in the Vote Smart bio) | pre | residual model | 1% |
| `quality.chall.rep` | D (R) nominee has held elected office (Congress, governor, or office in the Vote Smart bio) | pre | residual model | 0% |
| `inc` | Incumbency: +1 D incumbent, -1 R incumbent, 0 open (state leg.: the nominee won the seat last time) | pre | residual model; linear base | 0% |
| `dem.prev.over` | D (R) nominee's overperformance in their last general (House, Senate or governor): vote share - lean - cycle mean, own-party sign | pre | residual model | 57% |
| `rep.prev.over` | D (R) nominee's overperformance in their last general (House, Senate or governor): vote share - lean - cycle mean, own-party sign | pre | residual model | 52% |

**District partisanship**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `dist.pres.vs` | D two-party presidential share in the district (state for Senate/governor) in the reference election, on the cycle lines | pre | descriptive | 0% |
| `lean` | dist.pres.vs minus the national D two-party presidential share of the reference election | pre | residual model; linear base | 0% |

**Money: receipts**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `total.dem` | D (R) nominee total receipts through the money cutoff (Sep 30 of the election year); joint-fundraising memo copies dropped, and a House/Senate switcher counts only the contest office committee. A non-incumbent nominee with no money records (no FEC committee) is 0 here and in the other receipt, spending, donor and cash columns (2004 on) | pre | descriptive | 1% |
| `total.rep` | D (R) nominee total receipts through the money cutoff (Sep 30 of the election year); joint-fundraising memo copies dropped, and a House/Senate switcher counts only the contest office committee. A non-incumbent nominee with no money records (no FEC committee) is 0 here and in the other receipt, spending, donor and cash columns (2004 on) | pre | descriptive | 1% |

**National**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `midterm` | Midterm cycle | pre | descriptive | 0% |

**Sample flags**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `scored` | Row is a genuine D-vs-R contest with a two-party share in (.05, .95) and a lean (scored) / both nominees named and distinct (dr): the backtest sample | pre | descriptive | 0% |
| `contested26` | 2026 row is a D-vs-R contest that the model forecasts | pre | descriptive | 0% |

**Outcome**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `dem.cand.vs` | D two-party vote share of the general election (outcome) | outcome | descriptive | 7% |
| `y` | D two-party vote share minus .5 (modeling outcome; y also for governor and state legislature) | outcome | outcome | 7% |

**Ideology**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `dem.ip` | D (R) nominee CFscore (DIME; party-cycle mean when missing) | pre | residual model | 1% |
| `rep.ip` | D (R) nominee CFscore (DIME; party-cycle mean when missing) | pre | residual model | 0% |
| `dem.ext` | CFscore minus the party-cycle mean (extremity within party) | pre | residual model | 1% |
| `rep.ext` | CFscore minus the party-cycle mean (extremity within party) | pre | residual model | 0% |
| `dem.np` | Incumbent Nokken-Poole DW-NOMINATE dim 1, vote-weighted career to date through the Congress before the election (incumbents only) | pre | residual model | 63% |
| `rep.np` | Incumbent Nokken-Poole DW-NOMINATE dim 1, vote-weighted career to date through the Congress before the election (incumbents only) | pre | residual model | 61% |

**Money: model features**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `m.tot` | Money: log(D + 25k) - log(R + 25k) of total receipts, centered within cycle | pre | residual model | 2% |
| `m.ind` | Same, individual contributions | pre | residual model | 2% |
| `m.pac` | Same, PAC contributions | pre | residual model | 2% |
| `m.cash` | Same, cash on hand | pre | residual model | 26% |
| `m.q6` | Same, Apr-Jun election-year receipts | pre | residual model | 1% |

**Ratings and ballot**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `rating` | Expert rating mean of Cook, Inside Elections, Sabato as of Oct 9, -1 Safe R to +1 Safe D; pre-2020 House unlisted races set safe for the incumbent party / by lean | pre | residual model | 31% |

**Model output**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `oos.pp` | Final out-of-sample prediction of y (after the poll blend) | model | out-of-sample forecast | 36% |
| `oos.prob` | Out-of-sample D win probability | model | out-of-sample forecast | 47% |

## Governor (`data/governor.csv`, 14 variables)

**Keys**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `rn` | Contest key: cycle_state_district (House), cycle_state_S (Senate), cycle_state_G (governor), cycle_state_chamber_district (state legislature) | id | descriptive | 0% |
| `cycle` | Election year | id | descriptive | 0% |
| `state` | State postal code | id | descriptive | 0% |
| `dem.name` | D (R) nominee name | id | descriptive | 0% |
| `rep.name` | D (R) nominee name | id | descriptive | 1% |

**Outcome**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `dem.cand.vs` | D two-party vote share of the general election (outcome) | outcome | descriptive | 11% |
| `y` | D two-party vote share minus .5 (modeling outcome; y also for governor and state legislature) | outcome | outcome | 11% |

**Ratings and ballot**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `rating` | Expert rating mean of Cook, Inside Elections, Sabato as of Oct 9, -1 Safe R to +1 Safe D; pre-2020 House unlisted races set safe for the incumbent party / by lean | pre | residual model | 36% |

**Incumbency and candidates**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `inc` | Incumbency: +1 D incumbent, -1 R incumbent, 0 open (state leg.: the nominee won the seat last time) | pre | residual model; linear base | 0% |
| `dem.prev.over` | D (R) nominee's overperformance in their last general (House, Senate or governor): vote share - lean - cycle mean, own-party sign | pre | residual model | 77% |
| `rep.prev.over` | D (R) nominee's overperformance in their last general (House, Senate or governor): vote share - lean - cycle mean, own-party sign | pre | residual model | 66% |

**District partisanship**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `lean` | dist.pres.vs minus the national D two-party presidential share of the reference election | pre | residual model; linear base | 10% |

**Model output**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `oos.pp` | Final out-of-sample prediction of y (after the poll blend) | model | out-of-sample forecast | 21% |
| `oos.prob` | Out-of-sample D win probability | model | out-of-sample forecast | 40% |

## Stateleg (`data/stateleg.csv`, 15 variables)

**Keys**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `rn` | Contest key: cycle_state_district (House), cycle_state_S (Senate), cycle_state_G (governor), cycle_state_chamber_district (state legislature) | id | descriptive | 0% |
| `cycle` | Election year | id | descriptive | 0% |
| `state` | State postal code | id | descriptive | 0% |
| `seat` | State-legislative chamber (state:lower / state:upper) | id | descriptive | 0% |
| `district` | Contest or district identifier (distcyc = rn for federal rows; dkey = district without cycle; pbkey = precinct down-ballot lookup key) | id | descriptive | 0% |
| `dem.k` | State-legislative D (R) nominee name key (normalized surname + first initial) | id | descriptive | 18% |
| `rep.k` | State-legislative D (R) nominee name key (normalized surname + first initial) | id | descriptive | 18% |

**Outcome**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `y` | D two-party vote share minus .5 (modeling outcome; y also for governor and state legislature) | outcome | outcome | 6% |

**State legislature**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `lagc` | Previous D share minus .5 if contested, else 0 | pre | linear model | 0% |
| `uncD` | Previous election uncontested and won by D (R) | pre | linear model | 0% |
| `uncR` | Previous election uncontested and won by D (R) | pre | linear model | 0% |
| `upper` | Upper chamber | pre | linear model | 0% |

**Incumbency and candidates**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `inc` | Incumbency: +1 D incumbent, -1 R incumbent, 0 open (state leg.: the nominee won the seat last time) | pre | linear model | 0% |

**Model output**

| Variable | Description | Timing | Use | Missing |
|---|---|---|---|---|
| `oos.pp` | Final out-of-sample prediction of y (after the poll blend) | model | out-of-sample forecast | 71% |
| `oos.prob` | Out-of-sample D win probability | model | out-of-sample forecast | 71% |

