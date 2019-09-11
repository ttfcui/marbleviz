capture program drop drop_dupstat
program define drop_dupstat
    syntax varlist [if/] 
    
    foreach var of varlist `varlist' {
        capture sum type if !missing(`var') & `if', meanonly
	if _rc != 0 sum type if !missing(`var'), meanonly
        replace `var' = . if abs(type-`r(max)') < 1e-3 & ///
	    ((`var'[_n] == `var'[_n-1]) |(`var'[_n] == `var'[_n-2]))
    }

end

capture program drop std_byorder
program define std_byorder
    syntax varname [if]

        sort order
	by order: egen var_mean = mean(`varlist') `if'
	by order: egen var_sd = sd(`varlist') `if'
	
	replace `varlist' = -(`varlist' - var_mean)/(var_sd)
	drop var_mean var_sd
	
end

capture program drop std_difftype
program define std_difftype
    syntax varname [if], thres(real)
    
	sum `varlist' if type > `thres'
	replace `varlist' = -(`varlist' - `r(mean)')/`r(sd)' if type > `thres'

	sum `varlist' if type <= `thres'
	replace `varlist' = -(`varlist' - `r(mean)')/`r(sd)' if type <= `thres'

end

tempfile spdmain
/* 2019 CLEANING */	    
import delimited statslong_2019.csv, clear
rename ïteam  team
destring relay, ignore("DNFSQ") replace

sort team order type
drop_dupstat msprint-biathlon if type < 2019.5
foreach var of varlist msprint hurdles elimination biathlon {
    rename `var' `var'1
    egen `var' = std(`var'1)
    drop `var'1
    replace `var' = -`var'
}
std_difftype underwater, thres(2019.05)
std_difftype sandrally, thres(2019.5)
std_byorder relay if ///
    !(type <= 2019.05 & inlist(team, "MIDNIGHT WISPS", "O'RANGERS", "OCEANICS", "SAVAGE SPEEDERS"))

sort team order type
collapse (mean) relay-biathlon if !missing(order), by(team member)
save `spdmain'

/* 2018 CLEANING */
import delimited statslong_2018.csv, clear
rename ïteam  team
destring snowrally, ignore("DNFSQ") replace

sort team order type
drop_dupstat icedash-sandm if type < 2018.5
foreach var of varlist icedash speedskat snowboard sandmgl{
    rename `var' `var'1
    egen `var' = std(`var'1)
    drop `var'1
    replace `var' = -`var'
}
std_difftype snowrally, thres(2018.05)
std_byorder biathlon
rename biathlon winBiathlon

sort team order type
collapse (mean) snowrally-sandmgl if !missing(order), by(team member)
merge 1:1 team member using `spdmain'
tab team _m if _m!= 3
drop _m
save `spdmain', replace


/* 2017 CLEANING */
import delimited statslong_2017.csv, clear
rename ïteam  team
destring relay17 steeple sand17, ignore("DNFSQ") replace

sort team order type
drop_dupstat sprint-sand17 if type < 2017.5
foreach var of varlist sprint hurdles steeplechase undrwtr {
    rename `var' `var'1
    egen `var' = std(`var'1)
    drop `var'1
    replace `var' = -`var'
}
std_difftype sand17, thres(2017.05)
gen relayqual = relay17 if type < 2017.05
std_byorder relayqual
std_byorder relay17 if type >= 2017.05
replace relay17 = relayqual if type < 2017.05
drop relayqual

sort team order type
collapse (mean) relay17-undrwtr if !missing(order), by(team member)
merge 1:1 team member using `spdmain'
drop _m
save `spdmain', replace


/* 2016 CLEANING */
import delimited statslong_2016.csv, clear
rename ïteam  team
destring water16  hurdles, ignore("DNFSQ") replace
foreach var of varlist sand-hurdles {
    rename `var' `var'1
    egen `var' = std(`var'1)
    drop `var'1
    replace `var' = -`var'
}

sort team order type
collapse (mean) sand-hurdles if !missing(order), by(team member)
merge 1:1 team member using `spdmain'

* Pruning of irrelevant teams
drop if inlist(team, "BUMBLEBEES", "HORNETS", "TURTLE SLIDERS", "QUICKSILVERS")
drop if inlist(team, "BLACK JACKS", "GLIDING GLACIERS", "GOLDEN ORBS", "TEAM PLASMA")
tab team _m if _m!= 3
drop _m
egen overallcount = rownonmiss(sand16-biathlon)
export delimited MLspd_cleaned.csv, replace


* Deflating team event scores in mean
local teamdisc = 0.8
foreach var of relay17 winBiathlon relay {
    replace `var' = `var'*`teamdisc'
}
* Final tabulation of overall scores
rowsort sand16-biathlon, g(e1-e23) d highmiss


/*


sum underwater if type > 2019.05
replace underwater = -(underwater - `r(mean)')/`r(sd)' if type > 2019.05

sum underwater if type <= 2019.05
replace underwater = -(underwater - `r(mean)')/`r(sd)' if type <= 2019.05

sum sandrally if type > 2019.5
replace sandrally = -(sandrally - `r(mean)')/`r(sd)' if type > 2019.5

sum sandrally if type <= 2019.5
replace sandrally = -(sandrally - `r(mean)')/`r(sd)' if type <= 2019.5

bys order: egen relay_mean = mean(relay) if ///
    !(type <= 2019.05 & inlist(team, "MIDNIGHT WISPS", "O'RANGERS", "OCEANICS", "SAVAGE SPEEDERS"))
by order: egen relay_sd = sd(relay) if ///
    !(type <= 2019.05 & inlist(team, "MIDNIGHT WISPS", "O'RANGERS", "OCEANICS", "SAVAGE SPEEDERS"))

replace relay = -(relay - relay_mean)/(relay_sd)
drop relay_*

*egen minevents = rowmean(e1-e3)
*gen overallscore = overalltot/overallcount if overallcount <= 3
*replace overallscore = ((overalltot - minevents*3)*(overallcount-3) + minevents)/(overallcount-2)  ///
*    if overallcount > 3
*gen overallscore = overalltot/overallcount if overallcount <= 2
*replace overallscore = (overalltot - e1)/(overallcount-1) if overallcount > 2 & overallcount <= 5
*replace overallscore = (overalltot - e1 - e2)/(overallcount-2) if overallcount > 5


* Underweighing of poor events for "overworked" athletes - this is a weighted mean
* using a geometric discount factor.
foreach var of varlist e1-e23 {
    local n = substr("`var'", 2, .)
    replace `var' = (`discfact')^(`n'-1)*`var'
}
egen overalltot = rowtotal(e1-e23)
gen overallscore = overalltot/((1-`discfact'^overallcount)/(1-`discfact'))
drop overalltot e1-e23

bys team: egen teamrank = max(overallscore)
egen rank = rank(teamrank)
twoway scatter rank overallscore, ysc(r(0 120)) ylab(0(30)120)
drop teamrank

gsort -overallscore
*/