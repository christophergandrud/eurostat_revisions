/* Re-examine main findings with clustered standard errors rather than 
country fixed effects
*/

clear

cd "/git_repositories/eurostat_revisions/data_cleaning"

use "debt_sample.dta"

gen finstressXyrcurnt = finstress_mean*yrcurnt_corrected

* M1
regress cum_revision central_gov_debt euro_member excessdef, cluster(iso2c)

* M2
xi: regress cum_revision i.euro_member*central_gov_debt, cluster(iso2c)

* M3 
regress cum_revision central_gov_debt euro_member finstress_mean yrcurnt_corrected, cluster(iso2c)

* M4
xi: regress cum_revision central_gov_debt i.euro_member*finstress_mean, cluster(iso2c)

* M5
regress cum_revision central_gov_debt euro_member finstress_mean yrcurnt_corrected finstressXyrcurnt, cluster(iso2c)

* M6
regress cum_revision central_gov_debt euro_member finstress_mean endog_3, cluster(iso2c)

* M7
xi: regress cum_revision central_gov_debt euro_member i.endog_3*finstress_mean, cluster(iso2c)

* M8
regress cum_revision central_gov_debt euro_member fiscal_trans_gfs gdp_growth, cluster(iso2c)

* M9
regress cum_revision central_gov_debt euro_member contracts, cluster(iso2c)

* M10
xi: regress cum_revision i.euro_member*central_gov_debt excessdef i.endog_3*finstress_mean gdp_growth, cluster(iso2c)
