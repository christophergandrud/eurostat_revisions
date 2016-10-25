/* Re-examine main findings with clustered standard errors rather than 
country fixed effects
*/

clear

cd "/git_repositories/eurostat_revisions/data_cleaning"

use "debt_sample.dta"


* Euro membership and central government debt interaction
xi: regress cum_revision i.euro_member*central_gov_debt, cluster(iso2c)


* Financial market stress, non-interactive 
regress cum_revision central_gov_debt finstress_mean, cluster(iso2c)

* Unscheduled election and financial market stress interaction
xi: regress cum_revision central_gov_debt i.endog_3*finstress_mean, cluster(iso2c)

* Euro membership and financial market stress interaction
xi: regress cum_revision i.euro_member*finstress_mean, cluster(iso2c)


* Fiscal contracts
regress cum_revision central_gov_debt euro_member contracts, cluster(iso2c)
