# Reproduction materials for: Interpreting Fiscal Accounting Rules in the European Union

Christopher Gandrud and Mark Hallerberg

*Journal of European Public Policy*

This repository contains the complete materials needed to replicate the plots and tables displayed in the main text and supplementary files of the article.

It contains two child directories:

- *analysis*: R and Stata source code used to conduct the analysis in the paper.

- *data_cleaning*: raw data and R source code for gathering, cleaning, and merging the raw data for its use in the analysis.

## *analysis* contents

- *debt_descriptives.R*: finds the descriptive statistics of debt regressions discussed in the article's introduction.

- *debt_polynomial_explore.R*: Estimate model and plot results show in Figure A-1 of the supplementary materials.

- *descriptive_plots.R*: Creates Figure 1 in the main article and figures A-2 and A-3 in the supplementary files.

- *independent_monitor_table.R*: Creates Table A-1 in the supplementary files.

- *main_regression_models_tables.R*: Creates Table 1 in the main text and tables A-3 through A-5 in the supplementary materials.

- *main_with_clusters.do*: Stata source code to re-estimate the models shown in Table 1 with country clustered standard errors rather than country fixed effects.

- *me_plots.R*: Source code to create Figure 2 in the main text.

- *unscheduled_elections_table.R*: Creates Table A-2 in the supplementary files.


## Set working directory

If you intend to reproduce the analysis, note that the assumed location of this repository and thus the working directory is: */eurostat_replication_material/*.

Please change `setwd` calls as needed for your system.
