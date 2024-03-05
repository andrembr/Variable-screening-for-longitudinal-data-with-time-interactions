


To reproduce the tables and figures in the manuscript is explained in the R markdown file reproduce_results.Rmd and reproduce_results.html.



Structure of folders and files:

GEES.R : the GEES method
likelihood_screening.R : our proposed method

./sim_examples/:

	simulate_examples.R : function that simulates the linear mixed models corresponding to the three examples in the manuscript
	get_results_simulations.R : contains functions to simulate and analyse the examples with the three different methods 
	perform_analysis.R : this files actually runs and saves the results from get_results_simulations.R. 
	
	./intermediate_results/: the folder in which the results are saved. The files corresponding to the full runs are already in the folder.

R version 4.2.1 (2022-06-23)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Red Hat Enterprise Linux 8.9 (Ootpa)

Matrix products: default
BLAS/LAPACK: /opt/software/easybuild/software/FlexiBLAS/3.2.0-GCC-11.3.0/lib64/libflexiblas.so.3.2

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C
 [9] LC_ADDRESS=C               LC_TELEPHONE=C
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods
[8] base

other attached packages:
[1] MASS_7.3-57       Ball_1.3.13       doParallel_1.0.17 iterators_1.0.14
[5] foreach_1.5.2     geepack_1.3.4     lme4_1.1-29       Matrix_1.4-1
[9] ggplot2_3.3.6

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.8.3     pillar_1.7.0     compiler_4.2.1   nloptr_2.0.3
 [5] digest_0.6.29    boot_1.3-28      lifecycle_1.0.1  tibble_3.1.7
 [9] gtable_0.3.0     nlme_3.1-158     lattice_0.20-45  pkgconfig_2.0.3
[13] rlang_1.0.2      DBI_1.1.3        cli_3.3.0        mvtnorm_1.1-3
[17] withr_2.5.0      dplyr_1.0.9      generics_0.1.2   vctrs_0.4.1
[21] grid_4.2.1       tidyselect_1.1.2 glue_1.6.2       R6_2.5.1
[25] fansi_1.0.3      survival_3.3-1   minqa_1.2.4      farver_2.1.0
[29] tidyr_1.2.0      purrr_0.3.4      magrittr_2.0.3   codetools_0.2-18
[33] backports_1.4.1  scales_1.2.0     ellipsis_0.3.2   splines_4.2.1
[37] assertthat_0.2.1 colorspace_2.0-3 labeling_0.4.2   utf8_1.2.2
[41] munsell_0.5.0    gam_1.20.1       broom_0.8.0      crayon_1.5.1



