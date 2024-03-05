
Code to reproduce the simulated examples in manuscript "Conditional variable screening for ultra-high dimensional longitudinal data with time interactions"

Structure of folders and files:

GEES.R : the GEES method
likelihood_screening.R : our proposed method

./sim_examples/:

	simulate_examples.R : function that simulates the linear mixed models corresponding to the three examples in the manuscript
	get_results_simulations.R : contains functions to simulate and analyse the examples with the three different methods 
	perform_analysis.R : this files actually runs and saves the results from get_results_simulations.R. 
	

R version 4.2.1 (2022-06-23)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Red Hat Enterprise Linux 8.9 (Ootpa)




