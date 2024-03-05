


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




