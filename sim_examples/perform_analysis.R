
#!/usr/bin/env Rscript

source("get_results_simulations.R")


#' Performs the analysis from the file "get_results_simulations.R"


args_user <- commandArgs(trailingOnly=TRUE) # takes two arguments from the console: the example number and the number of simulations
example <- args_user[1] # which example number? must put either ex1, ex2 or ex3
ns_all <- c(40,80,100) # number of subjects
methods <- c("GEES","BCor-SIS","LS") # the methods that we compare
sigmabs <- c(0.1,0.9) # sd of the random effects
no_sim <- as.numeric(args_user[2]) # the number of simulations
no_cores <- 12


# iterate through both values of sigmab and different values for ns
full.results <- c()
for(sigmab in sigmabs){
  for(ns in ns_all){
    args <- c(1000,ns,sigmab,0.1,no_sim)
    for(method in methods){
      print(paste("Method: ", method))
      
      # If method is GEES, then we do the analysis for all three different correlation structures
      if (method == "GEES"){    
        corrs <- c("ind","cs","ar1")
        for (corr in corrs){
          cl <- makeCluster(no_cores,type="FORK")
          registerDoParallel(cl)
          results.parallel <- foreach(i=1:as.numeric(args[5]),.combine='cbind') %dopar% one_simulation(i,p=args[1],k=4,ns=args[2],
                                                                                                       sigmab=args[3],sigmae=args[4],
                                                                                                       example=example,method=method,corr=corr)
          stopCluster(cl)
          res <- summarize_results(results.parallel) # summarize the results from the n_sim runs
          full.results <- rbind(full.results,c(ns,sigmab,res)) # add the results to the full list
        }
      }
      
      # If the method is LS, then we do both the random intercept and random slope version
      else if (method == "LS"){  
        for(r in c(FALSE,TRUE)){
          tot.res <- sapply(1:as.numeric(args[5]), function(i){one_simulation(i,p=args[1],k=4,ns=args[2],sigmab=args[3],
                                                                              sigmae=args[4],example=example,method=method,rslope=r)})
          res <- summarize_results(tot.res) # summarize the results from the n_sim runs
          full.results <- rbind(full.results,c(ns,sigmab,res)) # add the results to the full list
        }
      }
      
      # else the method is bcor-sis
      else{
        cl <- makeCluster(no_cores,type="FORK")
        registerDoParallel(cl)
        results.parallel <- foreach(i=1:as.numeric(args[5]),.combine='cbind') %dopar% one_simulation(i,p=args[1],k=4,ns=args[2],
                                                                                                     sigmab=args[3],sigmae=args[4],
                                                                                                     example=example,method=method)
        stopCluster(cl)
        res <- summarize_results(results.parallel) # summarize the results from the n_sim runs
        full.results <- rbind(full.results,c(ns,sigmab,res)) # add the results to the full list
      }
      
    }
  }
}
colnames(full.results) <- c("ns","sigmab","Method","recovery A", "mean rate A", "recovery I", "mean rate I", "Time", "50%","75%","95%")
saveRDS(full.results,file = paste0("intermediate_results/",args_user[3],".rds")) 

