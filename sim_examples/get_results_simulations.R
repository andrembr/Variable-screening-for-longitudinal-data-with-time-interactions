library(MASS)
library(mvnfast)
library(doParallel)
library(foreach)
library(lme4)
library(Ball)
library(cdcsis)

source("simulate_examples.R")


get_MMS <- function(p, A0, I0, ranked_features){   # function to get the minimum model size
  MMS = 1
  while(MMS < p){
    A <- ranked_features[1:MMS]
    rate.A <- length(intersect(A, A0)) / length(A0)
    rate.I <- length(intersect(A, I0)) / length(I0)
    if (min(rate.A, rate.I) == 1) break
    else MMS = MMS + 1
  }
  return(MMS)
}

one_simulation <- function(i = 1, p, k, ns, sigmab, sigmae, example, method, corr = "ind", rslope = F){
  
  if (example == "ex1") { # set the correct settings for ex1
    lineartime0 <- T
    randomslope0 <- F
    tau0 <- 1
  }
  
  else if (example == "ex2") {  # set the correct settings for ex2
    lineartime0 <- T
    randomslope0 <- T
    tau0 <- 0.8
  }
  
  else if (example == "ex3"){  # set the correct settings for ex3
    lineartime0 <- F
    randomslope0 <- F
    tau0 <- 1
  }
  
  data <- simulate_LMM(seed = i, p = p, k = k, ns = ns, tau = tau0, sigmab = sigmab, sigmae = sigmae, 
                       setstype = "overlap", lineartime = lineartime0, randomslope = randomslope0)
  
  y <- data$y
  x <- data$x
  id <- as.factor(data$id)
  t <- data$t
  A0 <- data$A0
  I0 <- data$I0
  
  # Estimate effective sample size and set d = n_eff
  baseform <- as.formula(paste0("y ~ 1+(1|id)"))
  modelbase <- lmer(baseform,data.frame(y = y,id = id),REML =T)
  sdsample <- as.data.frame(VarCorr(modelbase))$vcov
  rho_sample <- sdsample[1]/(sdsample[1]+sdsample[2])
  n_eff <- ns*(k/(1+rho_sample*(k-1)))
  d <- n_eff     
  
  
  if (method == "LS"){
    source("../likelihood_screening.R")
    start <- Sys.time()
    lik0 <- likelihood_screen(y, x, t, id, rslope) # get the marginal utility from likelihood screening
    ranked_features <- order(lik0, decreasing = TRUE) # order the variables according to the marginal utility
    time <- difftime(Sys.time(), start, units = "secs")
    MMS <- get_MMS(p, A0, I0, ranked_features) # get the minimum model size to capture all of A0 and I0
    screened <- ranked_features[1:d] # do a final screening by only retaining the top d variables
    if (rslope == T) method <- paste0(method, "." , "slope")
    else method <- paste0(method, ".", "intercept")
  }
  
  else if (method == "GEES"){
    source("../GEES.R")
    start <- Sys.time()
    if (corr == "cs") g0 <- gee_screen(y, x, t, id, "exch")
    else g0 <- gee_screen(y, x, t, id, corr) # get the marginal utility from GEES
    ranked_features <- order(g0, decreasing = TRUE) 
    time <- difftime(Sys.time(), start, units = "secs")
    MMS <- get_MMS(p, A0, I0, ranked_features)
    screened <- ranked_features[1:d]
    method <- paste0(method, ".", corr)
  }
  
  else if (method == "BCor-SIS"){
    start <- Sys.time()
    A.first <- bcorsis(x, y, method = "interaction") # get the marginal utility from BCor-SIS
    ranked_features <- order(A.first$complete.info$statistic[,1], decreasing = T) # order the variables according to bcor
    time = difftime(Sys.time(), start, units = "secs")
    MMS <- get_MMS(p, A0, I0, ranked_features)
    screened <- ranked_features[1:d]
  }
  
  else if (method == "CDC-SIS"){
    ### group the observations into a multivariate response setting ###
    start <- Sys.time()
    ns <- length(unique(id))
    y.mat <- matrix(y, ncol = nlevels(as.factor(t)), byrow = T)
    x <- as.list(as.data.frame(x))
    x.list <- lapply(1:p, function(i) {
      matrix(x[[i]], ncol = nlevels(as.factor(t)), byrow = T)
    })
    t.mat <-  matrix(as.numeric(t), ncol = nlevels(as.factor(t)), byrow = T)
    width <- stats::bw.nrd0(as.numeric(t))
    cdc0 <- cdcsis::cdcsis(x = x.list, y = y.mat, z = matrix(1, nrow = ns, ncol = 1)) # get the marginal utility from CDC-SIS
    # Note that we set a constant value to condition on because it needs a non NULL value
    ranked_features <- order(cdc0[["cdcor"]], decreasing = TRUE)
    time <- difftime(Sys.time(), start, units = "secs")
    MMS <- get_MMS(p, A0, I0, ranked_features)
    screened <- ranked_features[1:d]
  }
    
  
  results <- list(method = method,
                  screening_set = screened,
                  MMS = MMS, time = time,
                  n_eff = n_eff, A0 = A0,
                  I0 = I0)
  return(results)
} 

summarize_results <- function(results){
  method <- results[,1]$method
  no_sim <- ncol(results)
  MMS = as.numeric(unlist(results["MMS",]))
  A0 <- results[,1]$A0
  I0 <- results[,1]$I0
  
  rates <- sapply(1:no_sim, function(i){
    A <- results[,i]$screening_set
    rate.A <- length(intersect(A, A0)) / length(A0)
    rate.I <- length(intersect(A, I0)) / length(I0)
    c(rate.A, rate.I)
  })
  rate.A <- sum(rates[1,] == 1) / no_sim
  rate.I <- sum(rates[2,] == 1) / no_sim
  rateA.mean  <- mean(rates[1,])
  rateI.mean <- mean(rates[2,])
  time.mean <-  mean(as.numeric(results["time",]))
  
  form <- function(value) format(round(value, digits = 3),nsmall = 3,scientific = FALSE) # function to round the results similarly
  form1<- function(value) format(round(value, digits = 0),nsmall = 0,scientific = FALSE) # function to round the results similarly
  
  MMS <- paste(form1(quantile(MMS, probs = c(0.5,0.75,0.95))))
  
  return(data.frame(method,form(rate.A),form(rateA.mean),form(rate.I),form(rateI.mean),form(time.mean),MMS[1],MMS[2],MMS[3]))
}

