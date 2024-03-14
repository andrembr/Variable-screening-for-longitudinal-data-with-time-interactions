library(MASS)
library(mvnfast)

simulate_LMM <- function(seed = NULL, p, k, ns, tau = 1, sigmab, sigmae, setstype,
                         lineartime = T, randomslope = F){
  # p: Number of covariates
  # k: Number of time points
  # ns: Number of subjects
  # sigmab: standard deviation of random effects
  # sigmae: standard deviation of the noise
  # typeset: what are the true active sets of main effects and interactions? either overlapping, disjoint or identical
  # lineartime: linear effect of time T/F (T for ex. 1 and 2)
  # randomslope: random intercept or random intercept + time slope (T for ex2)
  # tau: the autocorrelation parameter for the covariates (tau=1 for ex. 1 and ex. 3, tau=0.8 for ex. 2)
  
  set.seed(seed)
  n <- ns*k # Total number of observations
  time <- rep(1:k,   times = ns) # Time points
  id <- rep(1:ns,   each = k ) # Id of subjects
  
  if(setstype == "overlap"){
    A <- c(1,2,3,4)
    I <- c(2,3,4,5)
  }
  else if(setstype == "disjoint"){
    A <- c(1,2,3,4)
    I <- c(5,6,7)
  }
  else if(setstype == "identical"){
    A <- c(1,2,3,4)
    I <- A
  }
  
  ## Simulate high-dim design matrix X
  exponent <- abs(matrix(1:k-1, nrow = k, ncol = k, byrow = TRUE)-(1:k-1))
  S <- 0.4^2*tau^exponent   # tau = 1 in ex 1 for only baseline measurement of X
  x0 <- lapply(1:ns, function(i) mvrnorm(p,rep(0,k),S))
  x <- t(do.call(cbind,x0))

  
  if(lineartime==T){      # Linear effect of time? T for ex 1 and 2
    
    if(randomslope==F){ 
      ### Random intercept model ###
      b0 <- rep(rnorm(ns,mean=0,sd=sigmab),each=k)
      Zb <- b0
    }  
    else if(randomslope==T){
      ### Random slope time (for ex. 2)
      b <- mvrnorm(n=ns,mu=rep(0,2),Sigma=sigmab^2*diag(2))  # Simulate random effects
      Z <- model.matrix(~seq(0,by=1,length.out=k))   # Make the random design matrix for one subject
      Zb <- as.vector(sapply(1:ns,function(i) Z%*%b[i,]))
    }
    
    ## Set coefficients ###
    beta0 <- rep(0,p)
    beta0[A] <- 1
    alpha0 <- rep(0,p)
    alpha0[I] <- 0.5
    
    ## Make observations - linear effect of time##
    y <- rep(0,n)
    t <- rep(seq(0,by=1,length.out=k),ns)
    for (i in 1:n){
      y[i] <- 0.2*t[i] + x[i,]%*%beta0 + t[i]*x[i,]%*%alpha0
    }
    y <- y + Zb + rnorm(n,mean=0,sd=sigmae) # Add random effects + random noise
    data <- list(y = y, x = x, t = t, id = as.factor(id), A0 = A, I0 = I)
  }
  
  else if (lineartime == F){            # not linear effect of time for ex 3

    ### Random intercept model ###
    b0 <- rep(rnorm(ns, mean = 0, sd = sigmab), each = k)
    Zb <- b0
    
    ## Set coefficients ###
    beta <- rep(0,p)
    beta[A] <- rep(1,length(A))
    Z <- model.matrix(~factor(1:k))
    t.d <- Z[rep(1:4,ns),-1]
    alpha <- rep(1,k-1)
    Omega <- matrix(0,ncol=k-1,nrow=p)
    Omega[I,] <- rep(1,k-1)
    
    ## Make observations - time dummies ##
    y <- rep(NA,n)
    for (i in 1:n){
      ti <- t.d[i,]
      y[i] <- x[i,]%*%beta+ti%*%alpha+t(x[i,])%*%Omega%*%ti
    }
    y <- y + Zb + rnorm(n,mean=0,sd=sigmae)
    data <- list(y = y, x = x,
                 time = as.factor(time),
                 id = as.factor(id), A0 = A, I0 = I)
  }
  return(data)
}



