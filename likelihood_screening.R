library(lme4)
library(doParallel)
library(foreach)

lmm_likelihood <- function(df, rslope = F){
  if (rslope == T){
    t0 <- as.numeric(df$t)-1
    frmla <- as.formula(paste0("y ~ x+x:t+t+(1+t0|id)")) 
  }
  else frmla <- as.formula(paste0("y ~ x+x:t+t+(1|id)")) 
  model <- lmer(frmla, df, REML = F, control=lmerControl(check.conv.singular=.makeCC(action = "ignore",  tol = 1e-4)))
  return(logLik(model, REML = F))
}

likelihood_screen <- function(y, x, t, id, rslope){
  n <- length(y)
  ns <- length(unique(id))
  l <- ncol(x)
  k <- length(unique(t))
  no_cores <- 12
  cl <- makeCluster(no_cores,type="FORK")
  registerDoParallel(cl)
  likelihoods <- foreach(i=1:ncol(x),.combine='c') %dopar% {
    df <- data.frame(y = y, x = x[,i], t = t, id = as.factor(id))
    lmm_likelihood(df, rslope)}
  stopCluster(cl)
  return(likelihoods)
}

