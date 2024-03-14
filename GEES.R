library(MASS)
library(geepack)

#ar(1)
ar1_cor <- function(n, alpha) {
  exponent <- abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) - 
                    (1:n - 1))
  return(alpha^exponent)
}

#compound symmetry
cs_cor <- function(k, alpha){
  r <-  matrix(alpha, nrow = k, ncol = k)
  diag(r) <- 1
  return(r)
}

est_alpha <- function(x, y, id, corr){
  LM <- list(y = y, x = scale(x,T,T))
  gee <- geese(y~x-1, id = id, data = LM, family = gaussian(), corstr = corr)
  return(gee$alpha)
}

g0_func <- function(y, x, M, alpha = 0, corr = "ind"){
  if(corr == "ind"){
    R = diag(M)
  }
  else if (corr == "exch"){
    R = cs_cor(M, alpha)
  }
  else if (corr == "ar1"){
    R = ar1_cor(M, alpha)
  }
  
  invR <- solve(R)
  nna <- which(!is.na(y))
  x <- x[nna,]
  if(M == 1) g0 <- x%*%invR%*%y[nna]
  if(M > 1) g0 <- t(x)%*%invR%*%y[nna]
  return(g0)
}

gee_screen <- function(y, x, t, id, corr){
  ns <- length(unique(id))
  k <- length(unique(t))
  
  nna <- which(!is.na(y))
  x1 <- scale(x[nna,], TRUE, TRUE)
  y.mat <- matrix(y, nrow = ns, byrow = T)
  M <- sapply(1:ns, function(i) sum(!is.na(y.mat[i,])))

  x.list <- lapply(1:ns, function(i) {
    if(i == 1){x1[1:M[1],]}
    else{aa <- sum(M[1:(i-1)])+1
    bb <- sum(M[1:i])
    x1[aa:bb,]}
  })
  ## Estimate alpha ##
  if(corr == "exch" | corr == "ar1"){
    g0 <- (1/ns)*apply(sapply(1:ns, function(i) g0_func(y.mat[i,], x.list[[i]], M[i])), 1, sum)
    ind.init <- which(abs(g0) %in% tail(sort(abs(g0)), ns-1))
    alpha <- est_alpha(x1[,ind.init], y[nna], id=id[nna], corr=corr)
  }
  
  g0 <- (1/ns)*apply(sapply(1:ns, function(i) g0_func(y.mat[i,], x.list[[i]], M[i], alpha, corr)), 1, sum)
  return(abs(g0))
}



gee_screen_unbalanced <- function(df, M, corr){
  unique.ids <- unique(df$id)
  ns <- length(unique.ids)
  g0 <- do.call(cbind, lapply(1:ns,function(i){
    M = M[i]
    X = df[df$id==unique.ids[i],-seq(1,4)]
    y = df[df$id==unique.ids[i],1]
    g0_func(y,x=X,M,alpha=0,corr="ind")
  }))
  
  g0 <- apply(g0,1,mean)
  B <- order(g0,decreasing = TRUE)
  ind.init <- B[1:(ns-1)]
  frmla <- as.formula(paste0("y~-1+",paste0("x",ind.init,collapse="+")))
  gee <- geese(frmla, id=as.factor(id), data=df, family=gaussian(), corstr=corr)
  alpha <- gee$alpha
  g0 <- do.call(cbind, lapply(1:ns,function(i){
    M = M[i]
    X = df[df$id==unique.ids[i],-seq(1,4)]
    y = df[df$id==unique.ids[i],1]
    g0_func(y,x=X,M,alpha=alpha,corr=corr)
  }))
  g0 <- apply(g0,1,mean)
  return(abs(g0))
}




