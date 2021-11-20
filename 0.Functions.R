## FlexCAT project
## (0) Functions
## Tasos Psychogyiopoulos

## ----------------------------
## A. FUNCTIONS for CALIBRATION
## ----------------------------
## Fun. A1
## Convert the class condition probability matrices (prbs)
## from poLCA format to the format provided in van der Ark and Smits article.
pR <- prbs 
conPr <- function(pR){
  nc <- nrow(pR[[1]]) # N classes
  t <- do.call(rbind,pR) # merge the lists
  newpR <- list()
  for(i in 1:nc){
    newpR[[i]] <- t[seq(i, nrow(t)-nc+i, nc),] # new lists
  }
  return(newpR)
}

## Fun. A2
## Function to compute Item Score vectors (R)
Response.pat <- function(L, isc = 1:2) {
  tmp <- data.frame(V1 = isc) 
  tmp[,1:L] <- isc
  R <- expand.grid(tmp)
  return(as.matrix(R))
}

## Fun. A3
## Function to compute the density of the item-score vectors (p)

pv <- function(Pw, prbs, L, R, print.patterns = FALSE){
  vt <- c()
  for(i in 1:nrow(R)){
    vt[i] <- 0
    for(lambda in 1:length(Pw)) {
      tmp <- Pw[lambda]
      for(l in 1:L) {
        tmp <- tmp * prbs[[lambda]][l, R[i,l]]
      }
      vt[i] <- vt[i] + tmp
    }
  }
  if(print.patterns) vt <- cbind(vt, R)
  else
    return(vt)
}

## Fun A4
## Main Estimation function 
esT <-  function(X, 
                 n.class = c('fixed', 'explore'),
                 to ,
                 by = c("aic", "bic", "aic3", "aicc", "caic"), 
                 Rep = 1,
                 maxiter = 5000){
  # from Andries:
  if(!is.data.frame(X)) stop('X is not a data.frame')
  if(any(is.na(X))) stop('Some item scores are missing')
  if(!all(unlist(X) %in% 0:1)) stop('Data contain item scores other than 0 and 1')
  
  X <- X - min(X) + 1L # From 0/2 binary to 1/2 to work for poLCA
  J <-  ncol(X) 
  Q <- names(X)
  f <- as.formula(paste0("cbind(", 
                         paste0(Q[-J], sep = ",",collapse = "") , sep = "", 
                         paste0(Q[J], sep = "", ")~ 1", collapse = "")))
  
  .fi <- function(i){
    outputs <- c('N', 'resid.df', 'npar', 'llik', 'Gsq', 'aic', 'bic')
    .out <- poLCA(f, X, nclass = i, nrep = Rep, maxiter = maxiter)
    tmp <- sapply(outputs, function(.) .out[[.]]) %>% t() %>%  as.data.frame 
    tmp$classes <- max(.out$predclass)
    
    tmp$aic3 <- (-2*.out$llik + 3 * .out$npar)
    tmp$aicc <- .out$aic + (2 * .out$npar * (.out$npar + 1)) / (.out$N - .out$npar - 1)
    tmp$caic <- .out$bic - .out$npar
    tmp$`N/Npar` <- .out$N/.out$npar
    tmp$Entropy <- .out %>% poLCA.entropy()
    return(list(model = .out, summary = tmp))
  }
  if(n.class == 'fixed'){
    i <- 0L
    # three outputs: 1. summary table, 
    # 2. model output from poLCA,
    # 3. probabilities from poLCA (slightly redundant)
    .sum <- .model <- .param <- list()
    repeat{
      i <- i + 1
      .t <- .fi(i) # Models estimation
      
      # 1. Store the table summary as list
      .sum[[i]] <- .t$summary 
      # 2. Store the models output 
      .model[[i]] <- .t$model
      # 3. Store probabilities as lists
      .param$Pw[[i]] <- .t$model$P #P(x = c)
      .param$crP[[i]] <- .t$model$probs # P(yj = y| x= c)
      .param$posP[[i]] <- .t$model$posterior # P(x=c|vj = y)
      
      ## Extra from 18/11
      .param$pA[[i]] <- conPr(.t$model$probs) # see tasos' Fun.1
      .param$R[[i]] <- Response.pat(L = J, isc = 0:1) # see tasos' Fun.2
      
      Rtmp <- Response.pat(L = J, isc = 1:2)
      .param$dens[[i]] <- pv(Pw = .param$Pw[[i]],
                             L = J, R = Rtmp, 
                             prbs = .param$pA[[i]], 
                             print.patterns = FALSE) # see tasos' Fun.3
      
      # if reach the specified limit or df < 0 then stop
      if (i == to | .t$summary$resid.df < 0) break
    }
    
    .sum <- do.call(rbind, .sum) # list to table
    
    return(list(table.stat = .sum, model = .model, param = .param))
  }
  if(n.class == 'explore'){
    i <- 0L
    fit <- bestfit <- 1e100
    sum <- list()
    repeat{
      i <- i + 1
      .t <- .fi(i) 
      fit <- .t$model[[by]]
      sum[[i]] <- .t$summary
      if(fit < bestfit){
        bestfit <- fit
        bestK <- i  
        bestModel <- .t
      }
      if (i == bestK + 2) break
    }
    sum <- do.call(rbind ,sum)
    ### !! NEED CHANGE !! 
    bestModel$param$Pw <- .t$model$P #P(x = c)
    bestModel$param$crP <- .t$model$probs# P(yj = y| x= c)
    bestModel$param$posP <- .t$model$posterior # P(x=c|vj = y)
    
    return(list(table.stat = sum, best = bestModel))
  }
}

## ----------------------------
## FUNCTIONS FOR Starting Level
## ----------------------------
