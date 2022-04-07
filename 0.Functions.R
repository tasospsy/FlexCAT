## FlexCAT project
## (0) Functions
## Tasos Psychogyiopoulos

## Load or Install packages Function
load.or.install <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, 1])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

## load required packages
req_pckgs <-
  c("tidyverse", "haven", "poLCA", "patchwork", "furrr", "mokken")
load.or.install(req_pckgs)

## ----------------------------
## A. FUNCTIONS for CALIBRATION
## ----------------------------
## Fun. A1
## Convert the class condition probability matrices (prbs)
## from poLCA format to the format provided in van der Ark and Smits article.
conPr <- function(pR) {
  nc <- nrow(pR[[1]]) # N classes
  t <- do.call(rbind, pR) # merge the lists
  newpR <- list()
  for (i in 1:nc) {
    newpR[[i]] <- t[seq(i, nrow(t) - nc + i, nc), ] # new lists
  }
  return(newpR)
}

## Fun. A2
## Function to compute Item Score vectors (R)
Response.pat <- function(L, isc = 1:2) {
  tmp <- data.frame(V1 = isc)
  tmp[, 1:L] <- isc ### L: 10
  R <- expand.grid(tmp)
  return(as.matrix(R))
}
Response.pat(26)
## Fun. A3
## Function to compute the density of the item-score vectors (p)

pv <- function(Pw, prbs, L, R, print.patterns = FALSE) {
  vt <- c()
  for (i in 1:nrow(R)) {
    vt[i] <- 0
    for (lambda in 1:length(Pw)) {
      tmp <- Pw[lambda]
      for (l in 1:L) {
        tmp <- tmp * prbs[[lambda]][l, R[i, l]]
      }
      vt[i] <- vt[i] + tmp
    }
  }
  if (print.patterns)
    vt <- cbind(vt, R)
  else
    return(vt)
}

## Fun A4
## Main Estimation function
esT <-  function(X,
                 type = c('fixed', 'explore', 'given.true.class'),
                 from,
                 to,
                 keep.last.only = FALSE,
                 by = c("aic", "bic", "aic3", "aBIC", "caic"),
                 Rep = 1,
                 maxiter = 5000) {
  # from Andries:
  if (!is.data.frame(X))
    stop('X is not a data.frame')
  if (any(is.na(X)))
    stop('Some item scores are missing')
  if (!all(unlist(X) %in% 0:1))
    stop('Data contain item scores other than 0 and 1')
  
  X <- X - min(X) + 1L # From 0/1 binary to 1/2 to work for poLCA
  J <-  ncol(X)
  Q <- names(X)
  f <- as.formula(paste0(
    "cbind(",
    paste0(Q[-J], sep = ",", collapse = "") ,
    sep = "",
    paste0(Q[J], sep = "", ")~ 1", collapse = "")
  ))
  
  .fi <- function(i) {
    outputs <- c('N', 'aic', 'bic', 'resid.df')
    #outputs <- c(outputs, c('npar', 'llik', 'Gsq'))
    .out <-
      poLCA(
        f,
        X,
        nclass = i,
        nrep = Rep,
        maxiter = maxiter,
        verbose = FALSE
      )
    tmp <-
      sapply(outputs, function(.)
        .out[[.]]) %>% t() %>%  as.data.frame
    tmp$classes <- max(.out$predclass)
    
    ## Extra IC
    tmp$aic3 <- (-2 * .out$llik + 3 * .out$npar)
    tmp$aBIC <- (-2 * .out$llik + .out$npar * log((.out$N + 2) / 24))
    
    # Out from 30/11 to save space
    #tmp$caic <- .out$bic - .out$npar
    #tmp$`N/Npar` <- .out$N/.out$npar
    #tmp$Entropy <- .out %>% poLCA.entropy()
    return(list(model = .out,
                summary = tmp))
  }
  ## A.
  ## type == 'fixed'
  if (type == 'fixed') {
    # update from 3/3/22
    if (keep.last.only) {
      i <- to - 1
      .sum <- .model <- .param <- list()
      repeat {
        i <- i + 1
        .t <- .fi(i) # Models estimation
        .sum <- .t$summary
        .param$Pw <- .t$model$P #P(x = c)
        .param$crP <- .t$model$probs # P(yj = y| x= c)
        .param$posP <- .t$model$posterior # P(x=c|vj = y)
        .param$pA <- conPr(.t$model$probs) # see tasos' Fun.1
        .param$R <-
          Response.pat(L = J, isc = 1:2) # see tasos' Fun.2
        .param$dens <- pv(
          Pw = .param$Pw,
          L = J,
          R = .param$R,
          prbs = .param$pA,
          print.patterns = FALSE
        ) # see tasos' Fun.3
        if (i == to | .t$summary$resid.df < 0)
          break
      }
      return(list(table.stat = .sum, param = .param))
      
    } else {
      i <- from 
      # three outputs: 1. summary table,
      # 2. model output from poLCA,
      # 3. probabilities from poLCA (slightly redundant)
      .sum <- .model <- .param <- list()
      repeat {
        i <- i + 1
        .t <- .fi(i) # Models estimation
        
        # 1. Store the table summary as list
        .sum[[i]] <- .t$summary
        
        # 2. Store the models output
        ##out from 30/11 to save space
        #.model[[i]] <- .t$model
        
        # 3. Store probabilities as lists
        .param$Pw[[i]] <- .t$model$P #P(x = c)
        .param$crP[[i]] <- .t$model$probs # P(yj = y| x= c)
        .param$posP[[i]] <- .t$model$posterior # P(x=c|vj = y)
        
        ## Extra from 18/11
        .param$pA[[i]] <- conPr(.t$model$probs) # see tasos' Fun.1
        .param$R[[i]] <-
          Response.pat(L = J, isc = 1:2) # see tasos' Fun.2
        .param$dens[[i]] <- pv(
          Pw = .param$Pw[[i]],
          L = J,
          R = .param$R[[i]],
          prbs = .param$pA[[i]],
          print.patterns = FALSE
        ) # see tasos' Fun.3
        
        # if reach the specified limit or df < 0 then stop
        if (i == to | .t$summary$resid.df < 0)
          break
      }
      
      .sum <- do.call(rbind, .sum) # list to table
      
      return(list(table.stat = .sum,
                  #model = .model,
                  param = .param))
    }
    
  } 
  # update from 7/3/22
  
  
  # if type = 'fixed' end
  
  ## B.
  ## type == 'explore'Old solution from Andries ('bestK + 2')
  if (type == 'explore') {
    i <- 0L
    fit <- bestfit <- 1e100
    sum <- list()
    repeat {
      i <- i + 1
      .t <- .fi(i)
      fit <- .t$model[[by]]
      sum[[i]] <- .t$summary
      if (fit < bestfit) {
        bestfit <- fit
        bestK <- i
        bestModel <- .t
      }
      if (i == bestK + 2)
        break
    }
    sum <- do.call(rbind , sum)
    ### !! NEED CHANGE !!
    bestModel$param$Pw <- .t$model$P #P(x = c)
    bestModel$param$crP <- .t$model$probs# P(yj = y| x= c)
    bestModel$param$posP <- .t$model$posterior # P(x=c|vj = y)
    
    return(list(table.stat = sum, best = bestModel, 
                est.K = bestK))
  }
}

## ----------------------------
## FUNCTIONS FOR Starting Level
## ----------------------------
## FlexCAT project
## (2) LCA: Starting Level
## Tasos Psychogyiopoulos

## --------------
## Starting Level
## --------------
## Fun. B1
start.level <- function(R, density) {
  ## Total scores of the item score vector (?)
  # R <- Response.pat(L = L, isc = 0:1)
  r.plus <- R %*% matrix(1, nrow = ncol(R)) # by Andries
  ## All possible total scores x.plus
  x.plus <- matrix(seq(0, ncol(R)), ncol = 1)
  ## Q design matrix to relate p to px.+
  Q <- matrix(NA, ncol = nrow(x.plus), nrow = nrow(r.plus))
  for (i in 1:nrow(Q)) {
    # number of possible patterns
    for (j in 1:ncol(Q)) {
      # number of possible total scores
      ifelse(r.plus[i] == x.plus[j], Q[i, j] <- 1, Q[i, j] <- 0) # ?
    }
  }
  ## TOTAL SCORE DENSITY!
  if (is.matrix(density))
    px.plus <-  t(Q) %*% density
  if (is.numeric(density)) {
    density <- matrix(density, ncol = 1)
    px.plus <-  t(Q) %*% density
  }
  return(list(
    r.plus = r.plus,
    x.plus = x.plus,
    Q = Q,
    px.plus = px.plus
  ))
}

## ----------------------------
## SPECIFY TRUE MODELS FUNCTION
## ----------------------------
spTrueM <- function(X, C) {
  C <- C
  pseudo <- esT(type = 'fixed', 
                to = C, 
                X = X, 
                keep.last.only = TRUE)
  P <- pseudo$param$crP
  N <- nrow(X)
  R <- pseudo$param$R
  J <- ncol(R)
  Pw <- pseudo$param$Pw
  dens <- pseudo$param$dens
  return(list(
    Class = C,
    P = P,
    N = N,
    R = R,
    J = J,
    Pw = Pw,
    dens = dens
  ))
}

## ------------------------
## TIDY - ESTIMATE FUNCTION  ### REMOVED from 3/3/2022
## ------------------------
# TidyEstFun <- function(t, class.to = 25, mc = 7) {
#   ## A tibble with the datasets and the N reps
#   tbldat <- tibble(t)
#   tbldat <- tbldat %>%
#     add_column(Rep = 1:nrow(tbldat), .before = 1) %>%
#     rename('Dataset' = t) %>%
#     unnest_longer(col = Dataset)
#   
#   ## Add column of Ns
#   tbldat <- tbldat %>%
#     add_column(N = unlist(imap(tbldat$Dataset, ~ print(nrow(
#       .x
#     )))), .after = 1)
#   
#   ## !! 10 Reps only
#   ## Estimate LCMs using the data from the simulations and add them as column
#   ## next to the datasets.
#   plan(multisession, workers = mc)
#   tblmod. <- tbldat %>%
#     add_column(Model = future_imap(tbldat$Dataset,
#                                    ~ esT(
#                                      type = 'fixed',
#                                      to = class.to,
#                                      X = . - 1
#                                    )))
#   return(tblmod.)
# }

## --------------------------------------------
## Function to subset a nested list into chunks
## --------------------------------------------
sliceL <- function(List, chunks) {
  Chunks <- list()
  for (i in 1:chunks) {
    Chunks[[i]] <-
      List[seq(1, length(List), by = length(List) / chunks)[i]:seq(length(List) /
                                                                     chunks, length(List), by = length(List) / chunks)[i]]
  }
  return(tibble(Chunks))
}
