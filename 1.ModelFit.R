## FlexCAT project
## (1) LCA: Model Fit 
## Tasos Psychogyiopoulos
## c.05/10/2021/ m. 10.11.2021

## libraries
library(tidyverse)
## data from github
source(url("https://raw.githubusercontent.com/tasospsy/FlexCAT/main/0.Cleaning-Translation.R"))

## Subsets of Data
X08 <- tdat %>% slice_sample(prop = 0.8) # random proportion of 80%
X01 <- tdat %>% slice_sample(prop = 0.1)
X02 <- tdat %>% slice_sample(prop = 0.2)
X05 <- tdat %>% slice_sample(prop = 0.5)

#### LCA ####
library(poLCA)
#### ESTIMATION FUN ####
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
    return(list(model = .out, summary = tmp))
  }
  if(n.class == 'fixed'){
    i <- 0L
    sum <- list()
    repeat{
      i <- i + 1
      .t <- .fi(i)
      sum[[i]] <- .t$summary
        if (i == to | .t$summary$resid.df < 0) break
      }
    
    sum <- do.call(rbind ,sum)
    
    ## ! stores the params only for the last model
    .t$param$Pw <- .t$model$P #P(x = c)
    .t$param$crP <- .t$model$probs# P(yj = y| x= c)
    .t$param$posP <- .t$model$posterior # P(x=c|vj = y)
    
    return(list(table.stat = sum, model = .t))
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
    bestModel$param$Pw <- .t$model$P #P(x = c)
    bestModel$param$crP <- .t$model$probs# P(yj = y| x= c)
    bestModel$param$posP <- .t$model$posterior # P(x=c|vj = y)
    
    return(list(table.stat = sum, best = bestModel))
  }
}

start.script <- Sys.time()

## Output "EXPLORE"
out01  <- esT(n.class = 'explore', X = X01, by = 'aic')
out02  <- esT(n.class = 'explore', X = X02, by = 'aic')
out05  <- esT(n.class = 'explore', X = X05, by = 'aic')
out08  <- esT(n.class = 'explore', X = X08, by = 'aic')
#outall <- esT(n.class = 'explore', X = tdat, by = 'aic')

out.list <- list(
    out01$table.stat,
    out02$table.stat,
    out05$table.stat,
    out08$table.stat
    #,outall$table.stat
    )

## Output "FIXED"
out.fix01 <- esT(n.class = 'fixed', to = 30, X = X01)
out.fix02 <- esT(n.class = 'fixed', to = 30, X = X02)
out.fix05 <- esT(n.class = 'fixed', to = 30, X = X05)
out.fix08 <- esT(n.class = 'fixed', to = 30, X = X08)
#out.fixall <- esT(n.class = 'fixed', to = 30, X = tdat)

out.fix.list <- list(
  out.fix01$table.stat ,
  out.fix02$table.stat ,
  out.fix05$table.stat ,
  out.fix08$table.stat 
  #out.fixall
)

## Visualization

plot.fun <- function(d){
  plotICs <- d %>% 
    filter(resid.df >=0) %>% 
    dplyr::select(aic, bic, aic3, aicc, caic, classes, N) %>% 
    filter(classes > 01) %>%  #bc the 1st ruins the plot line
    gather(key = "Index", value = "value", -classes, -N) %>% 
    ggplot() + 
    geom_line(aes(x = classes, y = value, color = Index), 
              alpha = 1, show.legend = TRUE) +
    scale_x_continuous(limits = c(0, 30), breaks = 0:30) + 
    
    theme_bw()
  # facet_wrap(~Index, scales = 'free', nrow = 2)
  return(plotICs)
}
plot.fun(out.fix01$table.stat)
# Explored
plot.list <- map(out.list, plot.fun)

#Fixed
plot.fix.list <- map(out.fix.list, plot.fun)

library(patchwork)

all.exploreplots <- plot.list[[1]] / plot.list[[2]] / plot.list[[3]] / plot.list[[4]]
all.exploreplots

all.fixplots <- plot.fix.list[[1]] /
  plot.fix.list[[2]] /
  plot.fix.list[[3]] /
  plot.fix.list[[4]]
all.fixplots

end.script <- Sys.time()
end.script - start.script
# Time difference of 34.22859 mins