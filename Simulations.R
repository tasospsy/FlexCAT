## FlexCAT project
## (2) LCA: Simulations
## Tasos Psychogyiopoulos
## c. 17.11.2021 / m. 19.11.2021

library(tidyverse)
library(poLCA)

## --------------------------
### REPLICATE ARTICLE EXAMPLE
## --------------------------

## specify class-conditional probabilities in poLCA format
## ncol = N of responses (e.g., 0,1)
## nrow = nClasses
prbs <- list(matrix(c(0.3,0.7,
                      0.6, 0.4),ncol=2,byrow=TRUE), # P(L=1|class)
             matrix(c(0.2,0.8,
                      0.9, 0.1),ncol=2,byrow=TRUE), # P(L=2|class)
             matrix(c(0.1,0.9,
                      0.6, 0.4),ncol=2,byrow=TRUE)) # P(L=3|class)

## Convert the class condition probability matrices (prbs)
## from poLCA format to the format provided in van der Ark and Smits article.
pA <- conPr(pR) 
L <- 3 # number of items
R <- Response.pat(L = L, isc = 1:2) # for item scores 1:2 
Pw <- c(.2, .8)   # Class weights 

#compute the density of the item-score vectors (p)
pis <- pv(Pw = Pw, L = L, R = R, prbs = pA, print.patterns = FALSE)  

## --------------
## Starting Level
## --------------
## Fun. B1
start.level <- function(R, density){
  ## Total scores of the item score vector (?)
  # R <- Response.pat(L = L, isc = 0:1) 
  r.plus <- R %*% matrix(1, nrow = ncol(R)) # by Andries
  
  ## All possible total scores x.plus
  x.plus <- matrix(seq(0, ncol(R)), ncol = 1)
  
  ## Q design matrix to relate p to px.+
  Q <- matrix(NA, ncol = nrow(x.plus), nrow = nrow(r.plus))
  for(i in 1:nrow(Q)){ # number of possible patterns
    for(j in 1:ncol(Q)){ # number of possible total scores
      ifelse(r.plus[i] == x.plus[j], Q[i,j] <- 1, Q[i,j] <- 0) # ?
    }
  }
  ## TOTAL SCORE DENSITY! 
  px.plus <-  t(Q)%*%density 
  return(list(r.plus = r.plus, x.plus = x.plus, Q = Q, px.plus = px.plus))
}


## ------------
### SIMULATIONS
## ------------
pseudoTrue <- esT(n.class = 'fixed', to = 15, X = tdat)
TrueP <- pseudoTrue$param$crP[[15]]
Trueclass <- 15
TrueJ <- ncol(pseudoTrue$param$R[[15]])
TruePw <- pseudoTrue$param$Pw[[15]]

Ns <- c(2500, 5000, 7500, 10000)
dat <- list()
for(i in 1:length(Ns)) {
  dat[[i]] <- poLCA.simdata(N = Ns[i], 
                            probs = TrueP, 
                            nclass = Trueclass, 
                            ndv = TrueJ,
                            P = TruePw) 
  dat[[i]] <- dat[[i]]$dat
}

## Time intense 
library(furrr) 
library(tictoc)
set.seed(1992)

plan(multisession)

tic()
Tasos <- future_map(dat, ~ esT(n.class = 'fixed', to = 25, X = .-1),
                     .options = furrr_options(seed = TRUE))
toc()
## 70mins 
Tasos[[1]]$table.stat
Taslist <- list(Tasos[[1]]$table.stat,
                Tasos[[2]]$table.stat,
                Tasos[[3]]$table.stat,
                Tasos[[4]]$table.stat)

plotall <- map(Taslist, plot.fun, show.true.in = 15)
plotall


ncol(pseudoTrue$param$R[[15]])







