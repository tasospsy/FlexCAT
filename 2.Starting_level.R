## FlexCAT project
## (2) LCA: Starting Level
## Tasos Psychogyiopoulos

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
