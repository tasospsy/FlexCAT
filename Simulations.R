## FlexCAT project
## () LCA: Simulations
## Tasos Psychogyiopoulos
## c. 17.11.2021 / m. 19.11.2021

library(tidyverse)
library(poLCA)

## ------------
### SIMULATIONS
## ------------

## true model
pseudoTrue <- esT(n.class = 'fixed', to = 15, X = tdat)
TrueP <- pseudoTrue$param$crP[[15]]
Trueclass <- 15
Ntrue <-  nrow(tdat) # N = 4211
TrueJ <- ncol(pseudoTrue$param$R[[15]]) # 16
TruePw <- pseudoTrue$param$Pw[[15]]
## simulate data using poLCA
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

## Estimate Mj models with:
# M ={1,2,...,4} = length(dat)
# j ={1,2,...,25} = N classes
# with M15 the true model

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

Taslist <- list(Tasos[[1]]$table.stat,
                Tasos[[2]]$table.stat,
                Tasos[[3]]$table.stat,
                Tasos[[4]]$table.stat)
#save(file = "Taslist.Rdat", Taslist)
plotall <- map(Taslist, plot.fun, show.true.in = 15)
plotall


library(patchwork)
all.plot <- plotall[[1]]/
  plotall[[2]]/
  plotall[[3]]/
  plotall[[4]]
