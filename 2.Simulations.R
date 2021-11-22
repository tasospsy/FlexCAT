## FlexCAT project
## () LCA: Simulations
## Tasos Psychogyiopoulos
## c. 17.11.2021 / m. 19.11.2021

# setwd("FlexCAT/")  !!
# source("1.Calibration.R") !!
## ------------
### SIMULATIONS
## ------------

## Specify the true  model
Trueclass <- 15
pseudoTrue <- esT(n.class = 'fixed', to = Trueclass, X = tdat)
TrueP <- pseudoTrue$param$crP[[Trueclass]]
Ntrue <-  nrow(tdat) # N = 4211
TrueR <- pseudoTrue$param$R[[Trueclass]]
TrueJ <- ncol(TrueR) # 16
TruePw <- pseudoTrue$param$Pw[[Trueclass]]
Truedens <- pseudoTrue$param$dens[[Trueclass]]

## simulate data using poLCA
Ns <- c(2500, 5000, 7500, 10000)
Reps <- 1000
set.seed(1992)
plan(multisession)
startt <- Sys.time()
dat <- replicate(Reps,
                 future_map(Ns, ~ poLCA.simdata(N = ., 
                                                probs = TrueP, 
                                                nclass = Trueclass, 
                                                ndv = TrueJ,
                                                P = TruePw)$dat),
                 simplify = FALSE)
simBIGdat <- dat
(endt <- Sys.time() - startt)
# Time difference of 5.341289 mins
# save(file = "simBIGdat.Rdat", simBIGdat) #3.2GB!

## BIG SIMULATION !
## ! Time intense !
load("/Users/tasospsy/Google Drive/_UvA/Master Thesis/simBIGdat.Rdat")

## A tibble with the datasets and the N reps
tbldat <- tibble(simBIGdat) 
tbldat <- tbldat %>% 
  add_column(Rep = 1:nrow(tbldat), .before = 1) %>% 
  rename('Dataset' = simBIGdat) %>% 
  unnest_longer(col = Dataset) 

## Add column of Ns
tbldat <- tbldat %>% 
  add_column(N = unlist(imap(tbldat$Dataset, ~print(nrow(.x)))), .after = 1)

## !! 
## Estimate LCMs using the data from the simulations and add them as column 
## next to the datasets. 
plan(multisession, workers = 7 )
set.seed(1992)
startt <- Sys.time()
tblmodels <- tbldat %>% 
  add_column(Model = future_imap(tbl$Dataset, 
                                 ~ esT(n.class = 'fixed', to = 25, X = .-1)))
(endt <- Sys.time() - startt)
# Time difference of 2.914978 hours
# save(file = "tblmodels.Rdat", tblmodels)

 
  


## ------------------------
## Small example for testing

source('FlexCAT/_Replicate_example.R')
## small true model parameters
smallP   <- prbs
smallpA  <- pA
smallL   <- L
small    <- R
smallPw  <- Pw
smallpis <- pis
smTrueclass <- 2
# Reps 
smallReps <- 100
## small simdata 
smallNs <- c(25, 50, 75, 100)
plan(multisession)
startt <- Sys.time()
smalldat <- replicate(smallReps,
                      future_map(smallNs, ~ poLCA.simdata(N = ., 
                                         probs = smallP, 
                                         nclass = smTrueclass, 
                                         ndv = smallL,
                                         P = smallPw)$dat),
                      simplify = FALSE)
## [[1, 2, ..., 100]][[1,2,...,4]]
(endt <- Sys.time() - startt)

## Calculate cells 
smallCells <- expand.grid(R <- 1:100,
                         N <- smallNs,
                         c <- 1:5)
# n 5*4*100 = 2000

## Time intense 

plan(multisession)
set.seed(1992)
startt <- Sys.time()
smallRES <- future_map(smalldat, ~future_map(.,
                                 ~ esT(n.class = 'fixed', to = 5, X = .-1)))
(endt <- Sys.time() - startt)
## 20sec

smallRES[[1]][[4]]$param$dens
smallRES[[100]][[4]]$table.stat


## ---- OLD OLD OLD ! Change it----#

## Estimate Mj models with:
# M ={1,2,...,4} = length(dat)
# j ={1,2,...,25} = N classes
# with M15 the true model

## Time intense 
plan(multisession)
set.seed(1992)
startt <- Sys.time()
Tasos <- future_map(dat, ~ esT(n.class = 'fixed', to = 25, X = .-1),
                     .options = furrr_options(seed = TRUE))
(endt <- Sys.time() - startt)
## 70mins 

#save(file = "Taslist.Rdat", Taslist)
tableall <- map(Tasos, ~.x$table.stat) 
plotall <- map(tableall, plot.fun, show.true.in = 15)
plotall

all.plot <- plotall[[1]]/
  plotall[[2]]/
  plotall[[3]]/
  plotall[[4]]
