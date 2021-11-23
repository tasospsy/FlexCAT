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

TrueModel <- list(Trueclass = Trueclass,
                TrueP = TrueP,
                Ntrue =  Ntrue ,
                TrueR =  TrueR ,
                TrueJ =  TrueJ ,
                TruePw = TruePw,
                Truedens = Truedens)

# save(file = "TrueModel.Rdat", TrueModel)

## simulate data using poLCA
Ns <- c(2500, 5000, 7500, 10000)
Reps <- 10#00
set.seed(1992)
plan(multisession)
startt <- Sys.time()
dat10 <- replicate(Reps,
                 future_map(Ns, ~ poLCA.simdata(N = ., 
                                                probs = TrueP, 
                                                nclass = Trueclass, 
                                                ndv = TrueJ,
                                                P = TruePw)$dat),
                 simplify = FALSE)
simBIGdat <- dat
(endt <- Sys.time() - startt)
dat10
# Time difference of 5.341289 mins
# save(file = "simBIGdat.Rdat", simBIGdat) #3.2GB!
#load("simBIGdat.Rdat")

## BIG SIMULATION !
## ! Time intense !
load("/Users/tasospsy/Google Drive/_UvA/Master Thesis/simBIGdat.Rdat")

## A tibble with the datasets and the N reps
tbldat <- tibble(dat10) 
tbldat <- tbldat %>% 
  add_column(Rep = 1:nrow(tbldat), .before = 1) %>% 
  rename('Dataset' = dat10) %>% 
  unnest_longer(col = Dataset) 

## Add column of Ns
tbldat <- tbldat %>% 
  add_column(N = unlist(imap(tbldat$Dataset, ~print(nrow(.x)))), .after = 1)

## !! 5 Reps only
## Estimate LCMs using the data from the simulations and add them as column 
## next to the datasets. 
plan(multisession)
set.seed(1992)
startt <- Sys.time()
tblmodels <- tbldat %>% 
  add_column(Model = future_imap(tbldat$Dataset, 
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



