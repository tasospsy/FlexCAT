## FlexCAT project
## (2) LCA: Simulations
## Tasos Psychogyiopoulos
## c. 17.11.2021 / m. 31.12.2021


setwd("/Users/tasospsy/Google Drive/_UvA/Master Thesis/")

## LOAD THE DATASETS

# SVL 
load("SVL_clean.Rdat")
SVL <- tdat
# transitive reasoning 1 
load("TRANS1.Rdat")
#J = 12; N = 425
# Transitive Reasoning 2
load("TRANS2.Rdat")
# J = 16; N = 606 

datasets <- list("SVL" = SVL, "TRANS1" = TRANS1, "TRANS2" = TRANS2)

## SPECIFY TRUE MODELS
## TrueSVL : N=4211, J=16, Class=15
## TrueTRANS1: N=425, J=12, Class=5
## TrueTRANS2: N=606, J=16, Class=10

tSVL <- spTrueM(SVL, N = 4211, J = 16, C = 15)
tTR1 <- spTrueM(TRANS1, N = 425, J = 12, C = 5)
tTR2 <- spTrueM(TRANS2, N = 606, J = 16, C = 10)

True <- list("SVL" = tSVL, "TR1" = tTR1, "TR2" = tTR2)

#save(file = "True.Rdat", True)
#load("True.Rdat")

## ---------------
## DATA GENERATION 
## ---------------

## Generate data using poLCA
Ns <- c(500, 1000, 5000, 10000)
Reps <- 100
set.seed(1992)
plan(multisession, workers = 7)

simdat <- list()
for (i in 1:length(True)) {
  simDat[[i]] <- replicate(Reps,
                      future_map(Ns, ~poLCA.simdata(N = ., 
                                                    probs = True[[i]]$P, 
                                                    nclass = True[[i]]$Class, 
                                                    ndv = True[[i]]$J,
                                                    P = True[[i]]$Pw)$dat),
                      simplify = FALSE)
}

save(file = "simDat.Rdat", simDat) 








  
 







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

