## FlexCAT project
## (2) LCA: Simulations
## Tasos Psychogyiopoulos
## c. 17.11.2021 / m. 31.12.2021


## ------------
### SIMULATIONS
## ------------

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

## per dataset
SVLdat <- simDat[[1]]
TR1dat <- simDat[[2]]
TR2dat <- simDat[[3]]





## NEW !
## For 10 Reps, NEW Ns:
## CHUNK 1-10
#tblmodels10b <- tblmodels10
#save(file = "tblmodels10b", tblmodels10b)
### CHUNK 11-30
#tblmodels111_30
#save(file = "tblmodels111_30.Rdat", tblmodels111_30)
### CHUNK 31-40
#set.seed(1992)
#startt <- Sys.time()
#tblmod.31_40 <- TidyEstFun(dat.31_40)
#(endt <- Sys.time() - startt)
#save(file = "tblmod.31_40.Rdat", tblmod.31_40)
#rm(tblmod.31_40)
## CHUNK 41-50
set.seed(1992)
tblmod.41_50  <- TidyEstFun(dat.41_50 )
save(file = "tblmod.41_50.Rdat", tblmod.41_50)
rm(tblmod.41_50)
## CHUNK 51-60
set.seed(1992)
tblmod.51_60  <- TidyEstFun(dat.51_60 )
save(file = "tblmod.51_60.Rdat", tblmod.51_60)
## CHUNK 61-70
set.seed(1992)
tblmod.61_70  <- TidyEstFun(dat.61_70)
save(file = "tblmod.61_70.Rdat", tblmod.61_70)
## CHUNK 71-80
set.seed(1992)
tblmod.71_80  <- TidyEstFun(dat.71_80 )
save(file = "tblmod.71_80.Rdat", tblmod.71_80)
## CHUNK 81-90
set.seed(1992)
tblmod.81_90  <- TidyEstFun(dat.81_90 )
save(file = "tblmod.81_90.Rdat", tblmod.81_90)
## CHUNK 91-100
set.seed(1992)
tblmod.91_100 <- TidyEstFun(dat.91_100)
save(file = "tblmod.91_100.Rdat", tblmod.91_100)




  
 






  
## For 5 Reps, OLD Ns:
# Time difference of 2.914978 hours
# save(file = "tblmodels.Rdat", tblmodels)
  
  
  ## For 10 Reps, OLD Ns:
# Time difference of 5.390989 hours
#save(file = "tblmodels10.Rdat", tblmodels10)

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



