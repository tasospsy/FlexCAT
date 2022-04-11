## FlexCAT project
## (NEW) SVL - Model Estimation
## Tasos Psychogyiopoulos
## c.03/03/2022/ m.6/4/2022

## For Mac
mydir <- "/Users/tasospsy/Google Drive/_UvA/Master Thesis/"

## For VM - AWS
mydir <- "/home/rstudio"

setwd(mydir)
source("FlexCAT/0.Functions.R")

load("simdat1k.Rdata")
#load("simdat.Rdata")

#load("simdatMED.Rdata")
#load("simdat_small.Rdata")

load("true_mods.Rdata")
#load("true_modsMED.Rdata")

truensim1k <- left_join(true_mods, simdat1k, by = 'TrueMod') %>% 
  dplyr::select(TrueMod, Rep, N.y, Dataset, Class)

## ----------------
## MODEL ESTIMATION 
## ----------------
startt <- Sys.time()
plan(multisession, gc = TRUE)
est1k <- truensim1k %>% 
  group_by(Class) %>% 
  mutate(est.Model = future_map2(Dataset,
                                 Class,
                                       ~ esT(
                                         X = .x -1,
                                         type = 'fixed',
                                         from = 1,
                                         to = .y*2,
                                       ))) %>% 
  ungroup()

endt <- Sys.time()
endt - startt

setwd("/home/rstudio/datatorun")
save(est1k, file = 'est1k.Rdata') # Time difference of 7.247467 hours


