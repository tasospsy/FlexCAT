No## FlexCAT project
## (NEW) SVL - Model Estimation
## Tasos Psychogyiopoulos
## c.03/03/2022/ m.6/4/2022

## For Mac
mydir <- "/Users/tasospsy/Google Drive/_UvA/Master Thesis/"

## For VM - AWS
mydir <- "/home/rstudio"

setwd(mydir)
source("FlexCAT/0.Functions.R")

load("datatorun/simdat1k.Rdata")
#load("simdat.Rdata")

#load("simdatMED.Rdata")
#load("simdat_small.Rdata")

load("datatorun/true_mods.Rdata")
#load("true_modsMED.Rdata")

truensim1k <- left_join(true_mods, simdat1k, by = 'TrueMod') %>% 
  dplyr::select(TrueMod, Rep, N.y, Dataset, Class)

## ----------------
## MODEL ESTIMATION 
## ----------------
startt <- Sys.time()
plan(multisession, gc = TRUE)
est10 <- truensim1k %>% 
  mutate(Rep = as.numeric(Rep)) %>% 
  filter(Rep <= 10) %>% 
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

setwd("/home/rstudio/efs")
save(est10, file = 'est10.Rdata') # Time difference of 7.247467 hours


