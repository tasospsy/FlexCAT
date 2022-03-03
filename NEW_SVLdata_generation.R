## FlexCAT project
## (NEW) SVL - Data Generation
## Tasos Psychogyiopoulos
## c.02/03/2022


mydir <- "/Users/tasospsy/Google Drive/_UvA/Master Thesis/"
setwd(mydir)
source("FlexCAT/0.Functions.R")
## ----------------
## DATA PREPARATION 
## ----------------

## Select the categorical items that make them dichotomous
dat <- read_sav("SVL-i_Vlaanderen_TOT2_HV.sav")

svl_160_dich <- dat %>% 
  dplyr::select(LG1:SW16) %>%      # select specific cols
  mutate_all(., ~ if_else(.x <= 2, 0, 1)) %>% # make all 0 and 1s
  data.frame()                 #  transform to df

#save(svl_160_dich, file = "svl_160_dich.Rdat")

## ---------------
## DATA GENERATION 
## ---------------
## Specifying the true models 
SVL <- svl_160_dich
datas <- list(SVL1 = SVL[1:2], SVL2 = SVL[1:5])

tKs <- c(2, 3, 4) # Conditions for true number of classes (K)
grid <- expand_grid(datas, tKs)
plan(multisession)
startmap <- Sys.time()
true_mods <- future_map2(.x = grid$datas, .y = grid$tKs, 
                         ~spTrueM(X = .x, C = .y))
endmap <- Sys.time()
endmap - startmap
# Time difference of 20.16773 mins

true_mods  %<>%  tibble() %>% unnest_wider(c(.))

## Generate data using poLCA
Ns <- c(250, 500, 1000, 2000)
Reps <- 10
set.seed(1992)

simdat <- true_mods %>% rowwise() %>% 
  mutate(datalist = list(list(replicate(Reps,
                                   map(Ns,
                                       ~poLCA.simdata(N = .,
                                            probs = P,
                                            nclass = Class,
                                            ndv = J,
                                            P = Pw)$dat),
                              simplify = FALSE)))) %>% 
  dplyr::select(datalist) %>% # drop the true params
  unnest(c(datalist)) %>% 
  add_column(TrueMod = paste0('True.', 1:nrow(.))) %>% 
  unnest(datalist) %>% 
  group_by(TrueMod) %>% 
    mutate(Rep = paste(1:Reps)) %>%
    ungroup() %>% 
  mutate(datalist = imap(datalist, ~set_names(., as.character(Ns)))) %>% 
  unnest_wider(datalist) %>% 
  pivot_longer(c(-TrueMod, -Rep), names_to = 'N', values_to = 'Dataset')

mydir <- "/Users/tasospsy/Google Drive/_UvA/Master Thesis/"
setwd(mydir)
save(simdat, file = 'simdat.Rdat')


