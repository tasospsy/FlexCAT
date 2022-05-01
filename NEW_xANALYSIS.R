## FlexCAT project
## (NEW) ANALYSIS
## Tasos Psychogyiopoulos
## c.03/03/2022/ m.1/5/2022

## ------------------
## 1st STEP Khat - K
## ------------------

## from R -server
setwd("/home/rstudio")
## from mac
setwd("/Users/tasospsy/Google Drive/_UvA/Master Thesis/")

source("FlexCAT/0.Functions.R")
## Load files output from EFS
setwd("/home/rstudio/efs")
load("true_mods.Rdata")
load('out.Rdata') # 43GB!

## TIDY PART
out.td <- out %>% unnest_wider(est.Model) %>% 
  unnest_longer(table.stat, names_repair = 'unique') %>% 
  dplyr::select(TrueMod, Rep, N.y, Class, table.stat) %>% 
  group_by(Rep, TrueMod,  N.y, Class) %>% 
  summarize(byAIC = table.stat$classes[which.min(table.stat$aic)],
            byAIC3 = table.stat$classes[which.min(table.stat$aic3)],
            byBIC = table.stat$classes[which.min(table.stat$bic)],
            byaBIC = table.stat$classes[which.min(table.stat$aBIC)]) %>% 
  rowwise() %>% mutate_at(vars(starts_with("by")),  ~.x - Class) %>% 
  left_join(true_mods , by = 'TrueMod') %>% 
  dplyr::select(-Class.y, -P, -N, -R, -Pw, -dens)%>% 
  rename_at(vars(starts_with('by')), ~str_remove(.x,'by')) %>% 
  rename('K' = 'Class.x', 'N' = 'N.y')

#setwd("/home/rstudio/efs")
#save(out.td,file ='out-td.Rdata')

pertable <- out.td %>% 
  pivot_longer(cols = c(AIC,AIC3,BIC,aBIC), 
               names_to = 'IC', 
               values_to = 'K.hat-K') %>% 
  group_by(TrueMod, N, IC, K, J) %>% count(`K.hat-K`) %>% 
  filter(`K.hat-K`==0) %>% dplyr::select(-`K.hat-K`) %>% 
  mutate(per = n / 100 * 100) %>% dplyr::select(-n) %>% 
  pivot_wider(names_from = 'IC', values_from = 'per') 

#save(pertable,file ='pertable.Rdata')


## ------------------
## 2nd STEP p Vs pha
## ------------------

## Add density of sum scores (dens.ss) in TRUE MODELS
true_mods <- true_mods %>% 
  rowwise() %>%
  mutate(true.dens.ss = list(start.level(density = dens, R = R-1)$px.plus))

# 30/4/2022
#setwd("/home/rstudio/efs")
#save(true_mods,file ='true_mods.Rdata')

## WARNING: TIME INTESIVE ~ 5 HRS!! ~40GB!
out2.td <- out %>% unnest_wider(est.Model) %>% 
  left_join(true_mods , by = 'TrueMod') %>%
  unnest(table.stat, names_repair = 'unique') %>% 
  dplyr::select(TrueMod, Rep, N.y, J, Class.x, classes, param, dens) %>%
  unnest_wider(param, names_repair = 'unique') %>% 
  dplyr::select(-Pw, -crP, -posP, -pA, R) %>% 
  rename('K' = 'Class.x', 'N' = 'N.y', 'est.dens' = 'dens...12',
         'true.dens' ='dens...13', est.K = 'classes') %>% 
  # select the right vector of the list (list[[est.K]])
  mutate(est.dens = map2(.x = est.dens, .y = est.K, ~ .x[[.y]]),
         R = map2(.x = R, .y = est.K, ~ .x[[.y]])) %>% 
  # calculate P+
  mutate(est.dens.ss = map2(.x = est.dens, .y = R, 
                            ~start.level(density = .x, R = .y)$px.plus)) %>% 
  # join true P+
  left_join(true_mods %>% dplyr::select(TrueMod, true.dens.ss) , by = 'TrueMod')

# 30/4/2022 ! 45GB !
#setwd("/home/rstudio/efs")
#save(out2.td,file ='out2.td.Rdata')

## Compute KL distances and remove the densities
KL2 <- out2.td %>% 
  mutate(KL.Pp = unlist(map2(.x =est.dens.ss, 
                             .y = true.dens.ss,
                             ~kullback_leibler_distance(P = .x, # P
                                                        Q = .y, #Q
                                                        testNA = FALSE, unit ="log", 
                                                        epsilon = 0.000000001)))) %>%
  mutate(KL.P = unlist(map2(.x =est.dens, 
                            .y =true.dens,
                            ~kullback_leibler_distance(P = .x, # P
                                                       Q = .y, #Q
                                                       testNA = FALSE, unit ="log", 
                                                       epsilon = 0.000000001)))) %>%        
  dplyr::select(-R, -true.dens, -est.dens, -est.dens.ss, -true.dens.ss)

save(KL2,file ='KL2.Rdata')


out2.td

