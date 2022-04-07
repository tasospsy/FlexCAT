## FlexCAT project
## (NEW) SVL - Model Estimation
## Tasos Psychogyiopoulos
## c.03/03/2022

mydir <- "/Users/tasospsy/Google Drive/_UvA/Master Thesis/"
setwd(mydir)
source("FlexCAT/0.Functions.R")
load("simdat.Rdata")
load("simdat_small.Rdata")
load("true_mods.Rdata")

truensim <- left_join(true_mods, simdat, by = 'TrueMod') %>% 
  dplyr::select(TrueMod, Rep, N.y, Dataset, Class)

## ----------------
## MODEL ESTIMATION 
## ----------------
startt <- Sys.time()
plan(multisession, gc = TRUE)
est <- truensim %>% 
  group_by(Class) %>% 
  add_column(est.Model = future_imap(.$Dataset,
                                       ~ esT(
                                         type = 'fixed',
                                         from = 2,
                                         to = unique(truensim$Class)*2,
                                         X = . - 1  #1/2 -> 0/1 -> 1/2
                                       )))

#estmodTC3.small <- simdataTC3.small %>% 
#  add_column(est.Model = future_imap(.$Dataset,
#                               ~ esT(
#                                 #type = 'explore',
#                                 #by = 'aic',
#                                 type = 'fixed',
#                                 from = round(3-3+1),
#                                 to = round(3+3),
#                                 X = . - 1  #1/2 -> 0/1 -> 1/2
#                               )))
endt <- Sys.time()
endt - startt
# save(estmod, file = 'estmod.Rdata')
# load('estmod.Rdata')

cl <- makeClusterPSOCK(availableCores())
plan(cluster, workers = cl)
parallel::stopCluster(cl)
## ----------------
## DATA WRANGLING 
## ----------------
# 'fixed'
est.td.small <- est.small %>% unnest_wider(est.Model) %>% 
  unnest_longer(table.stat, names_repair = 'unique') %>% 
  dplyr::select(TrueMod, Rep, N.y, Class, table.stat) %>% 
  group_by(Rep,TrueMod,  N.y, Class) %>% 
  summarize(by.AIC = table.stat$classes[which.min(table.stat$aic)],
            by.AIC3 = table.stat$classes[which.min(table.stat$aic3)],
            by.BIC = table.stat$classes[which.min(table.stat$bic)],
            by.aBIC = table.stat$classes[which.min(table.stat$aic3)]) %>% 
  rowwise() %>% mutate_at(vars(starts_with("by")),  ~.x - Class)

pertable.small <- est.td.small %>% 
  pivot_longer(cols =starts_with('by'), 
               names_to = 'IC', 
               values_to = 'K.hat-K') %>% 
  group_by(TrueMod, N.y, IC, Class) %>% count(`K.hat-K`) %>% 
  filter(`K.hat-K`==0) %>% dplyr::select(-`K.hat-K`) %>% 
  mutate(per = n / 10 * 100) %>% dplyr::select(-n) %>% 
  pivot_wider(names_from = 'IC', values_from = 'per')


plotby <- est.td.small %>%
  pivot_longer(cols =starts_with('by'), 
               names_to = 'IC', 
               values_to = 'K.hat-K') %>% 
  ggplot() +
  #geom_point(aes(y = `K.hat-K`, x= IC, color = IC, shape = IC), 
  #                alpha = .6,
  #            stat = 'identity') +
  geom_boxplot(aes(y = `K.hat-K`, x= IC, color = IC))+
  facet_wrap(~TrueMod+N.y, ncol = 4) +
  theme_minimal()
plotby



# 'explore'
#est.td.TC3.small <- estmodTC3.small %>% unnest_wider(est.Model) %>% 
#  unnest_wider(best, names_repair = 'unique') %>% 
#  dplyr::select(TrueMod, Rep, N.y, Class, est.K)
