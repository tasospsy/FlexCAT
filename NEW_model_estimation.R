## FlexCAT project
## (NEW) SVL - Model Estimation
## Tasos Psychogyiopoulos
## c.03/03/2022/ m.6/4/2022

mydir <- "/Users/tasospsy/Google Drive/_UvA/Master Thesis/"
setwd(mydir)
source("FlexCAT/0.Functions.R")
load("simdat.Rdata")

load("simdatMED.Rdata")
load("simdat_small.Rdata")

load("true_mods.Rdata")
load("true_modsMED.Rdata")

truensimMED <- left_join(true_modsMED, simdatMED, by = 'TrueMod') %>% 
  dplyr::select(TrueMod, Rep, N.y, Dataset, Class)

## ----------------
## MODEL ESTIMATION 
## ----------------
startt <- Sys.time()
plan(multisession, gc = TRUE)
esttest <- truensimMED %>% 
  ## --- test
  filter(Rep ==1, N.y ==500) %>% 
  ## -- end test
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

## test
esttest <- esttest %>% unnest_wider(est.Model) %>% 
  unnest_longer(table.stat, names_repair = 'unique')  
  
esttest$table.stat$classes %>% unique()
## end test
endt <- Sys.time()
endt - startt
save(estMED, file = 'estMED.Rdata') # Time difference of 7.247467 hours
load('estMED.Rdata')


## ------------------
## 1st STEP Khat - K
## ------------------
# 'fixed'
esttest.rd <- esttest %>% unnest_wider(est.Model) %>% 
  unnest_longer(table.stat, names_repair = 'unique') %>% 
  esttest.rd <- esttest %>%  dplyr::select(TrueMod, Rep, N.y, Class, table.stat) %>% 
  group_by(Rep, TrueMod,  N.y, Class) %>% 
  summarize(byAIC = table.stat$classes[which.min(table.stat$aic)],
            byAIC3 = table.stat$classes[which.min(table.stat$aic3)],
            byBIC = table.stat$classes[which.min(table.stat$bic)],
            byaBIC = table.stat$classes[which.min(table.stat$aBIC)]) %>% 
  rowwise() %>% mutate_at(vars(starts_with("by")),  ~.x - Class) %>% 
  left_join(true_modsMED , by = 'TrueMod') %>% 
  dplyr::select(-Class.y, -P, -N, -R, -Pw, -dens)%>% 
  rename_at(vars(starts_with('by')), ~str_remove(.x,'by')) %>% 
  rename('K' = 'Class.x', 'N' = 'N.y')

pertable <- est.tdS %>% 
  pivot_longer(cols = c(AIC,AIC3,BIC,aBIC), 
               names_to = 'IC', 
               values_to = 'K.hat-K') %>% 
  group_by(TrueMod, N, IC, K, J) %>% count(`K.hat-K`) %>% 
  filter(`K.hat-K`==0) %>% dplyr::select(-`K.hat-K`) %>% 
  mutate(per = n / 50 * 100) %>% dplyr::select(-n) %>% 
  pivot_wider(names_from = 'IC', values_from = 'per') 


plotby <- est.tdS %>%
  pivot_longer(cols = c(AIC,AIC3,BIC,aBIC), 
               names_to = 'IC', 
               values_to = 'K.hat-K') %>% 
  mutate(J = case_when(J== 7 ~ 'J = 7',
                       J== 15 ~ 'J = 15'),
         K = case_when(K == 4 ~  'K = 4',
                             K == 8 ~  'K = 8',
                             K == 12 ~ 'K = 12')) %>% 
  ggplot() +
  geom_boxplot(aes(y = `K.hat-K`, 
                   x= reorder(IC, -`K.hat-K`), color = IC),
               outlier.size = .5)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey50", size=.2)+
  facet_grid(fct_relevel(N,'500','1000','2000', '5000') ~ 
               fct_relevel(J, 'J = 7', 'J = 15') +
               fct_relevel(K, 'K = 4', 'K = 8', 'K = 12')) +
  xlab("") + 
  ylab(expression( hat(K) - trueK )) +
  labs(title = 'Distribution of the outcome variable over the conditions')+
  theme_bw() +   
  theme1 
plotby


theme1 <- theme(plot.background = element_rect(fill = "white", color = NA), #background color
                text = element_text(family = "mono", color = "black"), # color of all text in the plot 
                plot.title = element_text(hjust = 0.5, color = "black", size = 10), # specs of the title
                strip.text = element_text(colour = "black", size = 12), # specs of the text inside plot
                panel.grid.major.x = element_line(size = .5), # change the grid layout
                panel.grid.major.y = element_line(size = 1), # change the grid layout
                panel.grid.minor.x = element_blank(), # remove the grid layout
                panel.grid.minor.y = element_blank(), # remove the grid layout
                axis.text=element_text(size=12, color = "black"), # specs of the text in axis
                axis.text.x = element_blank(),
                legend.position = "bottom", # legend down
                legend.title = element_blank(), # remove legend title,
                legend.text = element_text(colour = "black", size = 9)
)


## ------------------
## 2nd STEP p Vs phat
## ------------------

library(philentropy)
est2.tdMED <- estMED %>% unnest_wider(est.Model) %>% 
  left_join(true_modsMED , by = 'TrueMod') %>%
  unnest(table.stat, names_repair = 'unique') %>% 
  dplyr::select(TrueMod, Rep, N.y, J, Class.x, classes, param, dens)%>%
  unnest_wider(param, names_repair = 'unique') %>% 
  dplyr::select(-Pw, -crP, -posP, -pA, -R) %>% 
  rename('K' = 'Class.x', 'N' = 'N.y', 'est.dens' = 'dens...12',
         'true.dens' ='dens...13', est.K = 'classes') 
  unnest_longer(est.dens) 

est2.tdMED <- est2.tdMED %>%
  add_column(KL.ds.p = map2(.x = .$est.dens,
                            .y = .$true.dens,
                            ~kullback_leibler_distance(.y, # P
                                                       .x, #Q
                                                       testNA = FALSE, unit ="log"))) 

est2.tdMED %>% mutate('T' = unlist(map2(K, est.K, ~.x+.y)))
