## ------------------
## 1st STEP Khat - K
## ------------------

setwd("/home/rstudio")
source("FlexCAT/0.Functions.R")
## Load files output from EFS
setwd("/home/rstudio/efs")
load("true_mods.Rdata")
load("est1k.Rdata")
out1 <- est1k
load("est1k2.Rdata")
out2 <- est1k2
rm(est1k2)
rm(est1k)
# Merge them
out <- bind_rows(out1, out2)
rm(out1)
rm(out2)
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

setwd("/home/rstudio/efs")
save(out.td,file ='out-td.Rdata')

pertable <- out.td %>% 
  pivot_longer(cols = c(AIC,AIC3,BIC,aBIC), 
               names_to = 'IC', 
               values_to = 'K.hat-K') %>% 
  group_by(TrueMod, N, IC, K, J) %>% count(`K.hat-K`) %>% 
  filter(`K.hat-K`==0) %>% dplyr::select(-`K.hat-K`) %>% 
  mutate(per = n / 50 * 100) %>% dplyr::select(-n) %>% 
  pivot_wider(names_from = 'IC', values_from = 'per') 
  
save(pertable,file ='pertable.Rdata')
  
plotby <- out.td %>%
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
  geom_hline(yintercept=c(-10, -5, 5, 10), linetype="dashed", 
             color = "grey50", size=.2, alpha = .5)+
  geom_hline(yintercept=c(0), linetype="dashed", 
             color = "grey50", size=.4)+
  facet_grid(fct_relevel(N,'500','1000','2000', '5000') ~ 
               fct_relevel(J, 'J = 7', 'J = 15') +
               fct_relevel(K, 'K = 4', 'K = 8', 'K = 12')) +
  labs(x = "",
       y = expression( hat(K) - K ),
       title = 'Distribution of the outcome variable per condition')+
  theme_bw() +   
  theme1 
plotby


theme1 <- theme(plot.background = element_rect(fill = "white", color = NA), #background color
                  text = element_text(family = "mono", color = "black"), # color of all text in the plot 
                  plot.title = element_text(hjust = 0.5, color = "black", size = 11), # specs of the title
                  strip.text = element_text(colour = "black", size = 14), # specs of the text inside plot
                  panel.grid.major.x = element_line(size = .5), # change the grid layout
                  panel.grid.major.y = element_line(size = .5), # change the grid layout
                  panel.grid.minor.x = element_blank(), # remove the grid layout
                  panel.grid.minor.y = element_blank(), # remove the grid layout
                  axis.text=element_text(size=11, color = "black"), # specs of the text in axis
                  axis.text.x = element_blank(),
                  legend.position = "bottom", # legend down
                  #legend.title = element_blank(), # remove legend title,
                  legend.text = element_text(colour = "black", size = 9)
  )
  

## ------------------
## 2nd STEP p Vs phat
## ------------------

library(philentropy)

out2.td <- out %>% unnest_wider(est.Model) %>% 
  left_join(true_mods , by = 'TrueMod') %>%
  unnest(table.stat, names_repair = 'unique') %>% 
  dplyr::select(TrueMod, Rep, N.y, J, Class.x, classes, param, dens) %>%
  unnest_wider(param, names_repair = 'unique') %>% 
  dplyr::select(-Pw, -crP, -posP, -pA, R) %>% 
  rename('K' = 'Class.x', 'N' = 'N.y', 'est.dens' = 'dens...12',
         'true.dens' ='dens...13', est.K = 'classes') %>% 
  ## NEW SOLVE!!
  rowwise() %>% 
  mutate(est.dens = list(est.dens[[est.K]]),
         R = list(R[[est.K]])
  )
## Add density of sum scores (dens.ss)
out2.td <- out2.td %>%
  filter(TrueMod == 'True.1', N == 500) %>% 
  rowwise() %>% 
  mutate(dens.ss = list(start.level(density = est.dens, R = R-1)$px.plus))




est2.tdMED <- est2.tdMED %>%
  add_column(KL.ds.p = map2(.x = .$est.dens,
                            .y = .$true.dens,
                            ~kullback_leibler_distance(.y, # P
                                                       .x, #Q
                                                       testNA = FALSE, unit ="log"))) 

