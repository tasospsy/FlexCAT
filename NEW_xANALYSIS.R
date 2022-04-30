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
load("out2.td.Rdata")
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
  mutate(per = n / 100 * 100) %>% dplyr::select(-n) %>% 
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
## 2nd STEP p Vs pha
## ------------------

library(philentropy)
## Add density of sum scores (dens.ss) in TRUE MODELS
true_mods <- true_mods %>% 
  rowwise() %>%
  mutate(true.dens.ss = list(start.level(density = dens, R = R-1)$px.plus))

# 30/4/2022
setwd("/home/rstudio/efs")
save(true_mods,file ='true_mods.Rdata')

out2.td <- out %>% unnest_wider(est.Model) %>% 
  left_join(true_mods , by = 'TrueMod') %>%
  unnest(table.stat, names_repair = 'unique') %>% 
  dplyr::select(TrueMod, Rep, N.y, J, Class.x, classes, param, dens) %>%
  unnest_wider(param, names_repair = 'unique') %>% 
  dplyr::select(-Pw, -crP, -posP, -pA, R) %>% 
  rename('K' = 'Class.x', 'N' = 'N.y', 'est.dens' = 'dens...12',
         'true.dens' ='dens...13', est.K = 'classes') %>% 
  mutate(est.dens = map2(.x = est.dens, .y = est.K, ~ .x[[.y]]),
         R = map2(.x = R, .y = est.K, ~ .x[[.y]]))

# 30/4/2022
setwd("/home/rstudio/efs")
save(out2.td,file ='out2.td.Rdata')

testKL <- out2.td %>% 
  rowwise() %>% 
  mutate(KL.p = kullback_leibler_distance(P = est.dens, # P
                                          Q = true.dens, #Q
                                          testNA = FALSE, unit ="log", 
                                          epsilon = 0.000000001)) %>% 
  dplyr::select(-R, -true.dens, -est.dens)

#setwd("/home/rstudio/efs")
#save(testKL,file ='testKL.Rdata')

## which is the maximum percentage of estimated K that is 
## picked by each IC, within each condition of the 24?
perc.ICs.K <- out.td %>% 
  rowwise() %>% mutate_at(vars(AIC,AIC3,BIC,aBIC),  ~.x + K)  %>% 
  group_by(TrueMod, N, K, J) %>% 
  pivot_longer(cols = c(AIC,AIC3,BIC,aBIC), 
               names_to = 'IC', 
               values_to = 'est.K') %>% 
  group_by(TrueMod, N, K, J,IC) %>% 
  count(est.K) %>% 
  filter(n == max(n)) %>% 
  mutate(n = unique(n)) %>% 
  pivot_wider(names_from = IC, 
              values_from = n)

## The same in long format for plots
perc.ICs.K.LONG <- perc.ICs.K %>% 
  pivot_longer(cols = c(AIC,AIC3,BIC,aBIC), 
                                names_to = 'IC', 
                                values_to = 'percentage') %>% 
  filter(!is.na(percentage)) %>% 
  mutate(J = case_when(J== 7 ~ 'J = 7',
                       J== 15 ~ 'J = 15'),
         K = case_when(K == 4 ~  'K = 4',
                       K == 8 ~  'K = 8',
                       K == 12 ~ 'K = 12')) %>% 
  mutate(IC.y = case_when(IC == 'AIC' ~ .6,
                          IC == 'AIC3' ~ .5,
                          IC == 'BIC' ~ .4,
                          IC == 'aBIC' ~ .3))
## PLOT KL smooth spline per condition 
## ?? (WITH percentage of ICs)
testKL %>% 
  mutate(J = case_when(J== 7 ~ 'J = 7',
                       J== 15 ~ 'J = 15'),
         K = case_when(K == 4 ~  'K = 4',
                       K == 8 ~  'K = 8',
                       K == 12 ~ 'K = 12')) %>%
  mutate(t.K = as.integer(stringr::str_extract(K, "\\d+"))) %>% 
  ggplot() +
  geom_point(aes(x = est.K, y = KL.p), size = .8, color = 'lightblue') +
  geom_smooth(aes(x = est.K, y = KL.p), method = 'loess', 
              color = 'grey5', size = 1) +
  geom_vline(aes(xintercept= t.K), linetype="dotted", 
                        color = "black", size=.4)+
  
  ## ICs part
  #geom_label(data = perc.ICs.K.LONG,
  #  aes(x = est.K, y = IC.y,
  #      label = paste0(IC, ': K=',est.K,' (',percentage,'%)'), color = IC),
  #  label.size = 0,
  #  size = 3,
  #  #color = "black",
  #  #fill="lightgreen", 
  #  alpha = .9, 
  #  hjust = 0) +
  geom_vline(
    data = perc.ICs.K.LONG, 
      aes(xintercept= est.K, color = IC), size=1, alpha = .5, linetype = 'solid') +
  
  ## 
  facet_grid(fct_relevel(N,'500','1000','2000', '5000') ~ 
               fct_relevel(J, 'J = 7', 'J = 15') +
               fct_relevel(K, 'K = 4', 'K = 8', 'K = 12'),
             scales = 'free_x') + 
  labs(title = "",
       x = "Number of estimated classes",
       y = "Kullbach - Leibler distance (π - est. π)") +
  theme_bw() + theme1 +
  theme(axis.text.x = element_text(size=11, color = "black"), 
        #legend.position = 'none'
        )

## --- IN PROGRESS

## Add density of sum scores P+  (dens.ss) in estimated models
startt <- Sys.time()
test1 <- out2.td %>%
  filter(Rep <= 50) %>% 
  mutate(est.dens.ss = map2(.x = est.dens, .y = R, 
                                 ~start.level(density = .x, R = .y)$px.plus)) %>% 
  left_join(true_mods %>% dplyr::select(TrueMod, true.dens.ss) , by = 'TrueMod') 
endt <- Sys.time()
endt-startt

setwd("/home/rstudio/efs")
save(test1,file ='test1.Rdata')

load('test1.Rdata')


testKLplus <- test1 %>% 
  mutate(KL.Pp = map2(.x =est.dens.ss, 
                      .y = true.dens.ss,
                      ~kullback_leibler_distance(P = .x, # P
                                                 Q = .y, #Q
                                                 testNA = FALSE, unit ="log", 
                                                 epsilon = 0.000000001))) %>%
  mutate(KL.P = map2(.x =est.dens, 
                     .y =true.dens,
                     ~kullback_leibler_distance(P = .x, # P
                                                Q = .y, #Q
                                                testNA = FALSE, unit ="log", 
                                                epsilon = 0.000000001))) %>%        
  dplyr::select(-R, -true.dens, -est.dens, -est.dens.ss, -true.dens.ss)

save(testKLplus,file ='testKLplus.Rdata')
