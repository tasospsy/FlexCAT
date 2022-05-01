## FlexCAT project
## (NEW) VISUALIZATION
## Tasos Psychogyiopoulos
## c.03/03/2022/ m.1/5/2022

## Set working directory to work either from teh VM-in AWS or 
## from the personal computer.
## For VM
setwd("/home/rstudio")
## For Mac
setwd("/Users/tasospsy/Google Drive/_UvA/Master Thesis/")

## Load required code
source("FlexCAT/0.Functions.R")

## load data
load("out-td.Rdata")
load("out2.td.Rdata")
load("KL2.Rdata")

## Specify a general theme for the plots
theme1 <- theme(plot.background = element_rect(fill = "white", color = NA), #background color
                text = element_text(family = "mono", color = "black"), # color of all text in the plot 
                plot.title = element_text(hjust = 0.5, color = "black", size = 12), # specs of the title
                strip.text = element_text(colour = "black", size = 14), # specs of the text inside plot
                panel.grid.major.x = element_line(size = .5), # change the grid layout
                panel.grid.major.y = element_line(size = .5), # change the grid layout
                panel.grid.minor.x = element_blank(), # remove the grid layout
                panel.grid.minor.y = element_blank(), # remove the grid layout
                axis.text=element_text(size=14, color = "black"), # specs of the text in axis
                axis.text.x = element_blank(),
                legend.position = "bottom", # legend down
                #legend.title = element_blank(), # remove legend title,
                legend.text = element_text(colour = "black", size = 9)
)

## ====================================
## plotting the outcome Khat-K
## It show when a model is underfitted, 
## overfitted or accurate fitted
## ====================================

out.td %>%
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
  theme_bw() + theme1 

##=========================
## Adding and plotting KL-d
##=========================

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
KL2 %>% 
  mutate(J = case_when(J== 7 ~ 'J = 7',
                       J== 15 ~ 'J = 15'),
         K = case_when(K == 4 ~  'K = 4',
                       K == 8 ~  'K = 8',
                       K == 12 ~ 'K = 12')) %>%
  mutate(t.K = as.integer(stringr::str_extract(K, "\\d+"))) %>% 
  ggplot() +
  geom_point(aes(x = est.K, y = KL.P), size = .8, color = 'lightblue') +
  geom_smooth(aes(x = est.K, y = KL.P), method = 'loess', 
              color = 'darkblue', size = 1) +
  geom_vline(aes(xintercept= t.K), linetype="dotted", 
             color = "black", size=.4) +
  
  geom_point(aes(x = est.K, y = KL.Pp), size = .8, color = 'pink') +
  geom_smooth(aes(x = est.K, y = KL.Pp), method = 'loess', 
              color = 'darkred', size = 1) +
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
  geom_vline(data = perc.ICs.K.LONG,
             mapping = aes(xintercept= est.K, color = IC), size=.5, alpha = .8, linetype = 'solid') +
  
  ## 
  facet_grid(fct_relevel(N,'500','1000','2000', '5000') ~ 
               fct_relevel(J, 'J = 7', 'J = 15') +
               fct_relevel(K, 'K = 4', 'K = 8', 'K = 12'),
             scales = 'free_x') + 
  labs(title = "",
       x = "Number of estimated classes",
       y = "Kullback - Leibler distance") +
  theme_bw() + theme1 +
  theme(axis.text.x = element_text(size=11, color = "black"), 
        #legend.position = 'none'
  )
