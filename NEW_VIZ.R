## FlexCAT project
## (NEW) VISUALIZATION
## Tasos Psychogyiopoulos
## c.03/03/2022/ m.2/5/2022

## Set working directory to work either from teh VM-in AWS or 
## from the personal computer.
## For VM
setwd("/home/rstudio")
## For Mac
setwd("/Users/tasospsy/Google Drive/_UvA/Master Thesis/")

## Load required code and packages
source("FlexCAT/0.Functions.R")
#library(plotly)

colors <- RColorBrewer::brewer.pal(9, 'Paired')[c(3,4,7,8)]
## load data
load("data/out-td.Rdata")
load("data/out2.td.Rdata")
load("data/KL2.Rdata")

## Specify a general theme for the plots
theme1 <- theme(plot.background = element_rect(fill = "white", color = NA), #background color
                text = element_text(family = "Courier New", color = "black"), # color of all text in the plot 
                #plot.title = element_text(hjust = 0.5, color = "black", size = 12), # specs of the title
                strip.text = element_text(colour = "black", size = 14), # specs of the text inside plot
                panel.grid.major.x = element_line(size = .5), # change the grid layout
                panel.grid.major.y = element_line(size = .5), # change the grid layout
                panel.grid.minor.x = element_blank(), # remove the grid layout
                panel.grid.minor.y = element_blank(), # remove the grid layout
                axis.text=element_text(size=10, color = "black"), # specs of the text in axis
                axis.text.x = element_blank(),
                legend.position = "bottom", # legend down
                #legend.title = element_blank(), # remove legend title,
                legend.text = element_text(colour = "black", size = 9),
                #plot.title = element_markdown(size = 12,hjust = 0,lineheight = 1, 
                #color = "black", family = 'mono'),
                strip.background =element_rect(fill="grey100")
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
  mutate(IC = fct_relevel(IC,'AIC', 'AIC3', 'BIC', 'aBIC')) %>% 
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
count.ICs.K <- out.td %>% 
  rowwise() %>% mutate_at(vars(AIC,AIC3,BIC,aBIC),  ~.x + K)  %>% 
  group_by(TrueMod, N, K, J) %>% 
  pivot_longer(cols = c(AIC,AIC3,BIC,aBIC), 
               names_to = 'IC', 
               values_to = 'est.K') %>% 
  mutate(IC = fct_relevel(IC,'AIC', 'AIC3', 'BIC', 'aBIC')) %>% 
  group_by(TrueMod, N, K, J,IC) %>% 
  count(est.K) %>% 
  mutate(J = case_when(J== 7 ~ 'J = 7',
                       J== 15 ~ 'J = 15'),
         K = case_when(K == 4 ~  'K = 4',
                       K == 8 ~  'K = 8',
                       K == 12 ~ 'K = 12'))

##==========================================
## Table with percentages per condition cell
##==========================================
tableICs <- count.ICs.K %>% 
  pivot_wider(names_from = est.K, values_from = n) %>% 
  relocate(c(`2`, `3`), .before = `4`) %>% 
  arrange(fct_relevel(N,'500','1000','2000', '5000')) %>% 
  group_by(TrueMod) %>% group_split()

opts <- options(knitr.kable.NA = "-")
titles <- c()
for(i in 1:length(tableICs)) {
  titles[i] <- paste('True model:',tableICs[[i]]$K[1],'&', tableICs[[i]]$J[1])
  print(knitr::kable(tableICs[[i]] %>% dplyr::select(-TrueMod, -K, -J), 
                     caption = titles[i]))
}
##=====================================
## PLOT KL smooth spline per condition 
##=====================================
KL2 %>% 
  mutate(J = case_when(J== 7 ~ 'J = 7',
                       J== 15 ~ 'J = 15'),
         K = case_when(K == 4 ~  'K = 4',
                       K == 8 ~  'K = 8',
                       K == 12 ~ 'K = 12')) %>%
  mutate(t.K = as.integer(stringr::str_extract(K, "\\d+"))) %>% 
  ggplot() +
  ## Dotted line for true K
  geom_vline(aes(xintercept= t.K), linetype="dotted", 
             color = "black", size=.4) +
  ## Π+ smooth line and points
  geom_point(aes(x = est.K, y = KL.P), 
             size = .6, color = 'lightblue',alpha = .4) +
  geom_smooth(aes(x = est.K, y = KL.P), method = 'loess', 
              color = 'darkblue', size = .8) +
  ## Π smooth line and points
  geom_point(aes(x = est.K, y = KL.Pp), 
             size = .6, color = 'pink', alpha = .5) +
  geom_smooth(aes(x = est.K, y = KL.Pp), method = 'loess', 
              color = 'darkred', size = .8) +
  ## Line and point for ICs: most likely picked model
  geom_vline(data = count.ICs.K %>%
               filter(n == max(n)),
             mapping = aes(xintercept= est.K, color = IC), 
             size=.6, alpha = .6, linetype = 'solid') +
  geom_point(data = count.ICs.K%>% 
               filter(n == max(n)),
             mapping = aes(x= est.K, y = -.05, color = IC, shape = IC), 
             size=3, alpha = .5) +
  
  scale_color_manual(values = colors)+
  facet_grid(fct_relevel(N,'500','1000','2000', '5000') ~ 
               fct_relevel(J, 'J = 7', 'J = 15') +
               fct_relevel(K, 'K = 4', 'K = 8', 'K = 12'),
             scales = 'free_x') + 
  labs(title = "<span style = 'color:darkblue;font-size:19px'> **π Vs π&#770;**</span> & 
       <span style = 'color:darkred;font-size:19px'>**π<sub>+</sub> Vs π&#770;<sub>+</sub>**</span>",
       x = "Number of estimated classes",
       y = "Kullback - Leibler distance") +
  theme_bw() + theme1 +
  theme(axis.text.x = element_text(size=11, color = "black"), 
        #legend.position = 'none'
  )


p2 <- count.ICs.K  %>% 
  mutate(t.K = as.integer(stringr::str_extract(K, "\\d+"))) %>% 
  ggplot(aes(x= est.K, y = IC, fill = IC, size = n, shape = IC)) + 
  geom_point(alpha=0.5, shape = 21, color="black") + 
  scale_fill_manual(values = colors)+
  geom_vline(aes(xintercept= t.K), linetype="dotted", 
             color = "black", size=.4)+
  facet_grid(fct_relevel(N,'500','1000','2000', '5000') ~ 
               fct_relevel(J, 'J = 7', 'J = 15') +
               fct_relevel(K, 'K = 4', 'K = 8', 'K = 12'),
             scales = 'free_x') + 
  labs(title = "",
       x = "Number of estimated classes",
       y = "Information Criterion (IC)")+
  theme_bw() + theme1 +
  theme(legend.position='none',
        axis.text.x = element_text(size=11, color = "black"))
p2  

## In progress
library(plotly)
fig2 <- ggplotly(p2, tooltip = c('est.K', 'n'))
fig2

ps <- p1 / p2
ps

tb <- tibble(
  'Sample size' = c(500, 1000, 2000, 5000), 
  c('cell 1', 'cell 2','cell 3','cell 4'),
  c('cell 5', 'cell 6','cell 7','cell 8'),
  c( 'cell 9','cell 10','cell 11', 'cell 12'),
   c('cell 13','cell 14','cell 15','cell 16' ),
   c('cell 17','cell 18','cell 19', 'cell 20'),
   c('cell 21','cell 22','cell 23', 'cell 24')
)
library(kableExtra)
kable(
  tb,
  format = "latex",
booktabs = TRUE,
escape = FALSE,
col.names = c("Sample size", "J = 7","J = 7","J = 7","J = 15","J = 15","J = 15" ),
align = c("l", "c", "c", "c"),
caption = "Test"
)
