## FlexCAT project
## (3) Analsysis
## Tasos Psychogyiopoulos
## 20.11.2021

## Plot the results (only 1 rep? or few?)
plotall <- map(tableall, plot.fun, show.true.in = 15)
plotall

all.plot <- plotall[[1]]/
  plotall[[2]]/
  plotall[[3]]/
  plotall[[4]]

## Main analysis after big simulations
setwd("/Users/tasospsy/Google Drive/_UvA/Master Thesis/")
load("tblmodels.Rdat")
# Adding column with table summary per rep
step1 <- tblmodels %>% 
  add_column(SummaryTable = map(tblmodels$Model,~.x$table.stat))

## Adding column with list of density per replication
step2 <- step1 %>% 
  add_column(Density = imap(step1$Model, ~.x$param$dens))
## Adding column with density per Model
step3 <- unnest_longer(step2, col = Density)

## Adding Model's number of classes (manually, but it's fine)
step4 <- step3 %>% 
  add_column(nClasses = rep(1:25, 20), .before = 6) %>% 
  arrange(N) ## sort by 'N'. Note: crucial! because the seq. was by Rep,
## but the 'unnest()' (see, below), works by N. 
## So we need to alter the table sequence. 

## Adding column with total density (slow)
startt <- Sys.time()
step5 <- step4 %>% 
  add_column(Total.D = imap(step4$Density,
                            ~start.level(R = step4$Model[[1]]$param$R[[1]], # same for all
                                         .)$px.plus)) 
(endt <- Sys.time() - startt)
## Time difference of 14.89169 mins


## Adding 'stats' column which includes the statistics
## of the corresponding model
finalA <- step5 %>%  
  add_column(stats = map2(.x = step5$nClasses,
                          .y = seq_along(step5$SummaryTable),
                          ~step5$SummaryTable[[.y]][.x,]))



## rename 'N' to avoid conflict.
## Note: I keep both 'N' columns and 'classes' columns to check for mistakes
## We can deselect them later
## Also, I unnest the 'stats' column. 
## Eventually, we have a wide-format table with all the info. 
finalB <- finalA %>% 
  rename('Ndat' = N) %>% 
  dplyr::select(-SummaryTable) %>% # We do not need the table anymore. 
  unnest(stats)
finalB

finalS <- finalB %>% 
  dplyr::select(Rep, N, Total.D, classes, aic, bic, aic3, aicc, caic, `N/Npar`, Entropy)
#save(file = "finalS.Rdat", finalS)
setwd("/Users/tasospsy/Google Drive/_UvA/Master Thesis/")
load("finalS.Rdat")

## Make a tibble for the TRUE model


## RQ. Does choice of fit measure affect the accuracy and bias of density?
byAIC <- finalS %>% 
  dplyr::select(Rep,N,Total.D,classes,aic) %>% 
  group_by(Rep, N) %>% 
  slice(which.min(aic)) %>% 
  dplyr::select(-aic) %>% 
  unnest_longer(Total.D)

plotbyAIC <- byAIC %>% 
  ggplot(aes(x=Total.D, group = Rep, color = factor(classes))) +
  geom_density(alpha=.4)+ #, adjust = 0.5
  facet_wrap(~N, ncol = 4) +
  labs(title = "Total score density of best fitting models according to AIC.",
       subtitle = "",
       caption = "") +
  theme_minimal()
plotbyAIC
  


gather(key = "IC", value = "value", -Rep, -N, -Total.D, -classes) %>% 
  
  



print(Fitchoice, n = 127)
print(finalS, n = 127)









 



