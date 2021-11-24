## FlexCAT project
## (3) Analsysis
## Tasos Psychogyiopoulos
## 20.11.2021

## OLD Plot the results (only 1 rep? or few?)
#save(file = "Taslist.Rdat", Taslist)
tableall <- map(Tasos, ~.x$table.stat) 
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

## ---
## Make a tibble for the TRUE model
load("TrueModel.Rdat")
Truetbl <- tibble(N = c(2500,5000,7500,10000),
                  classes = TrueModel$Trueclass,
                  dens = list(TrueModel$Truedens),
                  Rep = 1)
Truetbl <- Truetbl %>% 
  add_column(Total.D = imap(Truetbl$dens, ~start.level(R = TrueModel$TrueR,.)$px.plus))
  

## -----------------------------------------------------------------------
## RQ. Does choice of fit measure affect the accuracy and bias of density?


## ---
## AIC
## ---
byAIC <- finalS %>% 
  dplyr::select(Rep,N,Total.D,classes,aic) %>% 
  group_by(Rep, N) %>% 
  slice(which.min(aic)) %>% 
  dplyr::select(-aic) %>% 
  add_column(bestby = 'AIC')

## ---
## BIC
## ---
byBIC <- finalS %>% 
  dplyr::select(Rep,N,Total.D,classes,bic) %>% 
  group_by(Rep, N) %>% 
  slice(which.min(bic)) %>% 
  dplyr::select(-bic)  %>% 
  add_column(bestby = 'BIC')

## ---
## AIC3
## ---
byAIC3 <- finalS %>% 
  dplyr::select(Rep,N,Total.D,classes,aic3) %>% 
  group_by(Rep, N) %>% 
  slice(which.min(aic3)) %>% 
  dplyr::select(-aic3) %>% 
  add_column(bestby = 'AIC3')

## ---
## AICc
## ---
byAICc <- finalS %>% 
  dplyr::select(Rep,N,Total.D,classes,aicc) %>% 
  group_by(Rep, N) %>% 
  slice(which.min(aicc)) %>% 
  dplyr::select(-aicc) %>% 
  add_column(bestby = 'AICc')

## -- ALL COMBINED 
all_by <- bind_rows(byAIC, byAIC3, byAICc, byBIC)

## plot Fun
library(RColorBrewer)
densplotFun <- function(dat) {
  plot <- dat %>% 
    unnest_longer(Total.D) %>% 
    ggplot() +
    geom_density(aes(x=Total.D, group = Rep, color = factor(classes)), alpha=.4)+ #, adjust = 0.5
    facet_wrap(~ bestby + N) +
    
    ## Add the true model
    geom_density(data = Truetbl %>% 
                   unnest_longer(Total.D), 
                 aes(x = Total.D, fill = 'True Model\n 15 classes'),size = .1, alpha = .2)+
    theme_minimal()
}

allplot <- densplotFun(all_by)
allplot
