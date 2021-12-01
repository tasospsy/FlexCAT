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
load("tblmodels10.Rdat")
# Adding column with table summary per rep
step1 <- tblmodels10 %>% 
  add_column(SummaryTable = map(tblmodels10$Model,~.x$table.stat))
#  step1   5.1    Gb

## Adding column with list of density per replication
step2 <- step1 %>% 
  add_column(Density = imap(step1$Model, ~.x$param$dens))
# step2   5.6    Gb

## Adding column with density per Model
step3 <- unnest_longer(step2, col = Density)
# step3 127.3    Gb !

## Adding Model's number of classes (manually, but it's fine)
step4 <- step3 %>% 
  add_column(nClasses = rep(1:25, nrow(step3)/25), .before = 6) %>% 
  arrange(N) ## sort by 'N'. Note: crucial! because the seq. was by Rep,
## but the 'unnest()' (see, below), works by N. 
## So we need to alter the table sequence. 
# step4 127.3    Gb
rm(step1, step2, step3)
## Adding column with total density (slow)
startt <- Sys.time()
step5 <- step4 %>% 
  add_column(Total.D = imap(step4$Density,
                            ~start.level(R = step4$Model[[1]]$param$R[[1]], # same for all
                                         .)$px.plus)) 
(endt <- Sys.time() - startt)
## Time difference of 14.89169 mins
## Time difference of 30.39494 mins

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
rm(finalA)

finalS <- finalB %>% 
  dplyr::select(Rep, N, Density, Total.D, classes, aic, bic, aic3, aicc)

save(file = "finalS.Rdat", finalS)

rm(finalB, tblmodels10)

setwd("/Users/tasospsy/Google Drive/_UvA/Master Thesis/")
load("finalS.Rdat")

## ---
## Make a tibble for the TRUE model
load("TrueModel.Rdat")
Truetbl <- tibble(N = c(2500,5000,7500,10000),
                  classes = TrueModel$Trueclass,
                  Density = list(TrueModel$Truedens),
                  Rep = 1)
Truetbl <- Truetbl %>% 
  add_column(Total.D = imap(Truetbl$Density, ~start.level(R = TrueModel$TrueR,.)$px.plus))
  
## - - - - -  -  - - - -
mem <- data.frame('object' = ls()) %>% 
  dplyr::mutate(size_unit = object %>%sapply(. %>% get() %>% object.size %>% format(., unit = 'auto')),
                size = as.numeric(sapply(strsplit(size_unit, split = ' '), FUN = function(x) x[1])),
                unit = factor(sapply(strsplit(size_unit, split = ' '), FUN = function(x) x[2]), levels = c('Gb', 'Mb', 'Kb', 'bytes'))) %>% 
  dplyr::arrange(unit, dplyr::desc(size)) %>% 
  dplyr::select(-size_unit)
mem

## -----------------------------------------------------------------------
## RQ. Does choice of fit measure affect the accuracy and bias of density?


## ---
## AIC
## ---
byAIC <- finalS %>% 
  dplyr::select(Rep,N,Density,Total.D,classes,aic) %>% 
  group_by(Rep, N) %>% 
  slice(which.min(aic)) %>% 
  dplyr::select(-aic) %>% 
  add_column(bestby = 'AIC')

## ---
## BIC
## ---
byBIC <- finalS %>% 
  dplyr::select(Rep,N,Density,Total.D,classes,bic) %>% 
  group_by(Rep, N) %>% 
  slice(which.min(bic)) %>% 
  dplyr::select(-bic)  %>% 
  add_column(bestby = 'BIC')

## ---
## AIC3
## ---
byAIC3 <- finalS %>% 
  dplyr::select(Rep,N,Density,Total.D,classes,aic3) %>% 
  group_by(Rep, N) %>% 
  slice(which.min(aic3)) %>% 
  dplyr::select(-aic3) %>% 
  add_column(bestby = 'AIC3')

## ---
## AICc
## ---
byAICc <- finalS %>% 
  dplyr::select(Rep,N,Density,Total.D,classes,aicc) %>% 
  group_by(Rep, N) %>% 
  slice(which.min(aicc)) %>% 
  dplyr::select(-aicc) %>% 
  add_column(bestby = 'AICc')

## -- ALL COMBINED 
all_by <- bind_rows(byAIC, byAIC3, byAICc, byBIC)

## FIRST PLOTTING
## plot Fun
densplotFun <- function(dat) {
  plot <- dat %>% 
    unnest_longer(Total.D) %>% 
    ggplot() +
    geom_density(aes(x=Total.D, group = Rep, color = as.factor(classes)), alpha=.4)+ #, adjust = 0.5
    facet_wrap(~ bestby + N) +
    
    ## Add the true model
    geom_density(data = Truetbl %>% 
                   unnest_longer(Total.D), 
                 aes( x=Total.D, fill = 'True Model\n 15 classes'),size = .1, alpha = .2)+
    theme_minimal()
}

allplotp.plus <- densplotFun(all_by)
allplotdens
allplotp.plus
## THEN testing the difference
## Kolmogorov-Smirnov Test
testall <- all_by %>% 
  add_column(KS = unlist(imap(all_by$Total.D, 
                              ~ks.test(.x, Truetbl$Total.D[[1]])$statistic))) %>% 
  add_column(chisq = unlist(imap(all_by$Total.D, 
                              ~chisq.test(.x, Truetbl$Total.D[[1]])$statistic))) %>% 
  add_column(p.value = unlist(imap(all_by$Total.D, 
                                 ~chisq.test(.x, Truetbl$Total.D[[1]])$p.value)))
  
print(testall, n = 126)

ksplotFun <- function(dat) {
  plot <- dat %>% 
    group_by(N, Rep) %>% 
    ggplot() +
    geom_density(aes(x=ks.test.D, color = bestby), alpha=.4, show.legend = FALSE)+ #, adjust = 0.5
    facet_wrap(~ bestby + N) +
    theme_minimal()
}
ksplot <- ksplotFun(testall)
ksplot


