## FlexCAT project
## (3) Analsysis
## Tasos Psychogyiopoulos
## 31.12.2021



## - - - - -  -  - - - -
data.frame('object' = ls()) %>% 
  dplyr::mutate(size_unit = object %>%sapply(. %>% get() %>% object.size %>% format(., unit = 'auto')),
                size = as.numeric(sapply(strsplit(size_unit, split = ' '), FUN = function(x) x[1])),
                unit = factor(sapply(strsplit(size_unit, split = ' '), FUN = function(x) x[2]), levels = c('Gb', 'Mb', 'Kb', 'bytes'))) %>% 
  dplyr::arrange(unit, dplyr::desc(size)) %>% 
  dplyr::select(-size_unit)

## ----------------
## ANALYZE FUNCTION 
## ----------------
analyZ <- function(tblmod) {
  # Adding column with table summary per rep
  step1 <- tblmod %>% 
    add_column(SummaryTable = map(tblmod$Model,~.x$table.stat))
  #  step1   4.9    Gb
  
  ## Adding column with list of density per replication
  step2 <- step1 %>% 
    add_column(Density = imap(step1$Model, ~.x$param$dens))
  # step2   5.3    Gb
  
  ## Adding column with density per Model
  step3 <- unnest_longer(step2, col = Density)
  # step3 121.9    Gb 
  
  ## Adding Model's number of classes (manually, but it's fine)
  step4 <- step3 %>% 
    add_column(nClasses = rep(1:25, nrow(step3)/25), .before = 6) %>% 
    arrange(N) ## sort by 'N'. Note: crucial! because the seq. was by Rep,
  ## but the 'unnest()' (see, below), works by N. 
  ## So we need to alter the table sequence. 
  # step4 121.9    Gb
  
  ## Adding column with total density (slow)
  step5 <- step4 %>% 
    add_column(Total.D = imap(step4$Density,
                              ~start.level(R = step4$Model[[1]]$param$R[[1]], # same for all
                                           .)$px.plus)) 

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
  
  finalS <- finalB %>% 
    dplyr::select(Rep, N, Density, Total.D, classes, aic, bic, aic3, aBIC)
  return(finalS)
}

## Main analysis after big simulations
setwd("/Users/tasospsy/Google Drive/_UvA/Master Thesis/")

## analyZ CHUNK 1-10
load("tblmodels10b.Rdat")
load("finalS.Rdat")
## analyZ CHUNK 11-30
load("tblmodels111_30.Rdat")
load("finalS_11_30.Rdat")
## analyZ CHUNK 31-40
load("tblmod.31_40.Rdat")
startt <- Sys.time()
fS_sq_31_40 <- analyZ(tblmod.31_40)
(endt <- Sys.time() - startt)
save(file = 'fS_sq_31_40.Rdat', fS_sq_31_40)




## OLD CODE : 20.11.2021

## OLD Plot the results (only 1 rep? or few?)
#save(file = "Taslist.Rdat", Taslist)
tableall <- map(Tasos, ~.x$table.stat) 
plotall <- map(tableall, plot.fun, show.true.in = 15)
plotall

all.plot <- plotall[[1]]/
  plotall[[2]]/
  plotall[[3]]/
  plotall[[4]]


## ---
## Make a tibble for the TRUE model
load("TrueModel.Rdat")
Truetbl <- tibble(N = c(2500,5000,7500,10000),
                  classes = TrueModel$Trueclass,
                  Density = list(TrueModel$Truedens),
                  Rep = 1)
Truetbl <- Truetbl %>% 
  add_column(Total.D = imap(Truetbl$Density, ~start.level(R = TrueModel$TrueR,.)$px.plus))
  




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
## aBIC
## ---
byaBIC <- finalS %>% 
  dplyr::select(Rep,N,Density,Total.D,classes,aBIC) %>% 
  group_by(Rep, N) %>% 
  slice(which.min(aBIC)) %>% 
  dplyr::select(-aBIC) %>% 
  add_column(bestby = 'aBIC')

## -- ALL COMBINED 
all_by <- bind_rows(byAIC, byAIC3, byaBIC, byBIC)

save(file = "all_by.Rdat", all_by)

## Distance difference in distributions
library(philentropy)
load("all_by.Rdat")

testall <- all_by %>% 
  
  # KL distance for total-score density
  add_column(KL.ds.p.plus = unlist(imap(all_by$Total.D, 
                          ~kullback_leibler_distance(Truetbl$Total.D[[1]], # P
                                                     .x, #Q
                                                     testNA = FALSE, unit ="log")))) %>% 
  # KL distance for score density
  add_column(KL.ds.p = unlist(imap(all_by$Density, 
                       ~kullback_leibler_distance(Truetbl$Density[[1]], # P
                                                  .x, #Q
                                                  testNA = FALSE, unit ="log")))) %>% 
  # Each prob. in score density Vs true prob. score density 
  add_column(dens_diff = map(all_by$Density, ~ abs(.x - Truetbl$Density[[1]]))) %>% 
  # Mean Square Error of score density & true score density
  add_column(MSE.d = map(all_by$Density, ~ mean((Truetbl$Density[[1]]-.x)^2)))

print(testall, n = 126)

## Summary Statistics per condition fo KLd
testall.t <-  testall %>% 
  group_by(bestby, N) %>% 
  summarize(median_KL.p = median(KL.ds.p), mean_KL.p = mean(KL.ds.p),
            median_KL.p.p = median(KL.ds.p.plus), mean_KL.p.p = mean(KL.ds.p.plus),
            .groups = 'drop') %>% 
  mutate(labelspp= paste("KLmedian = ", round(median_KL.p.p,4), 
                       "\nKLmean =", round(mean_KL.p.p,4))
  ) %>% 
  group_split(N)

## CUMULATIVE DIST. PLOTS FOR
## a. Score density
cdpFun <- function(dat) {
  plot <- dat %>% 
    unnest_longer(Density) %>% 
    ggplot() +
    stat_ecdf(aes(x=Density, group = Rep, color = as.factor(classes)),
              alpha = .5, geom = "step")+
    facet_wrap(~ bestby + N) +
     ## Add the true model
    stat_ecdf(data = Truetbl %>% 
              unnest_longer(Density), 
              aes(x=Density),
              geom = "step", alpha = .7)+
    scale_x_log10()+
    theme_minimal()
}
cdp <- cdpFun(all_by)
cdp

## b. total score density
cdppFun <- function(dat) {
  plot <- dat %>% 
    unnest_longer(Total.D) %>% 
    ggplot() +
    stat_ecdf(aes(x=Total.D, group = Rep, color = as.factor(classes)),
              alpha = .5, geom = "step")+
    facet_wrap(~ bestby + N) +
    ## Add the true model
    stat_ecdf(data = Truetbl %>% 
                unnest_longer(Total.D), 
              aes(x=Total.D),
              geom = "step", alpha = .7)+
    ## Add text labels
    geom_text(data = testall.t, aes(label = labelspp), 
              x=Inf, y=.3, hjust=1, vjust=1, size=3) +
    theme_minimal()
}
cdpp <- cdppFun(all_by)
cdpp

dif.cdp
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
## p.+ plot Fun
p.plusplotFun <- function(dat) {
  plot <- dat %>% 
    unnest_longer(Total.D) %>% 
    ggplot() +
    geom_density(aes(x=Total.D, group = Rep, color = as.factor(classes)), alpha=.1)+ #, adjust = 0.5
    facet_wrap(~ bestby + N) +
    
    ## Add the true model
    geom_density(data = Truetbl %>% 
                   unnest_longer(Total.D), 
                 aes( x=Total.D, fill = 'True Model\n 15 classes'),size = .1, alpha = .2)+
    theme_minimal()
}

allplotp.plus <- p.plusplotFun(all_by)
allplotp.plus

## p. plot Fun
p.plotFun <- function(dat) {
  plot <- dat %>% 
    unnest_longer(Density) %>% 
    ggplot() +
    geom_density(aes(x=Density, group = Rep, color = as.factor(classes)), alpha=.1)+ #, adjust = 0.5
    facet_wrap(~ bestby + N) +
    
    ## Add the true model
    geom_density(data = Truetbl %>% 
                   unnest_longer(Density), 
                 aes( x=Density, fill = 'True Model\n 15 classes'),size = .1, alpha = .2)+
    scale_x_log10()+
    theme_minimal()
}

allplotp. <- p.plotFun(all_by)
allplotp.

## CUMULATIVE PLOTS
CplotFun <- function(dat) {
  plot <- dat %>% 
    unnest_longer(Total.D) %>% 
    ggplot() +
    ecdf(aes(x=Total.D, group = Rep, color = as.factor(classes)))+ #, adjust = 0.5
    facet_wrap(~ bestby + N) +
    
    ## Add the true model
    ecdf(data = Truetbl %>% 
                   unnest_longer(Total.D), 
                 aes( x=Total.D, fill = 'True Model\n 15 classes'),size = .1)+
    theme_minimal()
}
Cplus <- CplotFun(all_by)
## THEN testing the difference
test <- KL.dist(all_by$Total.D[[1]], Truetbl$Total.D[[1]])
## Distance
library(philentropy)
testall <- all_by %>% 
  add_column(KL.ds.p.plus = unlist(imap(all_by$Total.D, 
                          ~kullback_leibler_distance(.x, Truetbl$Total.D[[1]], 
                                                     testNA = FALSE, unit ="log2")))) %>% 
  add_column(KL.ds.p = unlist(imap(all_by$Density, 
                                        ~kullback_leibler_distance(.x, Truetbl$Density[[1]], 
                                                                   testNA = FALSE, unit ="log2"))))

print(testall, n = 126)

dist.plot <- function(dat) {
  plot <- dat %>% 
    group_by(N, Rep) %>% 
    ggplot() +
    geom_jitter(aes(x=KL.ds.p.plus, y =KL.ds.p,  color = as.factor(N)),
               stat = "identity", alpha=.4, show.legend = FALSE)+ #, adjust = 0.5
    facet_wrap(~ bestby + N) +
    theme_minimal()
}
plot1 <- dist.plot(testall)
plot1


## OLD Plot the results (only 1 rep? or few?)
#save(file = "Taslist.Rdat", Taslist)
#tableall <- map(Tasos, ~.x$table.stat) 
#plotall <- map(tableall, plot.fun, show.true.in = 15)
#plotall
#
#all.plot <- plotall[[1]]/
#  plotall[[2]]/
#  plotall[[3]]/
#  plotall[[4]]
