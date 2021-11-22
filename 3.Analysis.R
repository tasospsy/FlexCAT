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

## Adding column with list of densities per replication
step2 <- step1 %>% 
  add_column(Density = imap(step1$Model, ~.x$param$dens))


## Adding column with density per Model
step3 <- unnest_longer(step2, col = Density)

## Adding Model's number of classes
tblclass <- letssee %>% 
  add_column(nClasses = rep(1:25, 20), .before = 6) # Idont like it
## Adding column with total denisty
#plan(multisession(workers = availableCores()-1))
final <- tblclass %>% 
  add_column(Total.D = imap(tblclass$Density,
                            ~start.level(R = tblclass$Model[[1]]$param$R[[1]], # same for all
                                         .)$px.plus)) %>% 
  dplyr::select(-Model, -Dataset)
final

#save(file = "final.Rdat", final)

### tests 
test <- final %>% 
  group_by(N) %>% 
  dplyr::select(-Summary, -Density)



# Old output 

## Function to extract density from the nested lists output of
## our simulations, by which model minimizes the ICs. 
byIndexFUN <- function(arg1,
                     arg2){
  best <- Tasos[[arg1]]$param$dens[[which.min(Tasos[[arg1]]$table.stat[,arg2])]]
  return(best)
}

## Constructing the table with best model params by Information Criterion
ICanalysis <- expand_grid(N.size =  c(1, 2, 3, 4),
                      Index = c('aic', 'bic', 'aic3', 'aicc', 'caic')) %>% 
  add_column(dens = map2(ICanalysis$N.size, ICanalysis$Index, byIndexFUN))
## add total density
ICanalysis <- ICanalysis %>% 
  add_column(total.dens = imap(ICanalysis$dens, 
                               ~start.level(R = Tasos[[1]]$param$R[[1]], # just to be sure
                                           .)$px.plus))
## Add a row with params of the true model
ICanalysis <- ICanalysis %>% 
  add_row(N.size = c(1,2,3,4), Index = "TrueModel", dens = list(Truedens), 
          total.dens = list(start.level(R = TrueR, Truedens)$px.plus), .before = 1)
#

## LETS PLOT

wide <- unnest_wider(ICanalysis, total.dens)
long <- unnest_longer(ICanalysis, total.dens)

library(ggridges)
plot1 <- unnest_longer(ICanalysis, total.dens) %>% 
  dplyr::select(-dens) %>% 
  group_by(Index, N.size) %>% 
  ggplot(aes(x=total.dens, y = Index, fill=Index, color = Index)) +
  geom_density_ridges(alpha=.3,show.legend = FALSE)+
  facet_wrap(~N.size) +
  theme_minimal()
plot1

plot2<- unnest_longer(ICanalysis, total.dens) %>% 
  dplyr::select(-dens) %>% 
  group_by(Index, N.size) %>% 
  ggplot(aes(x=total.dens, fill=Index, color = Index)) +
  stat_ecdf(geom = "step") + 
  facet_wrap(~N.size) +
  theme_minimal()
plot2




 



