## Compare total density


# N[2] = 5000
Tasos[[2]]

summin <- Tasos[[2]]$table.stat %>% 
  dplyr::select(aic, bic, aic3, aicc, caic, classes, N) %>%
  gather(key = "Index", value = "value", -classes, -N) %>% 
  group_by(Index) %>%
  summarize(minvalue = min(value), 
            whichclass = classes[which(value == min(value))])

# Score Densities 
bestAIC  <- Tasos[[2]]$param$dens[[16]]
bestAIC3 <- Tasos[[2]]$param$dens[[16]]
bestAICc <- Tasos[[2]]$param$dens[[16]]
bestBIC  <- Tasos[[2]]$param$dens[[9]]
bestcAIC <- Tasos[[2]]$param$dens[[11]]
True     <- Tasos[[2]]$param$dens[[15]]

# total score densities
R <- Tasos[[2]]$param$R[[1]] # same for all 5 
totaldAIC  <- cbind(start.level(R, bestAIC)$px.plus, rep("AIC", 17))
totaldAIC3 <- cbind(start.level(R, bestAIC3)$px.plus, rep("AIC3", 17))
totaldAICc <- cbind(start.level(R, bestAICc)$px.plus, rep("AICc", 17))
totaldBIC  <- cbind(start.level(R, bestBIC)$px.plus, rep("BIC", 17))
totaldcAIC <- cbind(start.level(R, bestcAIC)$px.plus, rep("cAIC", 17))
totalTrue  <- cbind(start.level(R, True)$px.plus, rep("True", 17))

alltotal <- rbind(totaldAIC,
                  totaldAIC3,
                  totaldAICc,
                  totaldBIC,
                  totaldcAIC,
                  totalTrue) %>% as.data.frame() 
## Tidy results
testtbl <- tibble(
  Index = c("AIC", "AIC3", "AICc", "BIC", "cAIC", "True"),
  N = 5000,
  Dataset = 1,
  density = list(bestAIC, bestAIC3, bestAICc, bestBIC, bestcAIC, True),
  total.density = list(totaldAIC, totaldAIC3, totaldAICc,totaldBIC, totaldcAIC, totalTrue )
)

test <- testtbl %>% 
  filter(Index == 'AIC', N == 5000) %>% 
  dplyr::select(total.density)





all.total.plot <- alltotal %>%
    group_by(V2) %>% 
    ggplot(aes(x=V1, group=V2, fill=V2, color = V2)) +
    geom_density(adjust=1.5, alpha=.2) +
    theme_minimal()
all.total.plot

ks.test(as.numeric(totaldAIC[,1]), as.numeric(totaldBIC[,1]))
