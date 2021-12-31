
plot.fun <- function(d, show.true.in = NULL){
  plotICs <- d %>% 
    filter(resid.df >=0) %>% # if we have neg df 
    dplyr::select(aic, bic, aic3, aicc, caic, classes, N) %>% 
    filter(classes > 1) %>% 
    gather(key = "Index", value = "value", -classes, -N) %>% 
    ggplot() + 
    geom_line(aes(x = classes, y = value, color = Index), 
              alpha = .8, show.legend = TRUE, size = .8) +
    # I create a subset of 'd' including the min values of indices
    # to plot them as points over the lines
    geom_point(data = d.min <- d %>% 
                 filter(resid.df >=0) %>% 
                 dplyr::select(aic, bic, aic3, aicc, caic, classes, N) %>%
                 gather(key = "Index", value = "value", -classes, -N) %>% 
                 group_by(Index) %>%
                 summarize(minvalue = min(value), 
                           whichclass = classes[which(value == min(value))]),
               aes(x = whichclass, y = minvalue, 
                   color = Index, shape = Index), size = 2) +
    ## chunk for the sample size text label
    geom_label(data = d, aes(label = paste0("N = ",N)), 
               x=Inf, y=Inf, hjust=1, vjust=1, size=5, family = "mono") +
    ## Chuck for the vertical line of true model (default = NULL)
    geom_vline(xintercept = show.true.in, linetype="dotted", 
               color = "red", size=0.5) +
    labs(title = "",
         subtitle = "",
         caption = "") +
    scale_x_continuous(limits = c(0, 25), breaks = 0:25) + 
    
    theme_minimal()
  # facet_wrap(~Index, scales = 'free', nrow = 2)
  return(plotICs)
}
plot.fun(out.fix02$table.stat)

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
    scale_x_continuous(trans="log10")+
    theme_minimal()
}

allplotp. <- p.plotFun(all_by)
allplotp.