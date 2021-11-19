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