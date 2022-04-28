library(gsubfn)
library(lme4)

get_plot_limits <- function(plot) {
  gb = ggplot_build(plot)
  xmin = gb$layout$panel_params[[1]]$x.range[1]
  xmax = gb$layout$panel_params[[1]]$x.range[2]
  ymin = gb$layout$panel_params[[1]]$y.range[1]
  ymax = gb$layout$panel_params[[1]]$y.range[2]
  list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}

equal_y_limits <- function(...) {
  plots <- list(...)
  ymin = c()
  ymax = c()
  for (plot in plots) {
    lims = get_plot_limits(plot)
    ymin = append(ymin, lims$ymin)
    ymax = append(ymax, lims$ymax)
  }
  ymin = min(ymin)
  ymax = max(ymax)
  c(ymin, ymax)
} 


set_equal_y_lims <- function(...) {
  ylim = equal_y_limits(...)
  plots = list(...)
  for (i in 1:length(plots)) {
    plot = plots[[i]]
    plot <- plot + ylim(ylim)
    plots[[i]] = plot
  }
  return(plots)
}


sqrt_sum_sq <- function(...) {
  nums = list(...)
  r = 0
  for (num in nums) {
    r = r + num ^ 2
  }
  sqrt(r)
}

conf.interval.factor = 1.96 # multiply std. error by this value to get the one way amplitude of the 95% conf interval

predictBounded <- function(mod, newdata, nsim=200) {
  newdata$Prediction = predict(mod, newdata=newdata, re.form = ~0)
  
  predFun <- function(x) predict(x,newdata=newdata,re.form=NA)
  bb <- bootMer(mod,
                FUN=predFun,
                nsim=200)  
  bb_ci <- as.data.frame(t(apply(bb$t,2,quantile,c(0.025,0.975))))
  names(bb_ci) <- c("Lower","Upper")
  newdata <- cbind(newdata, bb_ci)
  return(newdata)
}

statBounded <- function(mod, predict.function, nsim=200) {
  bb <- bootMer(mod,
                FUN=predict.function,
                nsim=200)  
  bb_ci <- as.data.frame(t(apply(bb$t,2,quantile,c(0.025,0.975))))
  names(bb_ci) <- c("Lower","Upper")
  newdata <- cbind(Fit = predict.function(mod), bb_ci)
  return(newdata)
}

# calculate_bounded <- function(values, factors) {
#   result = 0
#   for (i in 1:length(values)) {
#     result = values[[i]][[1]]*factors[[i]]
#   }
# }