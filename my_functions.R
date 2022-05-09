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
  newdata$Fit = predict(mod, newdata=newdata, re.form = ~0)
  
  predFun <- function(x) predict(x,newdata=newdata,re.form=NA)
  bb <- bootMer(mod,
                FUN=predFun,
                nsim=200)  
  bb_ci <- as.data.frame(t(apply(bb$t,2,quantile,c(0.025,0.975))))
  names(bb_ci) <- c("Lower","Upper")
  newdata <- cbind(newdata, bb_ci)
  return(newdata)
}

statBounded <- function(mod, predict.function, nsim=200, is.ratio = FALSE) {
  # if (is.ratio) {
  #   predict.function = log(predict.function)
  # }
  bb <- bootMer(mod,
                FUN=predict.function,
                nsim=200)  
  bb_ci <- as.data.frame(t(apply(bb$t,2,quantile,c(0.025,0.975))))
  names(bb_ci) <- c("Lower","Upper")
  newdata <- cbind(Fit = predict.function(mod), bb_ci)
  newdata <- append.p.value(newdata)
  return(newdata)
}


p.value <- function(fit, lwr, upr) {
  std.err = (upr - lwr)  / (2 * conf.interval.factor)
  z = fit / std.err
  p = exp(- 0.717*z - 0.416*z^2)
  return(p)
}

append.p.value <- function(df) {
  df <- cbind(df, "P-value" = p.value(df$Fit, df$Lower, df$Upper))
  return(df)
}

summarise.factors <- function(data, ...) {
  return(summarise(group_by(data, ...)))
}

summarise.predict <- function(data, model, ...) {
  newdata = summarise.factors(data, ...)
  newdata <- predictBounded(model, newdata = newdata)
  return(newdata)
}


darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}


lighten <- function(color, factor = 0.5) {
  if ((factor > 1) | (factor < 0)) stop("factor needs to be within [0,1]")
  col <- col2rgb(color)
  col <- col + (255 - col)*factor
  col <- rgb(t(col), maxColorValue=255)
  col
}
