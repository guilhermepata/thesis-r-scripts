library(gsubfn)
library(pracma)
library(lme4)

get_plot_limits <- function(plot) {
  gb = ggplot_build(plot)
  xmin = gb$layout$panel_params[[1]]$x.range[1]
  xmax = gb$layout$panel_params[[1]]$x.range[2]
  ymin = gb$layout$panel_params[[1]]$y.range[1]
  ymax = gb$layout$panel_params[[1]]$y.range[2]
  list(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax
  )
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


equal_x_limits <- function(...) {
  plots <- list(...)
  xmin = c()
  xmax = c()
  for (plot in plots) {
    lims = get_plot_limits(plot)
    xmin = append(xmin, lims$xmin)
    xmax = append(xmax, lims$xmax)
  }
  xmin = min(xmin)
  xmax = max(xmax)
  c(xmin, xmax)
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


set_equal_x_lims <- function(...) {
  xlim = equal_x_limits(...)
  plots = list(...)
  for (i in 1:length(plots)) {
    plot = plots[[i]]
    plot <- plot + xlim(xlim)
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

predictBounded <- function(mod, newdata, nsim = 200) {
  newdata$Fit = predict(mod, newdata = newdata, re.form = ~ 0)
  
  predFun <- function(x)
    predict(x, newdata = newdata, re.form = NA)
  bb <- bootMer(mod,
                FUN = predFun,
                nsim = 200)
  bb_ci <-
    as.data.frame(t(apply(bb$t, 2, quantile, c(0.025, 0.975))))
  names(bb_ci) <- c("Lower", "Upper")
  newdata <- cbind(newdata, bb_ci)
  return(newdata)
}

statBounded <-
  function(mod,
           predict.function,
           nsim = 200,
           is.ratio = FALSE) {
    # if (is.ratio) {
    #   predict.function = log(predict.function)
    # }
    bb <- bootMer(mod,
                  FUN = predict.function,
                  nsim = 200)
    bb_ci <-
      as.data.frame(t(apply(bb$t, 2, quantile, c(0.025, 0.975))))
    names(bb_ci) <- c("Lower", "Upper")
    newdata <- cbind(Fit = predict.function(mod), bb_ci)
    newdata <- append.p.value(newdata)
    return(newdata)
  }


p.value <- function(fit, lwr, upr) {
  std.err = (upr - lwr)  / (2 * conf.interval.factor)
  z = fit / std.err
  p = exp(-0.717 * z - 0.416 * z ^ 2)
  return(p)
}

append.p.value <- function(df) {
  df <- cbind(df, "P-value" = p.value(df$Fit, df$Lower, df$Upper))
  return(df)
}

summarise.factors <- function(data, ...) {
  return(summarise(group_by(data, ...)))
}

summarise.predict.old <- function(data, model, ...) {
  newdata = summarise.factors(data, ...)
  newdata <- predictBounded(model, newdata = newdata)
  return(newdata)
}

summarise.predict <- function(data, model, ...) {
  newdata = summarise.factors(data, Num, Session, Group, ...)
  auxdata = data.frame()
  
  predict.big = tryCatch({
    summary(emmeans(
      model,
      ~ Num * Session * Group,
      at = list(
        Num = unique(newdata$Num),
        Session = unique(newdata$Session),
        Group = unique(newdata$Group)
      ),
      adjust = 'none'
    ), infer = TRUE)
  },
  error = function(cond) {
    summary(emmeans(model,
                    ~ Num * Session,
                    at = list(
                      Num = unique(newdata$Num),
                      Session = unique(newdata$Session)
                      # Group = unique(newdata$Group)
                    ),
                    adjust = 'none'), infer = TRUE)
  })
  
  
  for (i in 1:nrow(newdata)) {
    row = newdata[i,]
    num = row$Num
    session = row$Session
    group = row$Group
    if ('Group' %in% names(predict)) {
      predict = filter(predict.big,
                       Num == num,
                       Session == session,
                       Group == group)[1,]
    } else {
      predict = filter(predict.big,
                       Num == num,
                       Session == session, )[1,]
    }
    
    predict.aux = data.frame(
      Fit = c(predict$emmean),
      Lower = c(predict$lower.CL),
      Upper = c(predict$upper.CL),
      P.value = c(predict$p.value)
    )
    auxdata = rbind(auxdata, predict.aux)
  }
  newdata <- cbind(newdata, auxdata)
  return(newdata)
}


summarise.predict.first.lr <- function(data, model, ...) {
  newdata = summarise.factors(data, Perc, Session, Group, ...)
  auxdata = data.frame()
  
  predict.big = tryCatch({
    summary(emmeans(
      model,
      ~ Perc * Session * Group,
      at = list(
        Perc = unique(newdata$Perc),
        Session = unique(newdata$Session),
        Group = unique(newdata$Group)
      ),
      adjust = 'none'
    ), infer = TRUE)
  },
  error = function(cond) {
    summary(emmeans(model,
                    ~ Perc * Session,
                    at = list(
                      Perc = unique(newdata$Perc),
                      Session = unique(newdata$Session)
                      # Group = unique(newdata$Group)
                    ),
                    adjust = 'none'), infer = TRUE)
  })
  
  
  for (i in 1:nrow(newdata)) {
    row = newdata[i,]
    num = row$Perc
    session = row$Session
    group = row$Group
    if ('Group' %in% names(predict)) {
      predict = filter(predict.big,
                       Perc == num,
                       Session == session,
                       Group == group)[1,]
    } else {
      predict = filter(predict.big,
                       Perc == num,
                       Session == session, )[1,]
    }
    
    predict.aux = data.frame(
      Fit = c(predict$emmean),
      Lower = c(predict$lower.CL),
      Upper = c(predict$upper.CL),
      P.value = c(predict$p.value)
    )
    auxdata = rbind(auxdata, predict.aux)
  }
  newdata <- cbind(newdata, auxdata)
  return(newdata)
}


darken <- function(color, factor = 1.4) {
  col <- col2rgb(color)
  col <- col / factor
  col <- rgb(t(col), maxColorValue = 255)
  col
}


lighten <- function(color, factor = 0.5) {
  if ((factor > 1) |
      (factor < 0))
    stop("factor needs to be within [0,1]")
  col <- col2rgb(color)
  col <- col + (255 - col) * factor
  col <- rgb(t(col), maxColorValue = 255)
  col
}


get_group_color <- function(group) {
  if (length(group) > 1) {
    return (cbind(get_group_color(group[[1]]), get_group_color(group[2:length(group)])))
  }
  group.parts = str_split(group, ":")[[1]]
  if (grepl('Exp', group.parts[[1]])) {
    name = group.parts[[1]]
    group = paste(group.parts[[2]], group.parts[[3]], sep = ':')
  }
  if (group == 'NotAtaxic:NoSwitch') {
    if (name  == 'Exp3') {
      return('slategray')
    } else {
      return('lightgoldenrod4')
    }
  } else if (group == 'NotAtaxic:Switch') {
    if (name == 'Exp3') {
      return('tan2')
    }
    else if (name == 'Exp5') {
      return('sienna1')
    } else {
      return('lightsalmon2')
    }
  } else if (group == 'Ataxic:Switch') {
    return('lightslateblue')
  }
}

height.large = 5.83 / 2
height.short = 5.83 / 2 * 2.5 / 3
height.xshort = 5.83 / 2 * 2.2 / 3


continuous.shades.frame <- function(shades.frame) {
  shades.frame = arrange(shades.frame, Trial)
  to.remove = list()
  for (i in 1:nrow(shades.frame)) {
    if (!shades.frame[i,]$Trial %in% to.remove) {
      delta = 0
      while (i + delta + 1 <= nrow(shades.frame) &&
             shades.frame[i + delta + 1,]$Trial - shades.frame[i + delta,]$Trial == 1)
      {
        to.remove = append(to.remove, shades.frame[i + delta + 1,]$Trial)
        shades.frame[i,]$xmax = shades.frame[i + delta + 1,]$xmax
        delta = delta + 1
      }
    }
  }
  shades.frame = filter(shades.frame,!Trial %in% to.remove)
}


fill.missing.data = function(data, model) {
  data$Was.missing = rep(FALSE, nrow(data))
  for (group in unique(data$Group)) {
    data.scope0 = filter(data, Group == group)
    for (session in unique(data.scope0$Session)) {
      data.scope1 = filter(data.scope0, Session == session)
      num.range = min(data.scope1$Num):max(data.scope1$Num)
      trial.range = rep(NA, length(num.range))
      for (i in 1:length(num.range)) {
        trial.range[[i]] = unique(filter(data.scope1, Num == num.range[[i]])$Trial)[[1]]
      }
      for (animal in unique(data.scope0$Animal)) {
        data.scope2 = filter(data.scope1, Animal == animal)
        for (i in 1:length(num.range)) {
          num = num.range[[i]]
          trial = trial.range[[i]]
          data.scope3 = filter(data.scope2, Num == num)
          if (nrow(data.scope3) == 0) {
            data[nrow(data) + 1, ] = NA
            data[nrow(data), ]$Group = group
            data[nrow(data), ]$Session = session
            data[nrow(data), ]$Animal = animal
            data[nrow(data), ]$Num = num
            data[nrow(data), ]$Trial = trial
            data[nrow(data), ]$Fit = predict(model, newdata = data[nrow(data), ])
            data[nrow(data), ]$Was.missing = TRUE
          }
        }
      }
    }
  }
  data = arrange(data, Experiment, Group, Animal, Trial)
  return(data)
}

predict.fit = function(data, model, fill.missing = TRUE) {
  if ("Fit" %in% colnames(data)) {
    data$Fit = predict(model, newdata = data)
  } else
  {
    data = cbind(data, Fit = predict(model, newdata = data))
  }
  if (fill.missing) {
    data = fill.missing.data(data, model)
  }
}

get_group_colors = function(groups) {
  res = c()
  for (group in groups) {
    res[group] = get_group_color(group)
  }
  return(res)
}

skipcomp.emmc <- function(levels,
                           skip = 1,
                           reverse = FALSE,
                           # adjust = TRUE,
                            ...) {
  if ((k <- length(levels)) < skip + 1)
    stop("Need at least ", skip + 1, " levels")
  coef <- data.frame()
  coef <- as.data.frame(lapply(seq_len(k - skip - 1), function(i) {
    sgn <- ifelse(reverse,-1, 1)
    sgn * c(rep(0, i - 1), 1, rep(0, skip),-1, rep(0, k - i - skip - 1))
  }))
  names(coef) <- sapply(coef, function(x)
    paste(levels[[which(x == 1)]], "-", levels[[which(x == -1)]]))
  attr(coef, "adjust") =  "fdr" # default adjustment method or none
  coef
}

skipconsecavg.emmc <- function(levels, exclude = integer(0), reverse = FALSE, ...) {

  if (!is.integer0(exclude)) {
    levels = levels[-exclude]
  }
  if ((k <- length(levels)) < 1 + 1)
    stop("Need at least ", 1, " levels")
  coef.aux <- data.frame()
  coef.aux <- as.data.frame(lapply(seq_len(k - 1), function(i) {
    sgn <- ifelse(reverse,-1, 1)
    sgn * c(rep(0, i - 1), 1,-1, rep(0, k - i - 1))
  }))
  # coef = data.frame()
  names(coef.aux) <- sapply(coef.aux, function(x)
    paste(levels[[which(x == 1)]], "-", levels[[which(x == -1)]]))
  
  select = seq(from = 2, to = length(levels)-1, by = 2)
  
  k = length(select)+1
  
  coef.aux = coef.aux[,select]
  
  coef = data.frame(Avg = rowSums(coef.aux, na.rm=TRUE) / (k-1))
  
  names(coef) <- sapply(coef, function(x)
    paste("(", paste(levels[which(x == 1/(k-1))], sep = '', collapse = ' + '), " - ", paste(levels[which(x == -1/(k-1))], sep = '', collapse = ' - '), ") / ", toString(k-1), sep = ""))
  coef = cbind(coef, coef.aux)
  attr(coef, "adjust") = "fdr"   # default adjustment method
  coef
}



revconsecavg.emmc <- function(levels, exclude = integer(0), reverse = FALSE, ...) {
  reverse = !reverse
  if (!is.integer0(exclude)) {
    levels = levels[-exclude]
  }
  if ((k <- length(levels)) < 1 + 1)
    stop("Need at least ", 1, " levels")
  coef.aux <- data.frame()
  coef.aux <- as.data.frame(lapply(seq_len(k - 1), function(i) {
    sgn <- ifelse(reverse,-1, 1)
    sgn * c(rep(0, i - 1), 1,-1, rep(0, k - i - 1))
  }))
  # coef = data.frame()
  coef = data.frame(Avg = rowSums(coef.aux, na.rm=TRUE) / (k-1))
  names(coef.aux) <- sapply(coef.aux, function(x)
    paste(levels[[which(x == 1)]], "-", levels[[which(x == -1)]]))
  names(coef) <- sapply(coef, function(x)
    paste("(", levels[[which(x == 1/(k-1))]], " - ", levels[[which(x == -1/(k-1))]], ") / ", toString(k-1), sep = ""))
  coef = cbind(coef, coef.aux)
  attr(coef, "adjust") = "fdr"   # default adjustment method
  coef
}

is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}


get_group_label = function(group) {
  if (length(group) > 1) {
    return (cbind(get_group_color(group[[1]]), get_group_color(group[2:length(group)])))
  }
  group.parts = str_split(group, ":")[[1]]
  if (grepl('Exp', group.parts[[1]])) {
    name = group.parts[[1]]
    group = paste(group.parts[[2]], group.parts[[3]], sep = ':')
  }
  if (group == 'NotAtaxic:NoSwitch') {
    if (name  == 'Exp3') {
      return('Continuous 1')
    } else {
      return('Continuous 2')
    }
  } else if (group == 'NotAtaxic:Switch') {
    if (name == 'Exp3') {
      return('Alternating 1')
    }
    else if (name == 'Exp5') {
      return('Alternating 2')
    } else {
      return('Control')
    }
  } else if (group == 'Ataxic:Switch') {
    return('Ataxic')
  }
}

get_group_labels = function(groups) {
  res = c()
  for (group in groups) {
    res[group] = get_group_label(group)
  }
  return(res)
}


rentention.values = function(data.split.summary,
                             model.split,
                             groups,
                             sessions = NULL) {
  values = data.frame()
  
  if (is.null(sessions)) {
    sessions = list()
  }
  
  for (group in groups) {
    if (is.null(sessions[group][[1]])) {
      if (!grepl('Exp4', group)) {
        if (grepl('Exp3', group) && grepl('NoSwitch', group)) {
          sessions[group] = list(c('S1', 'S2', 'S3', 'S4'))
        } else {
          sessions[group] = list(c('S1', 'S2', 'S3', 'S4', 'S5'))
        }
      } else {
        sessions[group] = list(c('S1', 'S2', 'S3'))
      }
    }
    
    for (session_ind in 2:length(sessions[group][[1]])) {
      session = sessions[group][[1]][[session_ind]]
      prev_session = sessions[group][[1]][session_ind - 1]
      prev_num = max(filter(
        data.split.summary,
        Group == group,
        Session == prev_session
      )$Num)
      curr_num = 0
      nums = c(curr_num, prev_num)
      
      values.aux = (summary(
        emmeans(
          model.split,
          consec ~ Num * Session | Group,
          at = list(
            Session = c(prev_session, session),
            Num = nums,
            Group = group
          ),
          adjust = 'none'
        ),
        infer = TRUE
      )$contrasts)[2,]
      
      session_number = substr(session,2,2)
      prev_session_number = substr(prev_session,2,2)
      
      ses_name = paste("\u0394", session_number, ',', prev_session_number, sep = '')
      
      values.aux$Session = ses_name
      
      values = rbind(values, values.aux)
      
    }
  }
  return(values)
}


library(gridExtra)

theme_black = function(base_size = 11, base_family = "") {
  
  theme_classic(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_line(color = "white"),
      axis.text = element_text(color = "white"),  
      axis.ticks = element_line(color = "white"),  
      axis.title = element_text(color = "white"),  
      # axis.title.y = element_text(color = "white", angle = 90),  
      # axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "transparent"),  
      legend.key = element_rect(color = NA,  fill = "transparent"),  
      # legend.key.size = unit(1.2, "lines"),  
      # legend.key.height = NULL,  
      # legend.key.width = NULL,      
      legend.text = element_text(color = "white"),  
      legend.title = element_text(color = "white"),  
      # legend.position = "right",  
      # legend.text.align = NULL,  
      # legend.title.align = NULL,  
      # legend.direction = "vertical",  
      # legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "transparent", color  =  NA),  
      panel.border = element_rect(fill = NA, color = NA),  
      panel.grid.major = element_line(color = NA),  
      panel.grid.minor = element_line(color = NA),  
      # panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(color = "white"),  
      strip.text.y = element_text(color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "transparent", fill = "transparent"),  
      plot.title = element_text(color = "white"),  
      # plot.spacing = unit(rep(1, 4), "lines")
      
    )
  
}
