# source("my_functions.R")
library(ggplot2)

plot.protocol <- function(data.summary,
                          group,
                          sessions = NULL,
                          legend = TRUE,
                          red.blue = FALSE,
                          # dark = FALSE,
                          asym = FALSE,
                          pos = NULL) {
  legend = !legend
  if (is.null(sessions)) {
    sessions = unique(data.summary$Session)
  }
  if (legend) {
    if (is.null(pos)){
      pos = "top"
    }
  } else {
    pos = "null"
  }
  
  data.summary = filter(data.summary,
                        Group == group,
                        Session %in% sessions)
  
  experiment = str_split(group, ':')[[1]][[1]]
  
  if (experiment == 'Exp4') {
    fast.speed = 0.275
    slow.speed = 0.125
    tied.speed = 0.2
  }  else {
    fast.speed = 0.375
    slow.speed = 0.175
    tied.speed = 0.275
  }
  speeds = data.frame(Slow = c(), Fast = c())
  for (i in 1:nrow(data.summary)) {
    if (data.summary[i, ]$Phase == 'Split') {
      speeds = rbind(speeds, data.frame(Slow = slow.speed, Fast = fast.speed))
    }
    else {
      speeds = rbind(speeds, data.frame(Slow = tied.speed, Fast = tied.speed))
    }
  }
  # print(speeds)
  data.summary = cbind(data.summary, speeds)
  
  
  # if (color == '') {
  #   color = get_group_color(group)
  # }
  
  trial.range = c(min(data.summary$Trial) - 1, max(data.summary$Trial) + 1)
  
  data.session.breaks = data.summary[match(sessions, data.summary$Session),]$Trial
  session.notes = c()
  for (i in 2:length(c(data.session.breaks, max(data.summary$Trial)))) {
    session.notes = c(session.notes, mean(c(data.session.breaks, max(data.summary$Trial))[(i-1):i]))
  }
  
  if (length(data.session.breaks) == 1) {
    data.session.breaks = c()
  } else {
    data.session.breaks = data.session.breaks[2:length(data.session.breaks)]
  }
  
  split.trials = unique(filter(data.summary, Phase == 'Split')$Trial)
  ymax = rep(Inf, length(split.trials))
  ymin = rep(-Inf, length(split.trials))
  split.shades.frame = data.frame(
    Trial = split.trials,
    xmin = split.trials - 0.5,
    xmax = split.trials + 0.5,
    ymin = ymin,
    ymax = ymax
  )
  split.shades.frame = continuous.shades.frame(split.shades.frame)
  
  intersplit.trials = unique(filter(data.summary, Phase == 'Intersplit')$Trial)
  ymax = rep(Inf, length(intersplit.trials))
  ymin = rep(-Inf, length(intersplit.trials))
  intersplit.shades.frame = data.frame(
    Trial = intersplit.trials,
    xmin = intersplit.trials - 0.5,
    xmax = intersplit.trials + 0.5,
    ymin = ymin,
    ymax = ymax
  )
  
  baseline.trials = unique(filter(data.summary, Phase == 'Baseline')$Trial)
  ymax = rep(Inf, length(baseline.trials))
  ymin = rep(-Inf, length(baseline.trials))
  baseline.shades.frame = data.frame(
    Trial = baseline.trials,
    xmin = baseline.trials - 0.5,
    xmax = baseline.trials + 0.5,
    ymin = ymin,
    ymax = ymax
  )
  
  washout.trials = unique(filter(data.summary, Phase == 'Washout')$Trial)
  ymax = rep(Inf, length(washout.trials))
  ymin = rep(-Inf, length(washout.trials))
  washout.shades.frame = data.frame(
    Trial = washout.trials,
    xmin = washout.trials - 0.5,
    xmax = washout.trials + 0.5,
    ymin = ymin,
    ymax = ymax
  )
  
  color.fast = `if`(!red.blue, darken(darken(get_group_color(group))), 'red')
  color.slow = `if`(!red.blue, get_group_color(group), "blue")
  color.asym = `if`(dark, "white", "black")
  
  p = ggplot(data.summary) +
    
    
    geom_vline(xintercept = data.session.breaks - 0.5, alpha = `if`(dark, 0.5, 0.5), color = `if`(dark, "white", "black")) +
    
    geom_rect(
      data = split.shades.frame,
      aes(
        xmin = split.shades.frame$xmin,
        xmax = split.shades.frame$xmax,
        ymin = split.shades.frame$ymin,
        ymax = split.shades.frame$ymax,
        fill = 'Split-belt'
      ),
      alpha = `if`(dark, .3, .3)
    ) +
    
    geom_point(
      data = data.summary,
      aes(
        x = Trial,
        y = Slow,
        color = 'Slow limb'
      ),
      # color = color.slow,
      # fill = color.slow,
      size = 1.5,
      shape = 15,
    ) +
    geom_line(data = data.summary,
              aes(x = Trial, y = Slow, color = 'Slow limb'),
              # color = color.slow,
              linetype = 'dashed',
              size = .5,) +
    
    geom_point(
      data = data.summary,
      aes(
        x = Trial,
        y = Fast,
        color = 'Fast limb'
      ),
      # color = color.fast,
      # fill = color.fast,
      size = 1.5,
      shape = 15,
    ) +
    geom_line(data = data.summary,
              aes(x = Trial, y = Fast, color = 'Fast limb'),
              # color = color.fast,
              linetype = 'dashed',
              size = .5,)
  
  # geom_hline(
  #   yintercept = c(tied.speed),
  #   linetype = "dashed",
  #   alpha = 0.5
  # ) +
  if (asym) {
    p = p +
      geom_point(
        data = data.summary,
        aes(
          x = Trial,
          y = Asym,
          color = 'Asym'
        ),
        # color = color.fast,
        # fill = color.fast,
        size = 1.5,
        shape = 16,
      ) +
      geom_line(data = data.summary,
                aes(x = Trial, y = Asym, color = 'Asym'),
                # color = color.fast,
                linetype = 'dashed',
                size = .5,) 
    
    # geom_hline(
    #   yintercept = c(tied.speed),
    #   linetype = "dashed",
    #   alpha = 0.5
    # ) +
  }
  
  p = p + 
    scale_x_continuous(limits = trial.range, expand = expansion(mult = 0, add = 0)) +
    scale_y_continuous(
      breaks = c(slow.speed, tied.speed, fast.speed),
      expand = expansion(mult = 0.25)
    ) +
    `if`(dark, theme_black(), theme_classic()) +
    scale_colour_manual(
      name = "",
      labels = `if`(!asym, c("Fast limb", "Slow limb"), c("Fast limb", "Slow limb", "Asymmetry")),
      values = `if`(!asym, c(color.fast, color.slow), c(color.fast, color.slow, color.asym))
    ) +
    scale_fill_manual(
      name = "",
      labels = c("Split belt"),
      values = `if`(dark,c('gray'),c('black'))
    ) +
    theme(
      legend.position = pos,
      legend.background = element_rect(fill = 'transparent', color = 'transparent'),
      legend.box.background = element_rect(fill = 'transparent', color = 'transparent'),
      legend.margin=margin(t = 0, unit='cm'),
      legend.text = element_text(size = 6)
    ) +
    # facet_wrap(~ Session) +
    labs(x = 'Trials', y = "Belt speeds (mm)")
  
  # if (dark) {
  #   p = p + theme_black()
  # }
  
  y = get_plot_limits(p)$ymax
  
  for (i in 1:length(session.notes)) {
    x = session.notes[[i]]
    session = sessions[[i]]
    p = p + annotate("text", x = x, y = y, label = session, color = `if`(dark, "white", "black"))
  }
  
  return(p)
}



plot.experiment <-
  function(data,
           data.summary,
           color = '#619CFF',
           show.animals = TRUE,
           show.fit = FALSE,
           separate.phases = TRUE) {
    
    
    data.median = data %>%
      group_by(Trial, Session, Phase) %>%
      summarise(Median = median(Asym))
    data.mean = data %>%
      group_by(Trial, Session, Phase) %>%
      summarise(Mean = mean(Asym))
    data.std.err = data %>%
      group_by(Trial, Session, Phase) %>%
      summarise(Std.err = std_err(Asym))
    
    trial.range = c(min(data.mean$Trial) - 1, max(data.mean$Trial) + 1)
    
    data.session.breaks = data.mean[match(unique(data.mean$Session), data.mean$Session),]$Trial
    data.session.breaks = data.session.breaks[2:length(data.session.breaks)]
    
    split.trials = filter(data.mean, Phase == 'Split')$Trial
    ymax = rep(Inf, length(split.trials))
    ymin = rep(-Inf, length(split.trials))
    
    shades.frame = data.frame(
      Trial = split.trials,
      xmin = split.trials - 0.5,
      xmax = split.trials + 0.5,
      ymin = ymin,
      ymax = ymax
    )
    shades.frame = continuous.shades.frame(shades.frame)
    
    color.dark <- `if`(dark, lighten(color), darken(color))
    color.light <- `if`(dark, darken(color), lighten(color))
    
    p <- ggplot(data, aes(x = Trial)) +
      
      geom_vline(xintercept = data.session.breaks - 0.5, alpha = `if`(dark, 1, 0.5), color = `if`(dark, "white", "black")) +
      
      geom_rect(
        data = shades.frame,
        xmin = shades.frame$xmin,
        xmax = shades.frame$xmax,
        ymin = shades.frame$ymin,
        ymax = shades.frame$ymax,
        alpha = 0.2,
        fill= `if`(dark, "white", "black")
      )
    
    if (show.animals) {
      p = p + geom_point(
        aes(y = Asym),
        alpha = 0.5,
        size = 1,
        shape = 21,
        fill = color.light,
        color = color.light
      ) +
        geom_line(aes(
          y = Asym,
          group = `if`(
            separate.phases,
            interaction(Session, Phase, Animal),
            Animal
          )
        ),
        color = color.light,
        alpha = 0.5)
    } else if (!show.fit) {
      p = p +
        geom_ribbon(
          data = data.std.err,
          aes(
            # y = Std.err,
            ymin = data.mean$Mean - 1.96 * Std.err,
            ymax = data.mean$Mean + 1.96 * Std.err,
            group = `if`(separate.phases, interaction(Session, Phase), 0)
          ),
          fill = color,
          alpha = 0.5,
          size = 0.5,
          width = 0.5
        )
    }
    
    
    p = p + geom_point(
      data = data.mean,
      aes(
        y = Mean,
        group = `if`(separate.phases, interaction(Session, Phase), 0)
      ),
      color = color.dark,
      fill = color.dark,
      size = 1.5,
      shape = 21,
      alpha = 0.6
    ) +
      geom_line(
        data = data.mean,
        aes(
          y = Mean,
          group = `if`(separate.phases, interaction(Session, Phase), 0)
        ),
        color = color.dark,
        size = .5,
        alpha = 0.6
      ) 
      
      
    if (show.fit) {
      
      p  = p +
        geom_ribbon(
          data = data.summary,
          aes(
            y = Fit,
            ymin = Lower,
            ymax = Upper,
            group = `if`(separate.phases, interaction(Session, Phase), 0)
          ),
          fill = color,
          alpha = 0.5,
          size = 0.5,
          width = 0.5
        ) +
        geom_line(
        data = data.summary,
        aes(
          y = Fit,
          group = `if`(separate.phases, interaction(Session, Phase), 0)
        ),
        color = `if`(dark, color.dark, color),
        size = .5,
        alpha = 1
      ) 
    }
    
      
     p = p +
       geom_hline(yintercept = c(0),
                 linetype = "dashed",
                 alpha = 0.5) +
      
      scale_x_continuous(limits = trial.range, expand = expansion(mult = 0, add = 0)) +
      `if`(dark, theme_black(), theme_classic()) +
      theme(legend.position = "none") +
      labs(x = 'Trials', y = "Step length asym. (mm)")
    
    
    
    
    return(p)
  }


plot.initial.error <- function(data.split,
                               data.split.summary,
                               groups,
                               xlim = c('S1', 'S2', 'S3', 'S4', 'S5')) {
  p = ggplot()
  
  p = p +
    
    geom_line(
      data = filter(data.split.summary, Group %in% groups, Num == 0),
      aes(x = Session, y = Fit, group = Group, color = Group),
      size = 0.5,
      position = position_dodge(0.5)
    ) +
    geom_point(
      data = filter(data.split.summary, Group %in% groups, Num == 0),
      aes(
        x = Session,
        y = Fit,
        group = Group,
        color = Group
      ),
      size = 2,
      position = position_dodge(0.5)
    ) +
    geom_errorbar(
      data = filter(data.split.summary, Group %in% groups, Num == 0),
      aes(
        x = Session,
        y = Fit,
        ymin = Lower,
        ymax = Upper,
        group = Group,
        color = Group,
      ),
      width = .2,
      position = position_dodge(0.5)
    ) +
    
    geom_hline(yintercept = c(0),
               linetype = "dashed",
               color = `if`(dark, "white", "black"),
               alpha = 0.5) +
    xlim(xlim) +
    `if`(dark, theme_black(), theme_classic()) + theme(legend.position = "none") +
    scale_fill_manual(values = get_group_colors(groups)) +
    scale_color_manual(values = get_group_colors(groups)) +
    labs(x = "Session", y = "Initial error (mm)")
  
  return(p)
}

plot.learning.rates <- function(values,
                                groups,
                                xlim = c('S1', 'S2', 'S3', 'S4', 'S5'),
                                draw.lines = FALSE) {
  p = ggplot()
  
  if (draw.lines) {
    p = p +
      geom_line(
        data = filter(values, Group %in% groups),
        aes(
          x = Session,
          y = Num.trend,
          group = Group,
          color = Group
        ),
        size = 0.5,
        linetype = 'dotted',
        position = position_dodge(0.5)
      )
  }
  
  p = p +
    geom_point(
      data = filter(values, Group %in% groups),
      aes(
        x = Session,
        y = Num.trend,
        group = Group,
        color = Group
      ),
      size = 1,
      position = position_dodge(0.5)
    ) +
    
    geom_errorbar(
      data = filter(values, Group %in% groups),
      aes(
        x = Session,
        y = Num.trend,
        ymin = lower.CL,
        ymax = upper.CL,
        group = Group,
        color = Group,
      ),
      width = .2,
      position = position_dodge(0.5)
    ) +
    
    geom_hline(yintercept = c(0),
               linetype = "dashed",
               alpha = 0.5) +
    xlim(xlim) +
    `if`(dark, theme_black(), theme_classic()) + theme(legend.position = "none") +
    scale_fill_manual(values = get_group_colors(groups)) +
    scale_color_manual(values = get_group_colors(groups)) +
    labs(x = "Session", y = "Session learning rate (mm/trial)")
  
  return(p)
}

plot.retention.func <- function(data.split.summary,
                           model.split,
                           groups,
                           sessions = NULL,
                           draw.lines = FALSE) {
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
  
  
  p = ggplot()
  
  
  
  if (draw.lines) {
    p = p +
      geom_line(
        data = filter(values, Group %in% groups),
        aes(
          x = Session,
          y = estimate,
          group = Group,
          color = Group
        ),
        size = 0.5,
        linetype = 'dotted',
        position = position_dodge(0.5)
      )
  }
  
  p = p +
    geom_col(
      data = filter(values, Group %in% groups),
      aes(
        x = Session,
        y = estimate,
        group = Group,
        color = Group
      ),
      size = 1,
      alpha = 0.75,
      width = .2,
      position = position_dodge(0.5)
    ) +
    
    geom_errorbar(
      data = filter(values, Group %in% groups),
      aes(
        x = Session,
        y = estimate,
        ymin = lower.CL,
        ymax = upper.CL,
        group = Group,
        color = Group,
      ),
      width = .2,
      position = position_dodge(0.5)
    ) +
    
    geom_hline(yintercept = c(0),
               linetype = "dashed",
               color = `if`(dark, "white", "black"),
               alpha = 0.5) +
    
    `if`(dark, theme_black(), theme_classic()) + theme(legend.position = "none") +
    scale_fill_manual(values = get_group_colors(groups)) +
    scale_color_manual(values = get_group_colors(groups)) +
    labs(x = "Session", y = "Inter-session error reduction (mm)")
  
  return(p)
}

plot.final.washout.asym <- function(data.washout,
                               data.washout.summary,
                               groups,
                               xlim = c('S1', 'S2', 'S3', 'S4', 'S5')) {
  is.max.num <-
    function(num, session, group, data = data.washout.summary) {
      max.num = max(filter(data, Session == session, Group == group)$Num)
      return(num == max.num)
    }
  
  p = ggplot()
  
  p = p +
    
    # geom_line(
    #   data = filter(data.washout.summary, Group %in% groups, is.max.num(Num, Session, Group)),
    #   aes(x = Session, y = Fit, group = Group, color = Group),
    #   size = 0.5,
    #   position = position_dodge(0.5)
    # ) +
    geom_col(
      data = filter(
        data.washout.summary,
        Group %in% groups,
        is.max.num(Num, Session, Group)
      ),
      aes(
        x = Session,
        y = Fit,
        group = Group,
        color = Group
      ),
      # size = 1,
      width = .2,
      position = position_dodge(0.5)
    ) +
    geom_errorbar(
      data = filter(
        data.washout.summary,
        Group %in% groups,
        is.max.num(Num, Session, Group)
      ),
      aes(
        x = Session,
        y = Fit,
        ymin = Lower,
        ymax = Upper,
        group = Group,
        color = Group,
      ),
      width = .2,
      position = position_dodge(0.5)
    ) +
    
    geom_hline(yintercept = c(0),
               linetype = "dashed",
               color = `if`(dark, "white", "black"),
               alpha = 0.5) +
    xlim(xlim) +
    `if`(dark, theme_black(), theme_classic()) + theme(legend.position = "none") +
    scale_fill_manual(values = get_group_colors(groups)) +
    scale_color_manual(values = get_group_colors(groups)) +
    labs(x = "Session", y = "Post-adaptation final error (mm)")
  
  return(p)
}



plot.change.vs.ae <-
  function(
    #  data.split,
    #  data.washout,
    # data.summary,
    # model.split,
    # model.washout,
    df.animals,
    df.fit,
    groups,
    # sessions.split = NULL,
    normalized = FALSE,
    limited = FALSE,
    plot.ratio = FALSE,
    ratio.of.means = FALSE,
    # dark = FALSE,
    show.legend = TRUE,
    types = c('Raw', 'Fit')) {
    
    
    #     df.animals = make_change_df_animals(
    #   data.split,
    #   data.washout,
    #   data.summary,
    #   model.split,
    #   model.washout,
    #   groups,
    #   sessions.split
    # )
    
    df.animals = filter(df.animals, Group %in% groups)
    
    
    df.mean = summarise(
      group_by(df.animals, Type, Group),
      Asym.recov = mean(Asym.recov),
      Asym.recov.std.err = std_err(Asym.recov),
      Asym.recov.norm = mean(Asym.recov.norm),
      Asym.recov.norm.std.err = std_err(Asym.recov.norm),
      After.effect = mean(After.effect),
      After.effect.std.err = std_err(After.effect),
      After.effect.norm = mean(After.effect.norm),
      After.effect.norm.std.err = std_err(After.effect.norm),
      Ratio = `if`(
        ratio.of.means,
        mean(After.effect) / mean(Asym.recov),
        mean(Ratio)
      )
    )
    
    df.fit = filter(df.fit, Group %in% groups)
    
    df.std.err = summarise(
      group_by(df.animals, Type, Group),
      Asym.recov = std_err(Asym.recov),
      Asym.recov.norm = std_err(Asym.recov.norm),
      After.effect = std_err(After.effect),
      After.effect.norm = std_err(After.effect.norm),
    )
    
    df.std.err = filter(df.std.err, Type %in% types)
    
    if (!plot.ratio) {
      p = ggplot() +
        
        # geom_point(
        #   data = filter(df.animals, Type %in% types),
        #   aes(
        #     x = `if`(normalized, Asym.recov.norm * 100, Asym.recov),
        #     y = `if`(normalized, After.effect.norm * 100, After.effect),
        #     color = Group,
        #     shape = Type
        #   ),
        #   alpha = 0.5,
        # ) +
      
        # geom_line(
        #   data = filter(df.animals, Type %in% types),
        #   aes(
        #     x = `if`(normalized, Asym.recov.norm * 100, Asym.recov),
        #     y = `if`(normalized, After.effect.norm * 100, After.effect),
        #     color = Group,
        #     group = Animal
        #   ),
        #   alpha = 0.5,
        #   linetype = 'dotted',
        # ) +
      
      geom_point(
        data = df.fit,
        aes(
          x = COS.Fit,
          y = AE.Fit,
          color = Group
          # shape = Type
        ),
        shape = 16,
        alpha = 1,
        size = 2,
      ) +

        geom_errorbarh(
          data = df.fit,
          aes(
            xmin = COS.Lower,
            xmax = COS.Upper,
            y = AE.Fit,
            color = Group
          ),
          alpha = 1,
          # size = 1,
          height = .2,
          show.legend = FALSE
        ) +
        
        geom_errorbar(
          data = df.fit,
          aes(
            x = COS.Fit,
            ymin = AE.Lower,
            ymax = AE.Upper,
            color = Group
          ),
          alpha = 1,
          # size = 1,
          width = .2,
          show.legend = FALSE
        ) +
        
        geom_abline(
          intercept = 0,
          slope = 1,
          linetype = "dashed",
          color = `if`(dark, "white", "black"),
          alpha = 0.3
        ) +
        
        geom_hline(yintercept = 0,
                   # slope = 1,
                   linetype = "dashed",
                   color = `if`(dark, "white", "black"),
                   alpha = 0.3) +
        
        geom_vline(xintercept = 0,
                   # slope = 1,
                   linetype = "dashed",
                   color = `if`(dark, "white", "black"),
                   alpha = 0.3)
      
      if (limited) {
        p = p + xlim(-50, 200) +
          ylim(-50, 200)
      }
      
      p = p +
        scale_color_manual(values = get_group_colors(groups), labels = get_group_labels(groups)) +
        scale_shape_manual(values = c(Raw = 17, Fit = 16), guide = "none") +
        
        `if`(dark, theme_black(), theme_classic()) +
        theme(legend.position = `if`(show.legend, "right", "none")) +
        labs(
          x = `if`(
            normalized,
            "Total recovered symmetry (%)",
            "Total recovered symmetry (mm)"
          ),
          y = `if`(normalized, "Last after effect (%)", "Last after effect (mm)")
        )
      
      
    } else {
      p = ggplot() +
        
        geom_boxplot(data = filter(df.animals, Type == 'Raw'),
                     aes(x = Group, y = Ratio, color = Group)) +
        
        geom_col(
          data = filter(df.mean, Type == 'Raw'),
          aes(x = Group, y = Ratio, fill = Group),
          width = 0.5
        ) +
        
        geom_hline(yintercept = 0,
                   # slope = 1,
                   linetype = "dashed",
                   alpha = 0.3) +
        
        # ylim(-2, 2) +
        scale_color_manual(values = get_group_colors(groups)) +
        scale_fill_manual(values = get_group_colors(groups)) +
        `if`(dark, theme_black(), theme_classic()) + theme(legend.position = "none")
    }
    
    
    # if (dark) {
    #   p = p + theme_black()
    # }
    
    
    return(p)
  }





make_change_df <- function(data.summary,
                           model.split,
                           model.washout,
                           groups,
                           sessions.split.groups = NULL) {
  if (is.null(sessions.split.groups)) {
    sessions.split.groups = list()
  }
  for (group in groups) {
    if (is.null(sessions.split.groups[group][[1]])) {
      if (grepl('Exp4', group)) {
        sessions.split.groups[group] = list(c('S1', 'S3'))
      }
      else if (grepl('Exp3', group) && grepl('NoSwitch', group))  {
        sessions.split.groups[group] = list(c('S1', 'S4'))
      }else {
        sessions.split.groups[group] = list(c('S1', 'S5'))}
    }
  }
  df = data.frame()
  for (group in groups) {
    sessions.split = sessions.split.groups[group][[1]]
    sessions.test = sessions.split
    if (length(sessions.split) == 1) {
      sessions.split = c(sessions.split[[1]], sessions.split[[1]])
    }
    
    data.summary2 = filter(data.summary, Group == group)
    
    first.trial = min(filter(data.summary2, Phase == 'Split',
                             Session == sessions.split[[1]])$Trial)
    last.trial = max(filter(data.summary2, Phase == 'Split',
                            Session == sessions.split[[2]])$Trial)
    washout.trial = last.trial + 1
    
    first.trial.row = filter(data.summary2, Trial == first.trial)
    last.trial.row = filter(data.summary2, Trial == last.trial)
    washout.row = filter(data.summary2, Trial == washout.trial)
    
    first.num = first.trial.row$Num
    first.session = first.trial.row$Session
    
    last.num = last.trial.row$Num
    last.session = last.trial.row$Session
    
    washout.num = washout.row$Num
    washout.session = washout.row$Session
    
    # if (length(sessions.split) == 1) {
    #   sessions =
    # }
    
    s.split = summary(
      emmeans(
        model.split,
        revpairwise ~ Num * Session * Group,
        at = list(
          Session = sessions.test,
          Num = c(first.num, last.num),
          Group = group
        ),
        adjust = "none"
      )$contrasts,
      infer = TRUE
    )
    
    s.split = filter(
      s.split,
      contrast == paste(
        last.num,
        last.session,
        group,
        '-',
        first.num,
        first.session,
        group,
        sep = ' '
      )
    )
    
    s.washout = summary(
      emmeans(
        model.washout,
        identity ~ Num * Session * Group,
        at = list(
          Session = washout.session,
          Num = washout.num,
          Group = group
        ),
        adjust = "none"
      )$contrasts,
      infer = TRUE
    )
    
    df.aux <- data.frame(
      Group = c(group),
      COS.Fit = c(s.split$estimate),
      COS.Lower = c(s.split$lower.CL),
      COS.Upper = c(s.split$upper.CL),
      COS.P.value = c(s.split$p.value),
      AE.Fit = c(s.washout$estimate),
      AE.Lower = c(s.washout$lower.CL),
      AE.Upper = c(s.washout$upper.CL),
      AE.P.value = c(s.washout$p.value)
    )
    
    df = rbind(df, df.aux)
  }
  
  return(df)
  
}

make_change_df_animals <- function(data.split,
                                   data.washout,
                                   data.summary,
                                   model.split,
                                   model.washout,
                                   groups,
                                   sessions.split.groups = NULL) {
  if (is.null(sessions.split.groups)) {
    sessions.split.groups = list()
  }
  for (group in groups) {
    if (is.null(sessions.split.groups[group][[1]])) {
      if (grepl('Exp4', group)) {
        sessions.split.groups[group] = list(c('S1', 'S3'))
      }
      else if (grepl('Exp3', group) && grepl('NoSwitch', group))  {
        sessions.split.groups[group] = list(c('S1', 'S4'))
      }else {
        sessions.split.groups[group] = list(c('S1', 'S5'))}
    }
  }
  df.animals = data.frame(
    Type = c(),
    Animal = c(),
    Asym.recov = c(),
    After.effect = c(),
    Group = c(),
    First.session = c(),
    Last.session = c(),
    Ratio = c()
  )
  for (group in groups) {
    sessions.split = sessions.split.groups[group][[1]]
    
    sessions.test = sessions.split
    if (length(sessions.split)  == 1) {
      sessions.split = c(sessions.split[[1]], sessions.split[[1]])
    }
    data.summary2 = filter(data.summary, Group == group)
    
    first.trial = min(filter(data.summary2, Phase == 'Split',
                             Session == sessions.split[[1]])$Trial)
    last.trial = max(filter(data.summary2, Phase == 'Split',
                            Session == sessions.split[[2]])$Trial)
    washout.trial = last.trial + 1
    
    first.trial.row = filter(data.summary2, Trial == first.trial)
    last.trial.row = filter(data.summary2, Trial == last.trial)
    washout.row = filter(data.summary2, Trial == washout.trial)
    
    first.num = first.trial.row$Num
    first.session = first.trial.row$Session
    
    last.num = last.trial.row$Num
    last.session = last.trial.row$Session
    
    washout.num = washout.row$Num
    washout.session = washout.row$Session
    
    animal_lists = list(
      filter(
        data.split,
        Group == group,
        Session == first.session,
        Num == first.num
      )$Animal,
      filter(data.split,
             Session == first.session,
             Num == first.num)$Animal,
      filter(data.washout,
             Group == group,
             Session == washout.session)$Animal
    )
    
    animals = Reduce(intersect, animal_lists)
    
    for (type in c('Asym', 'Fit')) {
      for (animal in animals) {
        df.aux = data.frame(
          Animal = c(animal),
          Group = c(group),
          First.session = c(first.session),
          Last.session = c(last.session),
          Type = c(c(Asym = 'Raw', Fit = 'Fit')[type]),
          Asym.recov = c((
            filter(
              data.split,
              Session == last.session &
                Num == last.num &
                Animal == animal
            )[type][[1]] -
              filter(
                data.split,
                Session == first.session &
                  Num == first.num &
                  Animal == animal
              )[type][[1]]
          )),
          Asym.recov.norm = -c(
            (
              filter(
                data.split,
                Session == last.session &
                  Num == last.num &
                  Animal == animal
              )[type][[1]] -
                filter(
                  data.split,
                  Session == first.session &
                    Num == first.num &
                    Animal == animal
                )[type][[1]]
            )
            / filter(
              data.split,
              Session == first.session &
                Num == first.num &
                Animal == animal
            )[type][[1]]
          ),
          After.effect = c(
            filter(
              data.washout,
              Group == group,
              Session == washout.session &
                Num == washout.num &
                Animal == animal
            )[type][[1]]
          ),
          After.effect.norm = -c(
            filter(
              data.washout,
              Group == group,
              Session == washout.session &
                Num == washout.num &
                Animal == animal
            )[type][[1]]
            / filter(
              data.split,
              Session == first.session &
                Num == first.num &
                Animal == animal
            )[type][[1]]
          ),
          Ratio = c(
            filter(
              data.split,
              Group == group,
              Session == washout.session &
                Num == washout.num &
                Animal == animal
            )[type][[1]] /
              (
                filter(
                  data.split,
                  Group == group,
                  Session == last.session &
                    Num == last.num &
                    Animal == animal
                )[type][[1]] - filter(
                  data.split,
                  Group == group,
                  Session == first.session &
                    Num == first.num &
                    Animal == animal
                )[type][[1]]
              )
          )
        )
        df.animals = rbind(df.animals, df.aux)
      }
    }
  }
  
  
  return(df.animals)
  
}