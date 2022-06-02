source('build_mega_model.R')

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
  
  color.fast = `if`(!red.blue, darken(get_group_color(group)), 'red')
  color.slow = `if`(!red.blue, get_group_color(group), "blue")
  color.asym = `if`(dark, "white", "black")
  
  p = ggplot(data.summary) +
    
    
    geom_vline(xintercept = data.session.breaks - 0.5, alpha = 0.5) +
    
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
    theme_classic() +
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
  
  if (dark) {
    p = p + theme_black()
  }
  
  return(p)
}

short.df = data.frame(Group = rep('Exp3:NotAtaxic:NoSwitch', 21),
                      Session = rep('S1', 21),
                      Trial = 1:21,
                      Num = c(0:2,
                              0:8,
                              0:8),
                      Phase = c(rep('Baseline', 3),
                                rep('Split', 9),
                                rep('Washout', 9))
                      )

example.curve = function(df, A = 6, r = 0.1, noise = 0.2) {
  res = c()
  adapt = function(x) {
    return(-A*exp(-x*r))
  }
  B = adapt(max(filter(df, Phase == 'Split')$Num)) - (-A)
  washout = function(x) {
    return(B*exp(-x*r*2))
  }
  for (row in 1:nrow(df)) {
    if (df[row,]$Phase == 'Baseline') {
      res = c(res, 0)
    } else if (df[row,]$Phase == 'Split') {
      res = c(res, adapt(df[row,]$Num))
    } else if (df[row,]$Phase == 'Washout') {
      res = c(res, washout(df[row,]$Num))
    }
    
  }
  res = res + rnorm(length(res), sd=noise)
  return(res)
}

short.df$Asym = example.curve(short.df, r = 0.4, noise = 0)

(plot.short.protocol = plot.protocol(short.df,
                                     'Exp3:NotAtaxic:NoSwitch',
                                     sessions=c('S1'), pos ='right', 
                                     red.blue = TRUE))

ggsave(
  plot = plot.short.protocol,
  paste("presentation_plots/", name, "_fig23_plot", ".png", sep = ""),
  bg = 'transparent',
  # device = cairo_pdf,
  width = 4.75,
  height = 2.8
)


(plot.exp3.noswitch.protocol = plot.protocol(mega.data.summary,
                                             'Exp3:NotAtaxic:NoSwitch', sessions = c('S1','S2','S3','S4','S5'), legend=FALSE))
(plot.exp3.switch.protocol = plot.protocol(mega.data.summary,
                                             'Exp3:NotAtaxic:Switch',
                                           sessions = c('S1','S2','S3','S4','S5'),
                                           legend = FALSE))

(plot.exp5.noswitch.protocol = plot.protocol(mega.data.summary,
                                           'Exp5:NotAtaxic:NoSwitch',
                                           legend = FALSE))
(plot.exp5.switch.protocol = plot.protocol(mega.data.summary,
                                           'Exp5:NotAtaxic:Switch',
                                           legend = FALSE))
(plot.exp4.control.protocol = plot.protocol(mega.data.summary,
                                           'Exp4:NotAtaxic:Switch',
                                           legend = FALSE))

(plot.exp4.ataxic.protocol = plot.protocol(mega.data.summary,
                                           'Exp4:Ataxic:Switch',
                                           legend = FALSE))