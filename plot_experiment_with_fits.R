source("build_mega_model.R")
# source("plot_limits.R")

### plot experiment with fits

sessions = c('S1', 'S2', 'S3', 'S4', 'S5')

plot.experiment <- function(data, data.summary, color = '#619CFF') {
  data.median = data %>%
    group_by(Trial, Session, Phase) %>%
    summarise(Median = median(Asym))
  data.mean = data %>%
    group_by(Trial, Session, Phase) %>%
    summarise(Mean = mean(Asym))
  
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
  
  color.dark <- darken(color)
  color.light <- lighten(color)
  
  p <- ggplot(data, aes(x = Trial)) +
    
    geom_vline(xintercept = data.session.breaks - 0.5, alpha = 0.5) +
    
    geom_rect(
      data = shades.frame,
      xmin = shades.frame$xmin,
      xmax = shades.frame$xmax,
      ymin = shades.frame$ymin,
      ymax = shades.frame$ymax,
      alpha = 0.2
    ) +
    
    # geom_point(aes(y=Asym), alpha = 0.5, size=1, shape=21, fill=color.light, color=color.light) +
    # geom_line(aes(y=Asym, group=Animal), color=color.light, alpha = 0.5) +
    
    
    geom_line(
      data = data.summary,
      aes(y = Fit, group = interaction(Session, Phase)),
      fill = color,
      size = .5,
      alpha = 0.75
    ) +
    # geom_point(data = data.summary, aes(y=Fit, group=0), size=1.5, color=color, alpha = 0.6) +
    # geom_errorbar(data = data.summary, aes(y=Fit, ymin=Lower, ymax=Upper, group=0), color=color, alpha = 1, size = 0.5, width=0.5) +
    geom_ribbon(
      data = data.summary,
      aes(
        y = Fit,
        ymin = Lower,
        ymax = Upper,
        group = interaction(Session, Phase)
      ),
      fill = color,
      alpha = 0.5,
      size = 0.5,
      width = 0.5
    ) +
    
    geom_point(
      data = data.mean,
      aes(y = Mean, group = interaction(Session, Phase)),
      color = color.dark,
      fill = color.dark,
      size = 1.5,
      shape = 21,
      alpha = 0.6
    ) +
    geom_line(
      data = data.mean,
      aes(y = Mean, group = interaction(Session, Phase)),
      color = color.dark,
      size = .5,
      alpha = 0.6
    ) +
    
    geom_hline(yintercept = c(0),
               linetype = "dashed",
               alpha = 0.5) +
    
    scale_x_continuous(limits = trial.range, expand = expansion(mult = 0, add = 0)) +
    theme_classic() +
    theme(legend.position = "none") +
    labs(x = 'Trials', y = "Step length asym. (mm)")
  return(p)
}


### Plot switch group

{group = paste(name, 'NotAtaxic:Switch', sep=':')

data.switch <- filter(mega.data, Group == group, Session %in% sessions)
data.switch.summary <-
  filter(mega.data.summary, Group == group)

color = get_group_color(group)

(
  plot.experiment.switch <-
    plot.experiment(data.switch, data.switch.summary, color = color)
)}


### Plot noswitch group

if (name != 'Exp4') {
  group = paste(name, 'NotAtaxic:NoSwitch', sep=':')
  
  data.noswitch <- filter(mega.data, Group == group, Session %in% sessions)
  data.noswitch.summary <-
    filter(mega.data.summary, Group == group)
  
  color = get_group_color(group)
  
  (
    plot.experiment.noswitch <-
      plot.experiment(data.noswitch, data.noswitch.summary, color = color)
  )
  
}
### Plot ataxic group

if (name == 'Exp4') {
  group = paste(name, 'Ataxic:Switch', sep=':')
  
  data.ataxic <- filter(mega.data, Group == group, Session %in% sessions)
  data.ataxic.summary <-
    filter(mega.data.summary, Group == group)
  
  color = get_group_color(group)
  
  (
    plot.experiment.ataxic <-
      plot.experiment(data.ataxic, data.ataxic.summary, color = color)
  )
}
