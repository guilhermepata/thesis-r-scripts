source("build_model.R")
# source("plot_limits.R")

### plot experiment with fits



plot.experiment <- function(data, data.summary, color='#619CFF') {
  data.median = data %>%
    group_by(Trial, Session, Phase) %>%
    summarise(Median = median(Asym))
  data.mean = data %>%
    group_by(Trial, Session, Phase) %>%
    summarise(Mean = mean(Asym))
  
  data.session.breaks = data.mean[match(unique(data.mean$Session), data.mean$Session),]$Trial
  
  split.trials = filter(data.mean, Phase == 'Split')$Trial
  ymax = rep(Inf, length(split.trials))
  ymin = rep(-Inf, length(split.trials))
  
  shades.frame = data.frame(Trial = split.trials,
                            xmin = split.trials - 0.5,
                            xmax = split.trials + 0.5,
                            ymin = ymin,
                            ymax = ymax)
  
  color.dark <- darken(color)
  color.light <- lighten(color)
  
  p <- ggplot(data, aes(x = Trial)) +
    
    geom_vline(xintercept=data.session.breaks-0.5, alpha=0.2) +
    
    geom_rect(data=shades.frame,
                xmin=shades.frame$xmin, xmax=shades.frame$xmax, ymin=ymin, ymax=ymax, alpha=0.2) +
    
    geom_point(aes(y=Asym), alpha = 0.5, size=1, shape=21, fill=color.light, color=color.light) +
    geom_line(aes(y=Asym, group=Animal), color=color.light, alpha = 0.5) +
    
    
    geom_col(data = data.summary, aes(y=Fit, group=0), fill=color, alpha = 0.75) +
    # geom_point(data = data.summary, aes(y=Fit, group=0), size=1.5, color=color, alpha = 0.6) +
    geom_errorbar(data = data.summary, aes(y=Fit, ymin=Lower, ymax=Upper, group=0), color=color, alpha = 1, size = 0.5, width=0.5) +
    
    geom_point(data = data.median, aes(y=Median, group=0), color=color.dark, fill=color.dark, size=1.5, shape=21, alpha=0.6) +
    geom_line(data = data.median, aes(y=Median, group=0), color=color.dark, size=.8, alpha=0.6) +
    
    geom_hline(yintercept=c(0), linetype="dashed", alpha=0.5) +
    
    theme_classic() +
    theme(legend.position="none") +
    labs(x='Trials', y = "Step length asymmetry (mm)")
  return(p)
}


### Plot switch group

data.switch <- filter(data.total_frame, Group == 'NotAtaxic:Switch')
data.switch.summary <- filter(data.summary, Group == 'NotAtaxic:Switch')

color = get_group_color('NotAtaxic:Switch')

(plot.experiment.switch <- plot.experiment(data.switch, data.switch.summary, color=color))


### Plot noswitch group

if (name != 'Exp4')
{
data.noswitch = filter(data.total_frame, Protocol == 'NoSwitch')
data.noswitch.summary <- filter(data.summary, Group == 'NotAtaxic:NoSwitch')

color = get_group_color('NotAtaxic:NoSwitch')

(plot.experiment.noswitch <- plot.experiment(data.noswitch, data.noswitch.summary, color=color))

}
### Plot ataxic group

if (name == 'Exp4' )
{
data.ataxic = filter(data.total_frame, Group == 'Ataxic:Switch')
data.ataxic.summary <- filter(data.summary, Group == 'Ataxic:Switch')

color = get_group_color('Ataxic:Switch')

(plot.experiment.ataxic <- plot.experiment(data.ataxic, data.ataxic.summary, color=color))
}
