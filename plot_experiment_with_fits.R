source("build_model.R")
# source("plot_limits.R")

### plot experiment with fits

### Plot switch group

plot.experiment <- function(data, data.summary) {
  data.median = data %>%
    group_by(Trial, Session) %>%
    summarise(Median = median(Asym))
  data.mean = data %>%
    group_by(Trial, Session) %>%
    summarise(Mean = mean(Asym))
  p <- ggplot(data, aes(x = Trial, y = Asym)) +
    geom_point(alpha = 0.2, aes(colour = Animal)) +
    geom_line(aes(group=Animal, colour = Animal), alpha = 0.2) +
    geom_point(data = data.median, aes(y=Median, group=0), color='Black', size=1) +
    geom_line(data = data.median, aes(y=Median, group=0), color='Black', size=1) +
    # geom_line(data = data.summary, aes(y=Fit, group=0), color='Blue', size=1) +
    geom_ribbon(data = data.summary, aes(y=Fit, ymin=Lower, ymax=Upper, group=0), fill='Black', alpha=0.2) +
    geom_hline(yintercept=c(0), linetype="dotted") +
    theme_classic() +
    theme(legend.position="none") +
    labs(x='Trials', y = "Step length asymmetry (mm)")
  return(p)
}

data.switch <- filter(data.total_frame, Protocol == 'Switch')
data.switch.summary <- filter(data.summary, Group == 'NotAtaxic:Switch')
plot.experiment.switch <- plot.experiment(data.switch, data.switch.summary)


### Plot noswitch group

data.noswitch = filter(data.total_frame, Protocol == 'NoSwitch')
data.noswitch.summary <- filter(data.summary, Group == 'NotAtaxic:NoSwitch')

plot.experiment.noswitch <- plot.experiment(data.noswitch, data.noswitch.summary)




