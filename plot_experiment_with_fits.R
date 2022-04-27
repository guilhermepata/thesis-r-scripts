### plot experiment with fits
library(cowplot)


### Plot switch group

data.switch = filter(data.total_frame, Protocol == 'Switch')
data.switch.median = data.switch %>%
  group_by(Trial, Session) %>%
  summarise(Median = median(Asym))
data.switch.mean = data.switch %>%
  group_by(Trial, Session) %>%
  summarise(Mean = mean(Asym))

plot.experiment.switch <- ggplot(data.switch, aes(x = Trial, y = Asym)) +
  # facet_wrap(~Session, nrow=1) +   # a panel for each session
  geom_point(alpha = 0.2, aes(colour = Animal)) +
  geom_line(aes(group=Animal, colour = Animal), alpha = 0.2) +
  geom_point(data = data.switch.median, aes(y=Median, group=0), color='Black', size=1) +
  geom_line(data = data.switch.median, aes(y=Median, group=0), color='Black', size=1) +
  # geom_point(data = 
  #              cbind(
  #                filter(data.switch, Phase=='Split'), 
  #                fit = predict(
  #                  model.split, 
  #                  newdata=filter(data.switch, Phase=='Split'), 
  #                  re.form=~0)
  #                ), 
  #            aes(y = fit),
  #            size = 2,
  #            color='Red')  # adding predicted line from mixed model
  # theme(panel.spacing = unit(2, "lines"))  # adding space between panels
  # ggtitle(model.equation)
  geom_hline(yintercept=c(0), linetype="dotted") +
  ylim(-10,10) +
  theme_classic() +
  theme(legend.position="none") 


### Plot noswitch group

data.noswitch = filter(data.total_frame, Protocol == 'NoSwitch')
data.noswitch.median = data.noswitch %>%
  group_by(Trial, Session) %>%
  summarise(Median = median(Asym))
data.noswitch.mean = data.noswitch %>%
  group_by(Trial, Session) %>%
  summarise(Mean = mean(Asym))


plot.experiment.noswitch <- ggplot(data.noswitch, aes(x = Trial, y = Asym)) +
    # facet_wrap(~Session, nrow=1) +   # a panel for each session
    geom_point(alpha = 0.2, aes(colour = Animal)) +
    geom_line(aes(group=Animal, colour = Animal), alpha = 0.2) +
    geom_point(data = data.noswitch.median, aes(y=Median, group=0), color='Black', size=1) +
    geom_line(data = data.noswitch.median, aes(y=Median, group=0), color='Black', size=1) +
    # geom_point(data = 
    #              cbind(
    #                filter(data.noswitch, Phase=='Split'), 
    #                fit = predict(
    #                  model.split, 
    #                  newdata=filter(data.noswitch, Phase=='Split'), 
    #                  re.form=~0)
    #                ), 
    #            aes(y = fit),
    #            size = 2,
    #            color='Red')  # adding predicted line from mixed model
  # theme(panel.spacing = unit(2, "lines"))  # adding space between panels
  # ggtitle(model.equation)
  geom_hline(yintercept=c(0), linetype="dotted") +
  ylim(-10,10) +
  theme_classic() +
  theme(legend.position="none") 

### join them together


plot_grid(plot.experiment.noswitch, plot.experiment.switch, nrow = 2, labels = 'AUTO')




