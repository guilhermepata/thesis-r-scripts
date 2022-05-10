source('build_spatiotemporal_model.R')


spatial.trends = as.data.frame(spatial.trend.test$emtrends)
temporal.trends =  as.data.frame(temporal.trend.test$emtrends)
means = as.data.frame(means.test$emmeans)

lines.frame.spatial = data.frame(Group=spatial.trends$Group, 
                         intercept=means$emmean, 
                         slope=spatial.trends$coo.trend)

lines.frame.temporal = data.frame(Group=temporal.trends$Group, 
                                 intercept=means$emmean, 
                                 slope=temporal.trends$double_support.trend)

groups = unique(data.zscored$Group)

( plot.parameters.spatial = ggplot() +
    geom_point(data=data.zscored, 
                       aes(x=coo, y=Asym, color=Group),
                       # color=get_group_color(group),
                       alpha=0.3) +
    geom_abline(data=lines.frame.spatial, 
                aes(intercept = intercept,
                    slope = slope,
                    group = Group,
                    color = Group)
                ) +
  theme_classic() + theme(legend.position="none") +
  scale_fill_manual(values=c(get_group_color(groups[[1]]), 
                             get_group_color(groups[[2]]))) +
  scale_color_manual(values=c(get_group_color(groups[[1]]), 
                              get_group_color(groups[[2]]))) +
  labs(x="Center of oscillation asym. (z-scored)", y = "Step length asym. (s-zcored)")
  )

( plot.parameters.temporal = ggplot() +
    geom_point(data=data.zscored, 
               aes(x=double_support, y=Asym, color=Group),
               # color=get_group_color(group),
               alpha=0.3) +
    geom_abline(data=lines.frame.temporal, 
                aes(intercept = intercept,
                    slope = slope,
                    group = Group,
                    color = Group)
    ) +
    theme_classic() + theme(legend.position="none") +
    scale_fill_manual(values=c(get_group_color(groups[[1]]), 
                               get_group_color(groups[[2]]))) +
    scale_color_manual(values=c(get_group_color(groups[[1]]), 
                               get_group_color(groups[[2]]))) +
    labs(x="Double support asym. (z-scored)", y = "Step length asym. (z-scored)")
)


