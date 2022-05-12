library(pracma)
source('build_spatiotemporal_model.R')


coo.range = linspace(min(data.zscored$coo), max(data.zscored$coo), n = 100)
(spatial.means = as.data.frame(emmeans(model.spatiotemporal, ~ coo*Group, 
                         at = list(Phase='Split', 
                                   coo=coo.range, 
                                   double_support=0))))

ds.range = linspace(min(data.zscored$double_support), max(data.zscored$double_support), n = 100)
(temporal.means = as.data.frame(emmeans(model.spatiotemporal, ~ double_support*Group, 
                                       at = list(Phase='Split', 
                                                 double_support=ds.range, 
                                                 coo=0))))

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
                       alpha=0.2) +
    geom_smooth(data=data.zscored, 
               aes(x=coo, y=Asym, color=Group, fill=Group),
               method = lm,
               linetype = "dotted",
               # color=get_group_color(group),
               alpha=0.4,
               size=1) +
    geom_abline(data=lines.frame.spatial,
                aes(intercept = intercept,
                    slope = slope,
                    group = Group,
                    color = Group),
                size=1) +
    # geom_line(data=spatial.means,
    #           aes(x=coo, 
    #               y=emmean, 
    #               group=Group, 
    #               color=Group),
    #           size=1) +
    geom_ribbon(data=spatial.means,
              aes(x=coo, 
                  y=emmean,
                  ymin=lower.CL,
                  ymax=upper.CL,
                  group=Group, 
                  fill=Group),
              size=1,
              alpha=0.4) +
  theme_classic() + theme(legend.position="none") +
  scale_fill_manual(values=c(get_group_color(groups[[1]]), 
                             get_group_color(groups[[2]]))) +
  scale_color_manual(values=c(get_group_color(groups[[1]]), 
                              get_group_color(groups[[2]]))) +
  labs(x="Center of oscillation asym. (z-scored)", y = "Step length asym. (z-scored)")
  )

( plot.parameters.temporal = ggplot() +
    geom_point(data=data.zscored, 
               aes(x=double_support, y=Asym, color=Group),
               # color=get_group_color(group),
               alpha=0.2) +
    geom_smooth(data=data.zscored, 
                aes(x=double_support, y=Asym, color=Group, fill=Group),
                method = lm,
                linetype = "dotted",
                # color=get_group_color(group),
                alpha=0.4,
                size=1) +
    geom_abline(data=lines.frame.temporal, 
                aes(intercept = intercept,
                    slope = slope,
                    group = Group,
                    color = Group),
                size=1) +
    # geom_line(data=temporal.means,
    #           aes(x=double_support, 
    #               y=emmean, 
    #               group=Group, 
    #               color=Group),
    #           size=1) +
    geom_ribbon(data=temporal.means,
                aes(x=double_support, 
                    y=emmean,
                    ymin=lower.CL,
                    ymax=upper.CL,
                    group=Group, 
                    fill=Group),
                size=1,
                alpha=0.4) +
    theme_classic() + theme(legend.position="none") +
    scale_fill_manual(values=c(get_group_color(groups[[1]]), 
                               get_group_color(groups[[2]]))) +
    scale_color_manual(values=c(get_group_color(groups[[1]]), 
                               get_group_color(groups[[2]]))) +
    labs(x="Double support asym. (z-scored)", y = "Step length asym. (z-scored)")
)


