library(pracma)
source('build_spatiotemporal_model.R')

plot.parameters <- function(param, param_title, data = data.zscored, model = model.spatiotemporal) {
  param.range = linspace(min(data[,param]), max(data[,param]), n = 100)
  at = list()
  at[[param]] = param.range
  param.means = as.data.frame(emmeans(model, 
                                      formula(paste('~', param , '*', 'Group')),
                                      at=at ))
  # param.trends = as.data.frame(emtrends(model, pairwise ~ Group, var = param))
  groups = unique(param.means$Group)
  
  end = length(param.range)
  param.fit.frame = data.frame(Group = groups, 
                               x = c(param.range[[1]], param.range[[1]]),
                               y = c(filter(param.means, Group == groups[[1]])$emmean[[1]],
                                     filter(param.means, Group == groups[[2]])$emmean[[1]]),
                               xend = c(param.range[[end]], param.range[[end]]),
                               yend = c(filter(param.means, Group == groups[[1]])$emmean[[end]],
                                        filter(param.means, Group == groups[[2]])$emmean[[end]]))
  
  p = ggplot() + 
    geom_point(data=data, 
               aes(x=!!sym(param), y=Asym, color=Group),
               alpha=0.2) +
    geom_smooth(data=data, 
                aes(x=!!sym(param), y=Asym, color=Group, fill=Group),
                method = lm,
                linetype = "dotted",
                alpha=0.4,
                size=1) +
    geom_ribbon(data=param.means,
                aes(x=!!sym(param), 
                    y=emmean,
                    ymin=lower.CL,
                    ymax=upper.CL,
                    group=Group, 
                    fill=Group),
                size=1,
                alpha=0.4) +
    geom_segment(data = param.fit.frame,
                 aes(x=x,
                     y=y,
                     xend=xend,
                     yend=yend,
                     color=Group),
                 size=1) +
    theme_classic() + theme(legend.position="none") +
    scale_fill_manual(values=c(get_group_color(groups[[1]]), 
                               get_group_color(groups[[2]]))) +
    scale_color_manual(values=c(get_group_color(groups[[1]]), 
                                get_group_color(groups[[2]]))) +
    labs(x=paste(param_title, "(z-scored)"), y = "Step length asym. (z-scored)")
  
  return(p)
}


( plot.parameters.spatial = plot.parameters(param='coo', param_title = 'Center of oscillation asym.'))

( plot.parameters.temporal = plot.parameters(param='double_support', param_title = 'Double support asym.')) 

( plot.parameters.spatial.ind = plot.parameters(param='swing_length', param_title = 'Swing length asym.'))

( plot.parameters.temporal.ind = plot.parameters(param='duty_factor', param_title = 'Duty factor asym.'))
