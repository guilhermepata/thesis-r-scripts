source("build_mega_model.R")
source("my_functions.R")


plot.retention <- function(data.split.summary,
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
    geom_point(
      data = filter(values, Group %in% groups),
      aes(
        x = Session,
        y = estimate,
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
               alpha = 0.5) +
    
    theme_classic() + theme(legend.position = "none") +
    scale_fill_manual(values = get_group_colors(groups)) +
    scale_color_manual(values = get_group_colors(groups)) +
    labs(x = "Session", y = "Inter-session error reduction (mm)")
  
  return(p)
}

if (name == 'Exp3') {
  groups = c('Exp3:NotAtaxic:NoSwitch', 'Exp3:NotAtaxic:Switch')
  
  (plot.retention = plot.retention(mega.data.split.summary, model.split,
                                   groups = groups))
}

if (name == 'Exp5') {
  groups = c('Exp3:NotAtaxic:Switch',
             'Exp5:NotAtaxic:NoSwitch',
             'Exp5:NotAtaxic:Switch')
  
  (plot.retention = plot.retention(mega.data.split.summary, model.split,
                                   groups = groups))
}

if (name == 'Exp4') {
  groups = c('Exp3:NotAtaxic:Switch',
             'Exp4:NotAtaxic:Switch',
             'Exp4:Ataxic:Switch')
  
  sessions = list()
  for (group in groups) {
    sessions[group] = list(c('S1', 'S2', 'S3'))
  }
  
  (plot.retention = plot.retention(mega.data.split.summary, model.split,
                                   groups = groups, sessions = sessions))
}
