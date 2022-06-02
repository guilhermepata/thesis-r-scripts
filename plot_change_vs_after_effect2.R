source("build_mega_model.R")
source("my_functions.R")
library("plotrix")

plot.change <-
  function(data.split,
           data.washout,
           data.summary,
           model.split,
           model.washout,
           groups,
           sessions.split = NULL,
           normalized = FALSE,
           limited = FALSE,
           plot.ratio = FALSE,
           ratio.of.means = FALSE,
           show.legend = TRUE,
           types = c('Raw', 'Fit')) {
    df.animals = make_change_df_animals(
      data.split,
      data.washout,
      data.summary,
      model.split,
      model.washout,
      groups,
      sessions.split
    )
    
    
    df.mean = summarise(
      group_by(df.animals, Type, Group),
      Asym.recov = mean(Asym.recov),
      Asym.recov.std.err = std.error(Asym.recov),
      Asym.recov.norm = mean(Asym.recov.norm),
      Asym.recov.norm.std.err = std.error(Asym.recov.norm),
      After.effect = mean(After.effect),
      After.effect.std.err = std.error(After.effect),
      After.effect.norm = mean(After.effect.norm),
      After.effect.norm.std.err = std.error(After.effect.norm),
      Ratio = `if`(
        ratio.of.means,
        mean(After.effect) / mean(Asym.recov),
        mean(Ratio)
      )
    )
    
      df.fit = make_change_df(
        data.summary,
        model.split,
        model.washout,
        groups,
        sessions.split
      )
    
    df.std.err = summarise(
      group_by(df.animals, Type, Group),
      Asym.recov = std.error(Asym.recov),
      Asym.recov.norm = std.error(Asym.recov.norm),
      After.effect = std.error(After.effect),
      After.effect.norm = std.error(After.effect.norm),
    )
    
    df.std.err = filter(df.std.err, Type %in% types)
    
    if (!plot.ratio) {
      p = ggplot() +
        
        geom_point(
          data = filter(df.animals, Type %in% types),
          aes(
            x = `if`(normalized, Asym.recov.norm * 100, Asym.recov),
            y = `if`(normalized, After.effect.norm * 100, After.effect),
            color = Group,
            shape = Type
          ),
          alpha = 0.5,
        ) +
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
        size = 3,
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
          alpha = 0.3
        ) +
        
        geom_hline(yintercept = 0,
                   # slope = 1,
                   linetype = "dashed",
                   alpha = 0.3) +
        
        geom_vline(xintercept = 0,
                   # slope = 1,
                   linetype = "dashed",
                   alpha = 0.3)
      
      if (limited) {
        p = p + xlim(-50, 200) +
          ylim(-50, 200)
      }
      
      p = p +
        scale_color_manual(values = get_group_colors(groups), labels = get_group_labels(groups)) +
        scale_shape_manual(values = c(Raw = 17, Fit = 16), guide = "none") +
        
        theme_classic() +
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
        theme_classic() + theme(legend.position = "none")
    }
    
    
    
    
    
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
      sessions.split.groups[group] = list(c('S1', 'S5'))
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
      sessions.split.groups[group] = list(c('S1', 'S5'))
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

if (name == 'Exp3') {
  groups = c('Exp3:NotAtaxic:NoSwitch', 'Exp3:NotAtaxic:Switch')
  
  sessions.split = setNames(list(c('S1', 'S4'), c('S1', 'S5')), groups)
  
  (
    plot.change = plot.change(
      mega.data.split,
      mega.data.washout,
      mega.data.summary,
      model.split,
      model.washout,
      groups = groups,
      sessions.split = sessions.split,
      # normalized = TRUE,
      # limited = TRUE,
      # plot.ratio = TRUE,
      types = c('Fit'),
      ratio.of.means = FALSE
    )
  )
  
}


if (name == 'Exp5') {
  groups = c(
    'Exp3:NotAtaxic:NoSwitch',
    'Exp3:NotAtaxic:Switch',
    'Exp5:NotAtaxic:NoSwitch',
    'Exp5:NotAtaxic:Switch'
  )
  
  sessions.split = setNames(list(c('S1', 'S4'), c('S1', 'S5'), c('S1', 'S5'), c('S1', 'S5')), groups)
  
  (
    plot.change = plot.change(
      mega.data.split,
      mega.data.washout,
      mega.data.summary,
      model.split,
      model.washout,
      groups = groups,
      sessions.split = sessions.split,
      # normalized = TRUE,
      # limited = TRUE,
      # plot.ratio = TRUE,
      types = c('Fit'),
      ratio.of.means = FALSE
    )
  )
  
}

if (name == 'Exp4') {
  groups = c(
    'Exp3:NotAtaxic:NoSwitch',
    'Exp3:NotAtaxic:Switch',
    'Exp4:NotAtaxic:Switch',
    'Exp4:Ataxic:Switch'
  )
  
  sessions.split = setNames(list(c('S1', 'S4'), c('S1', 'S5'), c('S1', 'S2'), c('S1', 'S2')), groups)
  
  (
    plot.change = plot.change(
      mega.data.split,
      mega.data.washout,
      mega.data.summary,
      model.split,
      model.washout,
      groups = groups,
      sessions.split = sessions.split,
      # normalized = TRUE,
      # limited = TRUE,
      # plot.ratio = TRUE,
      types = c('Fit'),
      ratio.of.means = FALSE
    )
  )
  
}
