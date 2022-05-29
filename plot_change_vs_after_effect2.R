source("build_mega_model.R")
source("my_functions.R")

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
           ratio.of.means = FALSE) {
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
      Asym.recov = median(Asym.recov),
      Asym.recov.norm = median(Asym.recov.norm),
      After.effect = median(After.effect),
      After.effect.norm = median(After.effect.norm),
      Ratio = `if`(ratio.of.means, 
                     mean(After.effect) / mean(Asym.recov), 
                     mean(Ratio))
    )

    if (!plot.ratio) {
      p = ggplot() +

        geom_point(
          data = filter(df.animals, Type == 'Raw'),
          aes(
            x = `if`(normalized, Asym.recov.norm * 100, Asym.recov),
            y = `if`(normalized, After.effect.norm * 100, After.effect),
            color = Group,
            shape = Type
          ),
          alpha = 0.5,
        ) +
        geom_line(
          data = filter(df.animals, Type == 'Raw'),
          aes(
            x = `if`(normalized, Asym.recov.norm * 100, Asym.recov),
            y = `if`(normalized, After.effect.norm * 100, After.effect),
            color = Group,
            group = Animal
          ),
          alpha = 0.5,
          linetype = 'dotted',
        ) +

        geom_point(
          data = filter(df.mean, Type == 'Raw'),
          aes(
            x = `if`(normalized, Asym.recov.norm * 100, Asym.recov),
            y = `if`(normalized, After.effect.norm * 100, After.effect),
            color = Group,
            shape = Type
          ),
          alpha = 1,
          size = 3,
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
        scale_color_manual(values = get_group_colors(groups)) +
        scale_shape_manual(values = c(Raw = 16, Fit = 17)) +

        theme_classic() + 
        # theme(legend.position = "none") +
        labs(x = `if`(normalized, "Asymmetry recovered (%)", "Asymmetry recovered (mm)"), 
                        y = `if`(normalized, "After effect (%)", "After effect (mm)"))
        
        
    } else {
      p = ggplot() +

        geom_boxplot(data = filter(df.animals, Type == 'Raw'),
                     aes(x = Group, y = Ratio, color = Group)) +

        geom_col(data = filter(df.mean, Type == 'Raw'),
                   aes(x = Group, y = Ratio, fill = Group), width = 0.5) +

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
    sessions.split = sessions.split.groups[group]

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
          Asym.recov = c(
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
          ),
          Asym.recov.norm = - c(
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
          After.effect.norm = - c(
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

{
  ### plot switch group

  groups = c(
    paste(name, 'NotAtaxic:NoSwitch', sep = ':'),
    paste(name, 'NotAtaxic:Switch', sep = ':')
  )


  if (name == 'Exp3') {
    sessions.split = setNames(list(c('S1', 'S4'), c('S1', 'S5')), groups)
  } else if (name == 'Exp5') {
    sessions.split = setNames(list(c('S1', 'S5'), c('S1', 'S5')), groups)
  }

  (
    plot.change.groups = plot.change(
      mega.data.split,
      mega.data.washout,
      mega.data.summary,
      model.split,
      model.washout,
      groups = unique(filter(mega.data, !grepl('Exp4', Group))$Group),
      sessions.split = sessions.split,
      # normalized = TRUE,
      # limited = TRUE,
      # plot.ratio = TRUE,
      ratio.of.means = FALSE
    )
  )
  
  ggsave(
    plot = plot.change.groups,
    paste("plots/", "COS_vs_AE_", n <- n+1, ".png", sep = ""),
    # device = cairo_pdf,
    width = 8.27,
    height = 8.27,
  )
  
}

