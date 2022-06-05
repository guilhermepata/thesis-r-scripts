source("build_mega_model.R")
library(gsubfn)

groups = unique(mega.data.summary$Group)
sessions = c('S1', 'S2', 'S3', 'S4', 'S5')

height = 5.42
width = 8.98

for (group in groups) {
  
  p = plot.protocol(
    mega.data.summary,
    group = group,
    sessions = sessions,
    legend = FALSE
  )
  
  if (dark) {
    ggsave(
      plot = p,
      paste(
        "presentation_plots2/",
        str_replace_all(group,":" ,"_"),
        "_protocol_plot",
        ".png",
        sep = ""
      ),
      bg = "black",
      width = width / 2,
      height = height /2
    )
  }
}

for (group in groups) {
  p = plot.experiment(
    filter(mega.data, Group == group, Session %in% sessions),
    filter(mega.data.summary, Group == group, Session %in% sessions),
    color = get_group_color(group),
    show.animals = FALSE,
  )
  
  p2 = plot.experiment(
    filter(mega.data, Group == group, Session %in% sessions),
    filter(mega.data.summary, Group == group, Session %in% sessions),
    color = get_group_color(group),
    show.animals = FALSE,
    show.fit = TRUE,
  )
  
  list[p, p2] = set_equal_y_lims(p, p2)
  
  if (dark) {
    ggsave(
      plot = p,
      paste(
        "presentation_plots2/",
        str_replace_all(group,":" ,"_"),
        "_experiment_plot",
        ".png",
        sep = ""
      ),
      bg = "black",
      width = width / 2,
      height = height /2
    )
  }
  if (dark) {
    ggsave(
      plot = p2,
      paste(
        "presentation_plots2/",
        str_replace_all(group,":" ,"_"),
        "_experiment_fit_plot",
        ".png",
        sep = ""
      ),
      bg = "black",
      width = width / 2,
      height = height /2
    )
  }
}


for (g_ind in 1:length(groups)) {
  
  group = groups[[g_ind]]
  
  if (group == 'Exp3:NotAtaxic:NoSwitch') {
    xlim = c('S1', 'S2', 'S3', 'S4')
  } else{
    xlim = sessions
  }
  
  p1 = plot.initial.error(
    mega.data.split,
    mega.data.split.summary,
    groups = groups[[g_ind]],
    xlim = xlim
  )
  
  p2 = plot.initial.error(
    mega.data.split,
    mega.data.split.summary,
    groups = groups[1:g_ind],
    xlim = xlim
  )
  
  list[p1, p2] = set_equal_y_lims(p1, p2)
  
  if (dark) {
    ggsave(
      plot = p1,
      paste(
        "presentation_plots2/",
        str_replace_all(group,":" ,"_"),
        "_initial_error_plot",
        ".png",
        sep = ""
      ),
      bg = "black",
      width = width / 2,
      height = height /2
    )
  }
  if (dark) {
    ggsave(
      plot = p2,
      paste(
        "presentation_plots2/",
        str_replace_all(group,":" ,"_"),
        "_initial_error_all_plot",
        ".png",
        sep = ""
      ),
      bg = "black",
      width = width / 2,
      height = height /2
    )
  }
  
} 

for (g_ind in 1:length(groups)) {
  
  group = groups[[g_ind]]
  
  p1 = plot.retention.func(
    mega.data.split.summary,
    model.split,
    groups = groups[[g_ind]]
  )
  
  p2 = plot.retention.func(
    mega.data.split.summary,
    model.split,
    groups = groups[1:g_ind]
  )
  
  list[p1, p2] = set_equal_y_lims(p1, p2)
  
  if (dark) {
    ggsave(
      plot = p1,
      paste(
        "presentation_plots2/",
        str_replace_all(group,":" ,"_"),
        "_retention_plot",
        ".png",
        sep = ""
      ),
      bg = "black",
      width = width / 2,
      height = height /2
    )
  }
  if (dark) {
    ggsave(
      plot = p2,
      paste(
        "presentation_plots2/",
        str_replace_all(group,":" ,"_"),
        "_retention_all_plot",
        ".png",
        sep = ""
      ),
      bg = "black",
      width = width / 2,
      height = height /2
    )
  }
  
} 

for (g_ind in 1:length(groups)) {
  
  group = groups[[g_ind]]
  
  p1 = plot.change.vs.ae(
    df.animals,
    df.fit,
    show.legend = FALSE,
    types = c("Fit"),
    groups = groups[[g_ind]]
  )
  
  p2 = plot.change.vs.ae(
    df.animals,
    df.fit,
    show.legend = FALSE,
    types = c("Fit"),
    groups = groups[1:g_ind],
  )
  
  list[p1, p2] = set_equal_y_lims(p1, p2)
  list[p1, p2] = set_equal_x_lims(p1, p2)
  
  if (dark) {
    ggsave(
      plot = p1,
      paste(
        "presentation_plots2/",
        str_replace_all(group,":" ,"_"),
        "_change_ae_plot",
        ".png",
        sep = ""
      ),
      bg = "black",
      width = height / 2,
      height = height /2
    )
  }
  if (dark) {
    ggsave(
      plot = p2,
      paste(
        "presentation_plots2/",
        str_replace_all(group,":" ,"_"),
        "_change_ae_all_plot",
        ".png",
        sep = ""
      ),
      bg = "black",
      width = height / 2,
      height = height /2
    )
  }
  
} 

for (g_ind in 1:length(groups)) {
  
  group = groups[[g_ind]]
  
  p1 = plot.final.washout.asym(
    mega.data.washout,
    mega.data.washout.summary,
    groups = groups[[g_ind]]
  )
  
  p2 = plot.final.washout.asym(
    mega.data.washout,
    mega.data.washout.summary,
    groups = groups[1:g_ind]
  )
  
  list[p1, p2] = set_equal_y_lims(p1, p2)
  
  if (dark) {
    ggsave(
      plot = p1,
      paste(
        "presentation_plots2/",
        str_replace_all(group,":" ,"_"),
        "_washout_plot",
        ".png",
        sep = ""
      ),
      bg = "black",
      width = width / 2,
      height = height /2
    )
  }
  if (dark) {
    ggsave(
      plot = p2,
      paste(
        "presentation_plots2/",
        str_replace_all(group,":" ,"_"),
        "_washout_plot",
        ".png",
        sep = ""
      ),
      bg = "black",
      width = width / 2,
      height = height /2
    )
  }
  
} 

for (g_ind in 1:length(groups)) {
  
  group = groups[[g_ind]]
  
  p1 = plot.learning.rates(learning.rate.values,
                           groups[[g_ind]],
                           # xlim = c('S1', 'S2', 'S3', 'S4', 'S5'),
                           draw.lines = TRUE)
  
  p2 = plot.learning.rates(learning.rate.values,
                           groups[1:g_ind],
                           # xlim = c('S1', 'S2', 'S3', 'S4', 'S5'),
                           draw.lines = TRUE)
  
  
  list[p1, p2] = set_equal_y_lims(p1, p2)
  
  if (dark) {
    ggsave(
      plot = p1,
      paste(
        "presentation_plots2/",
        str_replace_all(group,":" ,"_"),
        "_learning_rate_plot",
        ".png",
        sep = ""
      ),
      bg = "black",
      width = width / 2,
      height = height /2
    )
  }
  if (dark) {
    ggsave(
      plot = p2,
      paste(
        "presentation_plots2/",
        str_replace_all(group,":" ,"_"),
        "_learning_rate_all_plot",
        ".png",
        sep = ""
      ),
      bg = "black",
      width = width / 2,
      height = height /2
    )
  }
  
} 
 