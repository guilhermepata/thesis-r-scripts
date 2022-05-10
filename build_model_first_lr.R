library(lme4)
# library(lmerTest)
# library(lmtest)
library(modelsummary)
# library(kableExtra)
library(emmeans)
# library(optimx)
# library(EBImage)
# library(minpack.lm)
library(feather)
library(ggplot2)
# library(ggeffects)
# library(effects)
library(sjPlot)
library(tidyverse)
# library(merTools)
source("my_functions.R")

if (!exists("is.builtLR") || ! is.builtLR) {
  
  ### get file
  if (!exists("name")) {name = 'Exp3'}
  name = 'Exp3'
  if (name != 'Exp3') {
    data_frame_lr <- read.csv(
      file = paste('./data/', name, '_perc_df.csv', sep="", collapse=NULL),   
      sep=",",
      dec=".", 
      stringsAsFactors = TRUE)
  } else {
    data_frame_lr = data.frame()
    for (group in c('Switch', 'NoSwitch'))
    {data_frame_lr <-  rbind(data_frame_lr, read.csv(
      file = paste('./data/', name, group, '_perc_df.csv', sep="", collapse=NULL),   
      sep=",",
      dec=".", 
      stringsAsFactors = TRUE))}
  }
  
  # data_frame$Group = relevel(data_frame$Phenotype, ref=3)
  data_frame_lr <- within(data_frame_lr, Group <- factor(Phenotype:Protocol))
  if (name != 'Exp4'){
    data_frame_lr$Group = relevel(data_frame_lr$Group, 'NotAtaxic:NoSwitch')
  } else {
    data_frame_lr$Group = relevel(data_frame_lr$Group, 'NotAtaxic:Switch')
  }
  data_frame_lr <- data_frame_lr[! is.na(data_frame_lr$Asym),]
  # data_frame <- data_frame[data_frame$Protocol == 'Switch',]
  
  data.lr.total_frame <- data_frame_lr
  data.first.lr <- filter(data_frame_lr, Phase == "Split", Num == 0, Perc > 0)
  
  model.equation.lr <- 'Asym ~ Perc * Session * Group + (1 + Perc| Animal)'
  
  model.first.lr<-lmer(model.equation.lr, data=data.first.lr, REML= "true")
 
  modelsummary(model.first.lr, stars=TRUE, metrics=c("RMSE","R2"))
  
  modelsummary(model.first.lr, stars=TRUE, estimate = "{estimate}({p.value})",statistic = "[{conf.low}:{conf.high}]")
  
  data.first.lr.summary = summarise.predict.old(data.first.lr, model.first.lr, Trial, Perc, Num, Session, Group)
  
  # data.washout.summary = summarise.predict(data.washout, model.washout, Trial, Num, Session, Group)
  # data.intersplit.summary = summarise.predict(data.intersplit, model.intersplit, Trial, Num, Session, Group)
  # data.baseline.summary = summarise.predict(data.baseline, model.baseline, Trial, Num, Session, Group)
  # 
  # data.summary = rbind(data.split.summary, data.washout.summary, data.intersplit.summary, data.baseline.summary)
  
  # group_medians <- data.total_frame %>%
  #   group_by(Trial, Session, Group) %>%
  #   summarise(Median = median(Asym))
  
  ### plot model
  (mm_plot <- ggplot(data.first.lr, aes(x = Perc, y = Asym, color = Group)) +
      facet_wrap(~Session, nrow=1) +   # a panel for each session
      geom_point(data=data.first.lr, aes(x = Perc, y = Asym, group = Animal), alpha = 0.5) +
      geom_line(data=data.first.lr, aes(x = Perc, y = Asym, group = Animal, linetype = '--'), alpha = 0.5) +
      theme_classic() +
      geom_line(data = data.first.lr.summary, aes(y = Fit, group=Group),size = 1,) +  # adding predicted line from mixed model
      theme(panel.spacing = unit(2, "lines"))  # adding space between panels
    + ggtitle(model.equation.lr))
  
  is.builtLR <- TRUE
  
}
