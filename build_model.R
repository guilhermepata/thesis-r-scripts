library(lme4)
library(lmerTest)
library(lmtest)
library(modelsummary)
library(kableExtra)
library(emmeans)
library(optimx)
# library(EBImage)
library(minpack.lm)
library(feather)
library(ggplot2)
library(ggeffects)
library(effects)
library(sjPlot)
library(tidyverse)

### get file
name = 'Exp3'
data_frame <- read.csv(
  file = paste('C:/Users/User/Desktop/Guilherme/Mice (openlab)/thesis-r-scripts/data/', name, '_df.csv', sep="", collapse=NULL),  
                       sep=",",
                       dec=".", 
                       stringsAsFactors = TRUE)

data_frame$Group = relevel(data_frame$Phenotype, ref=3)
data_frame <- within(data_frame, Group <- factor(Phenotype:Protocol))
data_frame$Group = relevel(data_frame$Group, 'NotAtaxic:NoSwitch')
data_frame <- data_frame[! is.na(data_frame$Asym),]

data.total_frame <- data_frame
data.split <- data_frame[data_frame$Phase == "Split",]
data.intersplit <- data_frame[data_frame$Phase == "Intersplit",]
data.baseline <- data_frame[data_frame$Phase == "Baseline",]
data.washout <- data_frame[data_frame$Phase == "Washout",]


model.equation <- 'Asym ~ Num * Session * Group + (1 + Num| Animal)'

model.split<-lmer(model.equation, data=data.split, REML= "true")

modelsummary(model.split, stars=TRUE, metrics=c("RMSE","R2"))


### plot model
(mm_plot <- ggplot(data.split, aes(x = Num, y = Asym, colour = Animal, shape = Group, linetype = Group)) +
    facet_wrap(~Session, nrow=1) +   # a panel for each session
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(data.split, pred = predict(model.split)), aes(y = pred), size = 1,) +  # adding predicted line from mixed model
    theme(panel.spacing = unit(2, "lines"))  # adding space between panels
  + ggtitle(model.equation))


