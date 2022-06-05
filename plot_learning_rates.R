source("build_mega_model.R")
source("my_functions.R")


if (name == 'Exp3') {
  groups = c('Exp3:NotAtaxic:NoSwitch', 'Exp3:NotAtaxic:Switch')
  
  (plot.learning.rate = plot.learning.rates(learning.rate.values,
                                            groups = groups))
}

if (name == 'Exp5') {
  groups = c('Exp3:NotAtaxic:Switch',
             'Exp5:NotAtaxic:NoSwitch',
             'Exp5:NotAtaxic:Switch')
  
  (plot.learning.rate = plot.learning.rates(learning.rate.values,
                                            groups = groups))
}

if (name == 'Exp4') {
  groups = c('Exp3:NotAtaxic:Switch',
             'Exp4:NotAtaxic:Switch',
             'Exp4:Ataxic:Switch')
  
  (plot.learning.rate = plot.learning.rates(learning.rate.values,
                                            groups = groups))
}
