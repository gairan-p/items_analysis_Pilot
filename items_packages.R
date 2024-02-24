set.seed(098765)
some_packages <- c('ggplot2', 'gridExtra', 'dplyr', 'ggthemes', 
                   'tidyverse', 'mirt', 'ggmirt', 'ltm', 'psych', 
                   'readr', 'qgraph', 'bootnet', 'OpenMx', 'EGAnet', 
                   'lavaan', 'summarytools', 'sem', 'psychotools', 'GPArotation',  
                   'MBESS', 'ltm', 'coefficientalpha', 'writexl', 'epmr', 
                   'deltaPlotR', 'corrplot', 'ggdendro', 'difNLR', 
                   'ShinyItemAnalysis', 'difR', 'patchwork', 'viridis',
                   'sjPlot', 'parameters', 'performance', 'stringr',
                   'RColorBrewer', 'wesanderson', 'RMX', 
                   'RISEkbmRasch', 'grateful', 'ggrepel', 'ggrepel', 
                   'kableExtra', 'readxl', 'psychotree', 'matrixStats',
                   'reshape', 'knitr', 'formattable', 'glue', 'foreach',
                   'TAM', 'ltm', 'mokken')

lapply(some_packages, library, character.only=TRUE)

set.seed(5367)

#1) MSA IRT 
#mokken 

#2) Bayesian IRT 
# load required packages
bayes_pkgs <- c('TAM', 'RMX','mirt', 'ltm', 'psych', 'lme4', #basic psychometric 
                'brms', 'rstan') #bayseian IRT   

lapply(bayes_pkgs, library, character.only=TRUE)

# set ggplot theme
theme_set(bayesplot::theme_default())

# set rstan options
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = 2)

# create a "models" folder in the current working directory
# to store fitted model objects for easier re-usage
if (!dir.exists("models")) {
  dir.create("models")
}


# Custom theme for presentations
theme_doc <- function(
    base_size=10) {
  theme_minimal(base_size = base_size)+
    theme(
      plot.title = element_text(size = rel(1.6),face = "bold"),
      plot.subtitle = element_text(size = rel(1.1),face = "italic"),
      axis.text = element_text(size = rel(0.8))
    )
}
