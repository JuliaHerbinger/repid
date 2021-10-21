# REPID: Regional Effect Plots with implicit Interaction Detection

This repository gives access to an implementation of the methods
presented in the paper submission “REPID: Regional Effect Plots with implicit Interaction Detection”, 
as well as all code that was used for the
simulations and the real-world example.

This repository is structured as follows:

``` 
    
    ├── data/                   # Location where all generated data are stored
    │   ├── batchtools/         # Location where generated data of batchtools experiments are stored
    │   ├── sim_vine_vs_repid/  # Location where generated data of 1. simulation example (VINE vs. REPID) are stored
    │   ├── sim_weak/           # Location where generated data of 2. simulation example (Weaknesses of other methods) are stored
    │   ├── titanic/            # Location where pre-processed data of titanic example are stored
    |   notebooks/              # Python and R notebooks to generate data for VINE and titanic example
    ├── R/                      # All implemented methods and general helper functions                          
    |   ├── simulations/        # Scripts for simulation examples in paper
    |   |   ├── analysis/       # Scripts used to create figures and tables in the paper for simulation examples
    |   |   ├── batchtools/     # Scripts used to create data for more complex simulation examples (Sec. 4.2, A.4)
    |   ├── real_world_example/ # Script used to create figure for titanic example in Section 5
    ├── LICENSE
    └── README.md               
```



## Reproduce Experiments

Steps to reproduce the experiments of Section 4.2 and A.4.

1.  Install all required packages.

<!-- end list -->

``` r
# from CRAN
install.packages(c("ranger", "dplyr", "batchtools", "mlr", "ggplot2", "gridExtra", "tidyr", "reshape2",
"ggpubr", "BBmisc", "data.table", "stringi", "stringr", "checkmate", "kernlab", "xtable", "devtools",
"tidyverse", "Rmalschains", "iml","kmlShape","dtw","egg","rlist","mgcv","mvtnorm", "vip", "data.table",
"e1071", "RColorBrewer", "R6", "sfsmisc"))


# from github
devtools::install_github("giuseppec/featureImportance")
```

2.  Create an experimental registry, add experiments and problem and run simulations via
    script `R/simulations/generate_data.R`. Data produced by the scripts is stored in 
    the subfolder `data/batchtools/interaction_detection` as a separate registry.
    
3.  Prepare data for analysis by running the script `R/simulations/reduce_experiments.R`.

4.  To reproduce figures and tables of Section 4.2 and A.4.1 and A.4.2, run the script `R/simulations/analysis/analysis_sim_complex.R`. Figures produced within the script are stored in `figures`.


To reproduce the experiments of Section 3 and 4.1, run the scripts `R/simulations/analysis/analysis_sim_vine_vs_repid.R` and `R/simulations/analysis/analysis_sim_weak.R` respectively. To reproduce the results of the real-world example, run the script `R/real_world_example/titanic.R` 

