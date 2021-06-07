###############################################################################################################
# Data analysis of COVID-19 published at: (article submitted waiting for publication)
# date of creation: 06/07/2021 (date in US format)
# R version: 4.0.3
# script name: packages.R
# aim: install/load packages for the file script.R of folder codes
# input: none
# output: none
# external sources: none 
###############################################################################################################


# PACKAGES ----------------------------------------------------------------------------------------------------

base::message("Installing and loading required packages ...")

# list of packages
base::suppressWarnings(
  base::suppressMessages({
    load_lib <- base::c(
      # built in packages
      "base",              # version 4.0.3
      "utils",             # version 4.0.3
      "stats",             # version 4.0.3
      "grDevices",         # version 4.0.3
      # data wrangling
      "reshape2",          # version 1.4.4
      "plyr",              # version 1.8.6
      "dplyr",             # version 1.0.4
      # FA and CFA
      "psych",             # version 2.0.9
      "GPArotation",       # version 2014.11.1
      "lavaan",            # version 0.6.7
      # LMM
      "nlme",              # version 3.1.149
      # data visualization
      "sjPlot",            # version 2.8.7
      "ggplot2",           # version 3.3.3
      "grid",              # version 4.0.3
      "gridExtra",         # version 2.3
      "scales",            # version 1.1.1
      "htmlTable"          # version 2.1.0
    )

    # install/load packages
    install_lib <- load_lib[!load_lib %in% utils::installed.packages()]
    for (lib in install_lib) utils::install.packages(lib, dependencies = TRUE)
    loading <- base::sapply(load_lib, require, character = TRUE)
  })
)

# print errors
if (!any(loading)) {
  base::message("Packages not working: ",  load_lib[!loading])
  stop("The above packages were not properly installed.")
} else {
  base::message("Packages successfully loaded")
}