## Note: this app is under development as of 13 May 2019.


This subdirectory contains apps to run a Shiny app to interact with the models in this manuscript.

A live demo of the Shiny app is available at https://gauravsk.shinyapps.io/microbe-mediated-fitdiffs/.

The app can also be run directly from R. It depends on the following packages available from CRAN: tidyverse, deSolve, latex2exp, shinydashboard. These can be installed with `install.package(<packagename>)`. It also depends on the [patchwork](https://github.com/thomasp85/patchwork) package, which can be installed with `devtools::install_github("thomasp85/patchwork")`. 

Once these packages (as well as the Shiny package, `install.packages(shiny)`) are available, the app can be run with the following line:

`shiny::runGitHub("microbially-mediated-fitness-differences", "gauravsk", subdir = "shiny-app")`.




*Note that `core-functions.R` is a duplicate of `../figures_and_tables/core-functions.R`*