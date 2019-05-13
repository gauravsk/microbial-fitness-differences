### Header material ---------
### Final set of analyses for microbe/competition theory MS
### Gaurav Kandlikar, gaurav.kandlikar@gmail.com
### Last edit: 18 April 2019  

### This file generates Tables for the manuscript appendix

rm(list = ls())
library(tidyverse)
library(deSolve)
library(patchwork)
library(kableExtra)
library(magick)
library(emdbook)
source("figures_and_tables/core-functions.R")


# Appendix 1 does not have any tables. 

# Appendix 2 ----
col1 = c("$N_i$", "$S_A$", "$S_B$", "$g_i$", "$c_{ij}$", "$m_{ij}^1$", "$v$", "$v_{ij}$", "$q_i$", "$f_i$")
col2 = c("Number [of plants]; Population size of plant $i$", 
         "Dimensionless; proportion of soil community that reflects the microbes cultivated by plant species 1",
         "Dimensionless; proportion of soil community that reflects the microbes cultivated by plant species 2 (constrained to equal $1-S_A$)", 
         "$T^{-1}$; intrinsic rate of growth for plant species $i$", 
         "(Number [of plants])$^{-1}$; per-capita competitive effect of species $j$ on species $i$", 
         "$T^{-1}$; Effect of soil microbial community $j$ on plant species $i$", 
         "Dimensionless; rate at which plant species 2 cultivates its microbial community, relative to the rate at which plant species 1 cultivates its microbial community", 
         "NA", 
         "NA",
         "NA")
col3 = c("\\textit{same as in Bever (2003)}",
         "Number [of microbes]; Abundance of microbial community A",
         "Number [of microbes]; Abundance of microbial community B", 
         "\\textit{same as in Bever (2003)}", 
         "\\textit{same as in Bever (2003)}", 
         "(Number of microbes)$^{-1}$; per-capita effect of microbial community $j$ on plant species $i$", 
         "NA", 
         "(Number [of plants])$^{-1}$; per-capita rate at which plant species j cultivates microbial community $i$",
         "(Number [of microbes])$^{-1}$; Density-dependent mortality rate of microbial community $i$",
         "Intrinsic growth rate of microbial community $i$ (set to zero in all analyses in the main text)")

to_print <- cbind(col1, col2, col3)
colnames(to_print) <- c("Term",
                        "Units and Definition in Bever (2003) model",
                        "Units and Definition in our extended model")
table_s2.1 <- knitr::kable(to_print, format = "latex", booktabs = T, escape = F) %>%
  kableExtra::kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(c(2,3), width = "22em") %>%
  footnote(number = "Bever et al. (1997) define $m_{ij}$ as the growth of plant $i$ 
           with microbial community $j$ minus growth of plant $i$ on uncultivated soil", escape = F)

# Appendix 3 ----
# Generate tables S3.1 and S3.2, which present the parameter
# values used for each of the three Scenarios from the main 
# text of the manuscript. 

# first, import the Rdata file generated in figure_2.R
load("figures_and_tables/figure_2.Rdata")

# Table S3.1 first ----
# Make a list that contains the parameter values for Scenarios 1 and 2
pars_list <- list(scenario_1_parameters,
                  scenario_2_parameters)
# And a list that contains the outcomes for Scenarios 1 and 2
outcome_list <- list(scenario_1_outcome,
                     scenario_2_outcome)

# Make a table out of the following parameters from the two scenarios:
# the four c terms, the four m terms, and the four v terms
scenario_12_parameters <- sapply(pars_list, function(x) x[c("c11", "c12", "c21", "c22",
                                              "m1A", "m1B", "m2A", "m2B",
                                              "vA1", "vA2", "vB1", "vB2")],
                   simplify = T) %>% t
rownames(scenario_12_parameters) = paste("Scenario", c(1, 2))
colnames(scenario_12_parameters) = c("$c_{11}$", "$c_{12}$", "$c_{21}$", "$c_{22}$", 
                       "$m_{1A}$", "$m_{1B}$", "$m_{2A}$", "$m_{2B}$", 
                       "$v_{A1}$", "$v_{A2}$", "$v_{B1}$", "$v_{B2}$")
# Make a table of the outcomes from the two scenarios:
# The outcomes are rho; fitness ratio; and the rho and FD
# considering microbes only or considering competition only
scenario_12_outcomes <-  sapply(outcome_list, function(x) x[c("rho", "fitness_ratio", 
                                                   "rho_micr", "fitness_ratio_micr",
                                                   "rho_comp", "fitness_ratio_comp")]) %>% t
scenario_12_outcomes <- unlist(scenario_12_outcomes) %>% unname %>% matrix(nrow = 2)
colnames(scenario_12_outcomes) <- c("$\\rho$", "$\\kappa_2/\\kappa_1$", 
                         "$\\rho^{micr}$",  "$\\kappa_2/\\kappa_1^{micr}$",
                         "$\\rho^{comp}$",  "$\\kappa_2/\\kappa_1^{comp}$")
to_print <- cbind(scenario_12_parameters, scenario_12_outcomes)
table_C1 <- knitr::kable(to_print, "latex", booktabs = TRUE, 
                         escape = F, align=rep('c', 18), digits = 5) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  add_header_above(c(" " = 1, "Competition coefficients" = 4, 
                     "Microbe effects on plant" = 4,
                     "Microbe cultivation rates" = 4, 
                     "Net outcome" = 2, 
                     "Outcome due to microbes alone" = 2, 
                     "Outcome due to competition alone" = 2))

# Now, Table S3.2 ----
# We need to load the parameter values used to make Figure 3
load("figures_and_tables/figure_3.Rdata")

# The parameters are stored in the object "parameter_vector"
# from figure_3.Rdata. We can extract elements 13:21 and 25:33,
# which contain these terms: the nine m terms, and the nine v terms.
scenario_3_parameters <- c(parameter_vector[c(13:21, 25:33)])
names(scenario_3_parameters) <- c("$m_{1A}$", "$m_{1B}$","$m_{1C}$",
                     "$m_{2A}$", "$m_{2B}$","$m_{2C}$",
                     "$m_{3A}$", "$m_{3B}$","$m_{3C}$",
                     
                     "$v_{A1}$","$v_{B1}$","$v_{C1}$",
                     "$v_{A2}$","$v_{B2}$","$v_{C2}$",
                     "$v_{A3}$","$v_{B3}$","$v_{C3}$")
table_C2 <- knitr::kable(t(scenario_3_parameters), "latex", booktabs = TRUE, escape = F) %>%
  kable_styling(latex_options = c("scale_down")) %>%
  add_header_above(c( "Microbe effects on plant 1" = 3, 
                      "Microbe effects on plant 2" = 3, 
                      "Microbe effects on plant 3" = 3,
                      "Microbe cultivation by plant 1" = 3,
                      "Microbe cultivation by plant 2" = 3,
                      "Microbe cultivation by plant 3" = 3))


# Appendix 3 -----
# Table S3.3

# We need to load the parameter values used to make Figure S3.2
load("figures_and_tables/figure_S3.2.Rdata")

# As above, the parameters are stored in the object "parameter_vector"
# from figure_S3.2.Rdata We can extract elements 13:21 and 25:33,
# which contain these terms: the nine m terms, and the nine v terms.

scenario_S3_2_parameters <- c(parameter_vector[c(13:21, 25:33)])
names(scenario_S3_2_parameters) <- c("$m_{1A}$", "$m_{1B}$","$m_{1C}$",
                     "$m_{2A}$", "$m_{2B}$","$m_{2C}$",
                     "$m_{3A}$", "$m_{3B}$","$m_{3C}$",
                     
                     "$v_{A1}$","$v_{B1}$","$v_{C1}$",
                     "$v_{A2}$","$v_{B2}$","$v_{C2}$",
                     "$v_{A3}$","$v_{B3}$","$v_{C3}$")
table_C3 <- knitr::kable(t(scenario_S3_2_parameters), "latex", booktabs = TRUE, escape = F) %>%
  kable_styling(latex_options = c("scale_down")) %>%
  add_header_above(c( "Microbe effects on plant 1" = 3, 
                      "Microbe effects on plant 2" = 3, 
                      "Microbe effects on plant 3" = 3,
                      "Microbe cultivation by plant 1" = 3,
                      "Microbe cultivation by plant 2" = 3,
                      "Microbe cultivation by plant 3" = 3))

# Appendices 4 and 5 do not have any tables.

# save image -------
save.image(file ="figures_and_tables/appendix_tables.Rdata")

