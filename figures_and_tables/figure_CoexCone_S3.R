# A function to generate trajectory of the system
# this gets used by a call to deSolve::ode()
rm(list = ls())
library(patchwork)
library(deSolve)
library(tidyverse)
source("manuscript/code/core-functions.R")

# Super truncated cone ----------
# Make the "super truncated" cone, extending from
# rho = 0 to rho ~ 0.35
coex_cone_for_multisp <- ggplot(data = df) + 
  geom_ribbon(aes(x = niche_diff, ymin = min_fitness_ratio, 
                  ymax = max_fitness_ratio)) +
  scale_y_log10(limits = c(10^-.3, 10^.3)) + 
  ylab(latex2exp::TeX("$\\frac{\\kappa_i}{\\kappa_j}$")) + 
  xlab(latex2exp::TeX("Niche difference ($1-\\rho$)")) +
  annotate("text", x = 0.3, y = 1, label = "Coexistence", hjust = 1,
           col = "white", size = 5, fontface = "bold") + 
  annotate("text", x = 0.2, y = 1.75,
           label = latex2exp::TeX("Exclusion by species $j$"), 
           size = 5, hjust = 0.5) +
  annotate("text", x = 0.2, y = .6,
           label = latex2exp::TeX("Exclusion by species $i$"), 
           size = 5, hjust = 0.5) +
  # scale_x_continuous(expand = c(0, 0), limits = c(0,0.5))  + 
  xlim(c(0, 0.3)) +
  theme_gsk() +   
  theme(axis.title.y = element_text(size = 15, vjust = 0.5),
        axis.title.x = element_text(size = 15))


scenario_3_pair12 <- c(g1 = .2, g2 = .2,
                      c11 = .001, c12 = .001, 
                      c21 = .001, c22 = .001,

                      m1A = -0.012/2, m1B = -0.01/2,   
                      m2A = -0.013/2, m2B = -0.017/2, 
                      
                      vA1 = 0.01, vB1 = 0, 
                      vA2 = 0, vB2 = 0.01, 
                      qA = 0.01, qB = 0.01)
pair_12 <- do.call(predict_interaction_outcome, as.list(scenario_3_pair12))


scenario_3_pair13 <- c(g1 = .2, g2 = .2,
                       c11 = .001,  c12 = .001,
                       c21 = .001,  c22 = .001,
                       
                       m1A = -0.012/2,  m1B = -0.03/2.25,
                       m2A = -.0075/2,  m2B = -0.02/2,
                       
                       vA1 = 0.01, vB1 = 0,
                       vA2 = 0, vB2 = 0.01,
                       qA = 0.01, qB = 0.01)
pair_13 <- do.call(predict_interaction_outcome, as.list(scenario_3_pair13))


scenario_3_pair23 <- c(g1 = .2, g2 = .2,
                       c11 = .001,  c12 = .001,
                       c21 = .001,  c22 = .001,
                       
                       m1A = -0.017/2,  m1B = -0.015/2,
                       m2A =  -0.018/2, m2B = -0.02/2,

                       vA1 = 0.01,  vB1 = 0,
                       vA2 = 0, vB2 = 0.01,
                       qA = 0.01, qB = 0.01)
pair_23 <- do.call(predict_interaction_outcome, as.list(scenario_3_pair23))




coex_cone_scenario3 <- make_coex_cone_w_scenarios(which_base = coex_cone_for_multisp,
                                                  list_of_scenarios = list(pair_12, pair_13, pair_23), 
                           scenario_names = c("S3, i,j = 1,2", 
                                              "S3, i,j = 1,3", 
                                              "S3, i,j = 2,3"), 
                           lab_adj = 0.02) + 
  scale_color_manual(values = "black") 
coex_cone_scenario3


# Scenario S3.b ------------
coex_cone_for_multisp <- ggplot(data = df) + 
  geom_ribbon(aes(x = niche_diff, ymin = min_fitness_ratio, 
                  ymax = max_fitness_ratio)) +
  scale_y_log10(limits = c(10^-.5, 10^.5)) + 
  ylab(latex2exp::TeX("$\\frac{\\kappa_i}{\\kappa_j}$")) + 
  xlab(latex2exp::TeX("Niche difference ($1-\\rho$)")) +
  annotate("text", x = 0.6, y = 1, label = "Coexistence", hjust = 1,
           col = "white", size = 5, fontface = "bold") + 
  annotate("text", x = 0.2, y = 2.85,
           label = latex2exp::TeX("Exclusion by species $j$"), 
           size = 5, hjust = 0.5) +
  annotate("text", x = 0.2, y = .32,
           label = latex2exp::TeX("Exclusion by species $i$"), 
           size = 5, hjust = 0.5) +
  # scale_x_continuous(expand = c(0, 0), limits = c(0,0.5))  + 
  xlim(c(0, 0.75)) +
  theme_gsk() +   
  theme(axis.title.y = element_text(size = 15, vjust = 0.5),
        axis.title.x = element_text(size = 15))




scenario_3_pair12 <- c(g1 = .2, g2 = .2,
                       c11 = .001, c12 = .001, 
                       c21 = .001, c22 = .001,
                       
                       m1A = -0.015/2, m1B = -0.01/2,   
                       m2A = -0.01/2, m2B = -0.013/2, 
                       
                       vA1 = 0.01, vB1 = 0, 
                       vA2 = 0, vB2 = 0.01, 
                       qA = 0.01, qB = 0.01)
pair_12 <- do.call(predict_interaction_outcome, as.list(scenario_3_pair12))


scenario_3_pair13 <- c(g1 = .2, g2 = .2,
                       c11 = .001,  c12 = .001,
                       c21 = .001,  c22 = .001,
                       
                       m1A = -0.015/2,  m1B = -0.01/2,
                       m2A = -.0014/2,  m2B = -0.012/2,
                       
                       vA1 = 0.01, vB1 = 0,
                       vA2 = 0, vB2 = 0.01,
                       qA = 0.01, qB = 0.01)
pair_13 <- do.call(predict_interaction_outcome, as.list(scenario_3_pair13))

scenario_3_pair23 <- c(g1 = .2, g2 = .2,
                       c11 = .001,  c12 = .001,
                       c21 = .001,  c22 = .001,
                       
                       m1A = -0.013/2,  m1B = -0.01/2,
                       m2A =  -0.012/2, m2B = -0.012/2,
                       
                       vA1 = 0.01,  vB1 = 0,
                       vA2 = 0, vB2 = 0.01,
                       qA = 0.01, qB = 0.01)
pair_23 <- do.call(predict_interaction_outcome, as.list(scenario_3_pair23))

coex_cone_scenarioS3.b <- make_coex_cone_w_scenarios(which_base = coex_cone_for_multisp,
                                                  list_of_scenarios = list(pair_12, pair_13, pair_23), 
                                                  scenario_names = c("S3.b, i,j = 1,2", 
                                                                     "S3.b, i,j = 1,3", 
                                                                     "S3.b, i,j = 2,3"), 
                                                  lab_adj = 0.04) + 
  scale_color_manual(values = "white") 
coex_cone_scenarioS3.b

save.image("manuscript/code/figure_coexCone_s3.Rdata")

