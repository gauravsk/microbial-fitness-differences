### Header material ---------
### Final set of analyses for microbe/competition theory MS
### Gaurav Kandlikar, gaurav.kandlikar@gmail.com
### Last edit: 18 April 2019  

### This file generates Figure 3  for the manuscript and Figure S3.1 for Appendix S3.  

rm(list = ls())
library(tidyverse)
library(patchwork)
library(deSolve)


source("figures_and_tables/core-functions.R")

# Define the parameter vector
# This will have parameters for 3 plant species and 3 associated microbes

parameter_vector <- c(g1 = .2, g2 = .2, g3 = .2,   
                      ## COMPETITION
                      c11 = .001, c12 = .001, c13 = .001,
                      c21 = .001, c22 = .001, c23 = .001,
                      c31 = .001, c32 = .001, c33 = .001,
                      
                      # MICROBE EFFECTS
                      a1A = -0.012/2, a1B = -0.01/2, a1C = -0.03/2.25,
                      a2A = -0.013/2, a2B = -0.017/2, a2C = -0.015/2,
                      a3A = -.0075/2, a3B =  -0.018/2, a3C = -0.02/2,
                    
                      
                      ## MICROBE INTRINSIC
                      qA = .01, qB = .01, qC = .01,
                      
                      ## PLANT EFFECTS ON MICROBES
                      vA1 = 0.01, vB1 = 0, vC1 = 0,
                      vA2 = 0, vB2 = 0.01, vC2 = 0,
                      vA3 = 0, vB3 = 0, vC3 = 0.01)


# Simulate dynamics of the community when all three 
# plant species are present in the comunity
times <- seq(from = 0, to = 1200, by = .2)
init <- c("N1" = 50, "N2" = 20, "N3" = 10,
          "SA" = 10, "SB" = 10, "SC" = 10)

rps_proj <- data.frame(lsoda(y = init, times = times, 
                       func = model_RPS, parms = parameter_vector))

rps_proj <- rps_proj %>% 
  select(-SA, -SB, -SC) %>% gather(., species, size, -time)

# Make the trajectory plot
all_three <- ggplot(rps_proj) +
  geom_line(aes(y = size, x = time, color = species), 
            size = 1) + 
  scale_color_manual(values = c("#000000", "#D55E00", "#0072B2")) + 
  ylab("Population size") + 
  geom_text(aes(y = max(size)* 1.05, x = 10,  label="")) +
  annotate("text", x = Inf, y = 40.7, hjust = 1, 
           label = latex2exp::TeX("$N_1$"), size = 5) + 
  annotate("text", x = Inf, y = 50.7, hjust = 1, 
           label = latex2exp::TeX("$N_2$"), size = 5) + 
  annotate("text", x = Inf, y = 28, hjust = 1, 
           label = latex2exp::TeX("$N_3$"), size = 5) + 
  
  theme_gsk() + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 90))  + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(tag = "A", subtitle = "All three plants present in community") +
  theme(legend.position = "none",
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 15))


# Now, simulate the dynamics of the community when 
# species 1 absent at the beginning
init <- c("N1" = 0, "N2" = 35, "N3" = 50,
          "SA" = 15, "SB" = 10, "SC" = 10)
times <- seq(from = 0, to = 750, by = .2)

rps_proj_1 <- data.frame(lsoda(y = init, times = times,
                               func = model_RPS, parms = parameter_vector))
rps_proj_1
rps_proj_1 <- rps_proj_1 %>%
  select(-SA, -SB, -SC, -N1) %>% gather(., species, size, -time)

# Now, simulate the dynamics of the community when 
# species 2 absent at the beginning
init <- c("N1" = 15, "N2" = 0, "N3" = 50,
          "SA" = 15, "SB" = 10, "SC" = 10)
rps_proj_2 <- data.frame(lsoda(y = init, times = times,
                               func = model_RPS, parms = parameter_vector))
rps_proj_2
rps_proj_2 <- rps_proj_2 %>%
  select(-SA, -SB, -SC, -N2) %>% gather(., species, size, -time)

# Now, simulate the dynamics of the community when 
# species 3 absent at the beginning
init <- c("N1" = 15, "N2" = 75, "N3" = 0,
          "SA" = 15, "SB" = 10, "SC" = 10)
rps_proj_3 <- data.frame(lsoda(y = init, times = times,
                               func = model_RPS, parms = parameter_vector))
rps_proj_3
rps_proj_3 <- rps_proj_3 %>%
  select(-SA, -SB, -SC, -N3) %>% gather(., species, size, -time)

# Use the three data frames made above
# to make plots of trajectories when one of the species is missing
# at the start of the simulation

sp1_missing <- ggplot(rps_proj_1) +
  geom_line(aes(y = size, x = time, color = species),
            size = 1) +
  scale_color_manual(values = c("#D55E00", "#0072B2")) + 
  ylab("Pop. size") +
  xlab("") +
  annotate("text", x = Inf, y = 75, hjust = 1, 
           label = latex2exp::TeX("$N_2$"), size = 4) + 
  annotate("text", x = Inf, y = 25, hjust = 2, 
           label = latex2exp::TeX("$N_3$"), size = 4) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(from = 0, to = 120, by = 40), limits = c(0,110)) + 
  theme_gsk() +
  labs(tag = "B", subtitle = "N1 absent in community") +
  theme(legend.position = "none")

sp2_missing <- ggplot(rps_proj_2) +
  geom_line(aes(y = size, x = time, color = species),
            size = 1) +
  scale_color_manual(values = c("#000000", "#0072B2")) + 
  ylab("Pop. size") +
  xlab("") +
  annotate("text", x = Inf, y = 60, hjust = 1, 
           label = latex2exp::TeX("$N_3$"), size = 4) + 
  annotate("text", x = Inf, y = 24, hjust = 2, 
           label = latex2exp::TeX("$N_1$"), size = 4) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(from = 0, to = 100, by = 40), limits = c(0,100)) + 
  labs(tag = "C", subtitle = "N2 absent in community") +
  theme_gsk() +
  theme(legend.position = "none")
sp3_missing <- ggplot(rps_proj_3) +
  geom_line(aes(y = size, x = time, color = species),
            size = 1) +
  scale_color_manual(values = c("#000000", "#D55E00")) + 
  ylab("Pop. size") +
  annotate("text", x = Inf, y = 107, hjust = 1, 
           label = latex2exp::TeX("$N_1$"), size = 4) + 
  annotate("text", x = Inf, y = 32, hjust = 2, 
           label = latex2exp::TeX("$N_2$"), size = 4) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(from = 0, to = 100, by = 40), limits = c(0,145)) + 
  theme_gsk() +
  labs(tag = "D", subtitle = "N3 absent in community") +
  theme(legend.position = "none")


# Now, combine the plot showing the trajectory when all three species are
# present with the three plots showing trajectories when one of the
# plant species is missing

figure_3 <- all_three + 
  {sp1_missing/sp2_missing/sp3_missing} +
  plot_layout(widths = c(3, 1)) +
  NULL

figure_3

# Save the image as a PDF 
ggsave("figures_and_tables/figures/figure-3.pdf", figure_3, width = 8, height = 4.5, units = "in")


# Now, make Figure S3.1 ------------
# This figure shows these three species pairs in the nd/fd space. 

init <- c("Ni" = 40, "Nj" = 50, "Sk" = 15, "Sl" = 15)


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

ggsave(filename = "figures_and_tables/figures/figure-S3.1.pdf",
       plot = coex_cone_scenario3, height = 5, width = 8)


# Save the Environment as figure_3.RData, because it will
# be used to make the tables of parameter values in 
# Appendix S3. 
save.image(file = "figures_and_tables/figure_3.Rdata")
