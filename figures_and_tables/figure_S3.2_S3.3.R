### Header material ---------
### Final set of analyses for microbe/competition theory MS
### Gaurav Kandlikar, gaurav.kandlikar@gmail.com
### Last edit: 18 April 2019  

### This file generates Figures S3.2 and S3.3 for Appendix S3 of the manuscript.  


rm(list = ls())
library(patchwork)
library(deSolve)
library(tidyverse)
source("figures_and_tables/core-functions.R")

## NOTE! 
# The following code is very similar to the code in figure_3.R
# Only the parameter values are meaningfully different.

# Define the parameter vector
# This will have parameters for 3 plant species and 3 associated microbes
parameter_vector <- c(g1 = .2, g2 = .2, g3 = .2,   
                      ## COMPETITION
                      c11 = .001, c12 = .001, c13 = .001,
                      c21 = .001, c22 = .001, c23 = .001,
                      c31 = .001, c32 = .001, c33 = .001,
                      
                      # MICROBE EFFECTS
                      m1A = -0.015/2, m1B = -0.01/2,  m1C = -0.01/2,
                      m2A = -0.01/2,  m2B = -0.013/2, m2C = -0.01/2,
                      m3A = -0.014/2, m3B = -0.012/2, m3C = -0.012/2,
                      
                      
                      ## MICROBE INTRINSIC
                      qA = .01, qB = .01, qC = .01,
                      
                      ## PLANT EFFECTS ON MICROBES
                      vA1 = 0.01, vB1 = 0, vC1 = 0,
                      vA2 = 0, vB2 = 0.01, vC2 = 0,
                      vA3 = 0, vB3 = 0, vC3 = 0.01)


# Simulate dynamics of the community when all three 
# plant species are present in the comunity
times <- seq(from = 0, to = 750, by = .2)
init <- c("N1" = 50, "N2" = 20, "N3" = 40,
          "SA" = 10, "SB" = 10, "SC" = 10)

rps_proj <- data.frame(lsoda(y = init, times = times, 
                             func = model_RPS, parms = parameter_vector))
rps_proj
rps_proj <- rps_proj %>% 
  select(-SA, -SB, -SC) %>% gather(., species, size, -time)

# Make the trajectory plot
all_three <- ggplot(rps_proj) +
  geom_line(aes(y = size, x = time, color = species), 
            size = 1) + 
  scale_color_manual(values = c("#000000", "#D55E00", "#0072B2")) + 
  ylab("Population size") + 
  geom_text(aes(y = max(size)* 1.05, x = 10,  label="")) +
  annotate("text", x = Inf, y = 58, hjust = 1, 
           label = latex2exp::TeX("$N_1$"), size = 5) + 
  annotate("text", x = Inf, y = 93, hjust = 1, 
           label = latex2exp::TeX("$N_2$"), size = 5) + 
  annotate("text", x = Inf, y = 4, hjust = 1, 
           label = latex2exp::TeX("$N_3$"), size = 5) + 
  
  theme_gsk() + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100))  + 
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
  annotate("text", x = Inf, y = 107, hjust = 1, 
           label = latex2exp::TeX("$N_2$"), size = 4) + 
  annotate("text", x = Inf, y = 35, hjust = 2, 
           label = latex2exp::TeX("$N_3$"), size = 4) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(from = 0, to = 120, by = 40), limits = c(0,120)) + 
  theme_gsk() +
  labs(tag = "B", subtitle = "N1 absent in community") +
  theme(legend.position = "none")

sp2_missing <- ggplot(rps_proj_2) +
  geom_line(aes(y = size, x = time, color = species),
            size = 1) +
  scale_color_manual(values = c("#000000", "#0072B2")) + 
  ylab("Pop. size") +
  xlab("") +
  annotate("text", x = Inf, y = 30, hjust = 1, 
           label = latex2exp::TeX("$N_3$"), size = 4) + 
  annotate("text", x = Inf, y = 100, hjust = 2, 
           label = latex2exp::TeX("$N_1$"), size = 4) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(from = 0, to = 100, by = 40), limits = c(0,120)) + 
  labs(tag = "C", subtitle = "N2 absent in community") +
  theme_gsk() +
  theme(legend.position = "none")
sp3_missing <- ggplot(rps_proj_3) +
  geom_line(aes(y = size, x = time, color = species),
            size = 1) +
  scale_color_manual(values = c("#000000", "#D55E00")) + 
  ylab("Pop. size") +
  annotate("text", x = Inf, y = 37, hjust = 1, 
           label = latex2exp::TeX("$N_1$"), size = 4) + 
  annotate("text", x = Inf, y = 103, hjust = 2, 
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


figure_S3.2 <- all_three + 
{sp1_missing/sp2_missing/sp3_missing} +
  plot_layout(widths = c(3, 1)) +
  NULL

# Save the image as a PDF
ggsave("figures_and_tables/figures/figure-S3.2.pdf", figure_S3.2, width = 8, height = 4.5, units = "in")



# Now, make figure S3.3. ------
# This shows the three species pairs from above in the nd/fd space.
init <- c("Ni" = 40, "Nj" = 50, "Sk" = 15, "Sl" = 15)


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

ggsave(filename = "figures_and_tables/figures/figure-S3.3.pdf",
       plot = coex_cone_scenarioS3.b, height = 5, width = 8)

# Save the Environment as figure_3.RData, because it will
# be used to make the tables of parameter values in 
# Appendix S3. 
save.image(file = "figure_S3.2.Rdata")


