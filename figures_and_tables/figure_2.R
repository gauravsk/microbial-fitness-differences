### Header material ---------
### Final set of analyses for microbe/competition theory MS
### Gaurav Kandlikar, gaurav.kandlikar@gmail.com
### Last edit: 18 April 2019  

### This file generates Figure 2 for the manuscript.  
rm(list = ls())
library(tidyverse)
library(patchwork)
library(deSolve)
library(kableExtra)
library(magick)

### Import to make coexistence cone base plot ---------
source("figures_and_tables/core-functions.R")
times <- seq(from = 0, to = 85, by = 0.1)

### GENERATE the TWO PLANT+TWO MICROBE scenarios ----
# Scenario 1: 
# Microbes stabilize plant interaction but drive a strong fitness difference
scenario_1_parameters <- c(g1 = 1, g2 = 1, 
                           c11 = 0.003, c12 = 0.0024, 
                           c21 = 0.002, c22 = 0.004,
                           m1A = -0.05, m1B = -0.04, 
                           m2A = -0.01, m2B = -0.021, 
                           vA1 = 0.005, vA2 = 0, vB1 = 0, vB2 = 0.005,
                           qA = 0.01, qB = 0.01)
scenario_1_outcome <- do.call(predict_interaction_outcome, as.list(scenario_1_parameters))

# What hapens if there are no microbes in the system?
# Need to make this object to make the trajectories.
scenario_1_parameters_NOMICR <- c(g1 = 1, g2 = 1, 
                                  c11 = 0.003, c12 = 0.0024, 
                                  c21 = 0.002, c22 = 0.004,
                                  m1A = 0, m1B = 0, m2A = 0, m2B = 0, 
                                  vA1 = 0, vA2 = 0, vB1 = 0, vB2 = 0,
                                  qA = 0.1, qB = 0.1)
scenario_1_outcome_NOMICR <- do.call(predict_interaction_outcome, as.list(scenario_1_parameters_NOMICR))


# Scenario 2: interactive effects
# Microbes and Comp alone drive NO niche diffs; do so interactively
times <- seq(from = 0, to = 125, by = 0.1)

scenario_2_parameters <- c(g1 = 1, g2 = 1,
             c11 = 0.0005, c12 = 0.001, 
             c21 = 0.001, c22 = 0.002,
             m1A = -.025, m1B = -.025,
             m2A = -.01, m2B = -.01,
             vA1 = 0.005, vA2 = 0, vB1 = 0, vB2 = 0.005,
             qA = 0.1, qB = 0.1)

scenario_2_outcome <- do.call(predict_interaction_outcome, as.list(scenario_2_parameters))

# What happens if we ONLY CONSIDER COMPETITION
scenario_2_parameters_NOMICR <- c(g1 = 1, g2 = 1,
                                  c11 = 0.0012, c12 = 0.003, c21 = 0.002, c22 = 0.005,
                                  m1A = 0, m1B = 0,
                                  m2A = 0, m2B = 0,
                                  vA1 = 0, vA2 = 0, vB1 = 0, vB2 = 0,
                                  qA = 0.01, qB = 0.01)
scenario_2_outcome_NOMICR <- do.call(predict_interaction_outcome, 
                              as.list(scenario_2_parameters_NOMICR))

# What happens if we ONLY CONSIDER MICROBES
scenario_2_parameters_NOCOMP <- c(g1 = 1, g2 = 1,
                                  c11 = 0, c12 = 0, c21 = 0, c22 = 0,
                                  m1A = -.005, m1B = -.005,
                                  m2A = -.003, m2B = -.003,
                                  vA1 = 0.005, vA2 = 0, vB1 = 0, vB2 = 0.005,
                                  qA = 0.01, qB = 0.01)
scenario_2_outcome_NOCOMP <- do.call(predict_interaction_outcome, 
                                     as.list(scenario_2_parameters_NOCOMP))


# Make figure 2 ----
scenarios_1and2 <- list(scenario_1_outcome,
                        scenario_2_outcome)

# Get the values of niche overlap from all the scenarios
rho_vec <- c(map_dbl(scenarios_1and2, "rho"),
             map_dbl(scenarios_1and2, "rho_comp"),
             map_dbl(scenarios_1and2, "rho_micr"))
# Get the values of fitness difference from all the scenarios
kappa_vec <- c(map_dbl(scenarios_1and2, "fitness_ratio"),
               map_dbl(scenarios_1and2, "fitness_ratio_comp"),
               map_dbl(scenarios_1and2, "fitness_ratio_micr"))
# Get the outcome from all of the scenarios
outcome <- c(map_chr(scenarios_1and2, "coex_outcome"),
             "coexist", "exclusion",
             "exclusion", "exclusion")
# Name thse six different types of scenarios explored here
names(rho_vec) <- c("S1 (Net)", "S2 (Net)",
                    "S1 (Competition Only)", "S2 (Competition Only)",
                    "S1 (Microbes Only)", "S2 (Microbes Only)")
# make a dataframe out of the vectors above, and add some more
# columns that help with the plotting
plotting_df <- data.frame(rho_vec, kappa_vec, outcome) %>% 
  tibble::rownames_to_column("Scenario")
plotting_df$which_outcome <- c("net", "net", "comp", "comp", "micr", "micr")
plotting_df$label_pos <- 1-plotting_df$rho+.01  
plotting_df$label_pos[1:2] <- plotting_df$label_pos[1:2]+.01 
plotting_df$label_size <- c(5,5,1,1,1,1)

# Extract the values used for making the arrows that point from competition
# only point to net outcome point
segment_df_1 <- data.frame(x1 = 1-plotting_df[3,2], y1 = plotting_df[3,3]*1.1, 
                           x2 = 1-plotting_df[1,2], y2 = plotting_df[1,3]*.85)
segment_df_2 <- data.frame(x1 = (1-plotting_df[4,2])*3, y1 = plotting_df[4,3]*1.1, 
                           x2 = (1-plotting_df[2,2])*.9, y2 = plotting_df[2,3]*.9)

# Assemble Figure 2A by plotting points from the dataframes made above
# onto the coexistence cone 
figure_2A <- coex_cone_truncated + 
  geom_point(data = plotting_df, aes(x = 1-rho_vec, y = kappa_vec, 
                                     size = factor(which_outcome), fill = factor(which_outcome)),
             stroke = 1.2, pch = 21) +
  scale_size_manual(values = c(3,3,6)) + 
  scale_shape_identity() + 
  scale_fill_manual(values = c("#CC79A7", "#CC79A7", "#F0E442")) +
  theme(legend.position = "none") +
  geom_text(data = plotting_df, aes(x = label_pos, y = kappa_vec,
                                    label = Scenario, color = outcome), hjust = 0,
            size = c(5,5,3,3,3,3), fontface = "bold.italic") +
  scale_color_manual(values = c("white", "black"))+
  geom_segment(data = segment_df_1, aes(x = x1, y = y1, xend = x2-.0025, yend = y2-.1),
               arrow = arrow(length = unit(0.01, "npc"), 
                             type = "closed"), linejoin = "mitre" , size = 3.5) +
  geom_segment(data = segment_df_1, aes(x = x1, y = y1, xend = x2-.0025, yend = y2-.1),
               arrow = arrow(length = unit(0.01, "npc"), 
                             type = "closed"), linejoin = "mitre", size = 1.8, 
               color = "#CC79A7") + 
  geom_segment(data = segment_df_2, aes(x = x1+.008, y = y1, xend = x2-.01, yend = y2),
               arrow = arrow(length = unit(0.01, "npc"), 
                             type = "closed"), linejoin = "mitre" , size = 3.5) +
  geom_segment(data = segment_df_2, aes(x = x1+.008, y = y1, xend = x2-.01, yend = y2),
               arrow = arrow(length = unit(0.01, "npc"), 
                             type = "closed"), linejoin = "mitre", size = 1.8, 
               color = "#CC79A7") + 
  labs(tag = "A")


# Now, make Panels 2B-2E using the trajectories from each of the scenario outcomes
# First, scenario 1
scenario1_outcome_w_and_without <- bind_rows(scenario_1_outcome$trajectory,
                                             scenario_1_outcome_NOMICR$trajectory,
                                             .id = "microbes_or_no")

# Make the trajectory plot
traj_1 <- ggplot(scenario1_outcome_w_and_without) +   
  geom_line(aes(x = time, y = size, color = species, linetype = microbes_or_no), size = 1) +
  scale_color_manual(values = c("#0072B2", "#D55E00")) + 
  geom_text(aes(y = max(size)* 1.1, x = 10,  label="")) + 
  geom_text(aes(y = 0, x = 10,  label="")) + 
  theme_gsk()+ 
  scale_y_continuous(expand = c(0, 0))  + 
  theme(legend.position = "none", 
        strip.background = element_blank(), strip.text.x = element_blank()) +
  ylab("Population Size") + 
  labs(tag = "B", title = "Scenario 1") + 
  annotate("text", y = 85, x =  Inf, hjust = 1, 
           label = latex2exp::TeX("$N_2$"), size = 5) + 
  annotate("text", y = 15, x =  Inf, hjust = 1, 
           label = latex2exp::TeX("$N_1$"), size = 5) +
  annotate("text", y = 150, x =  Inf, hjust = 1, 
           label = latex2exp::TeX("$N_2$ (no microbes)"), size = 4) + 
  annotate("text", y = 200, x =  Inf, hjust = 1, 
           label = latex2exp::TeX("$N_1$ (no microbes)"), size = 4) 
  
# Now, for scenario 2.
# There will be three plots:
# 2C) will have trajectory when both microbial effects and competition operate
# 2D) will have trajectory when only competition operates
# 2E) will have trajectory when only microbes operate
traj_2_net <- plot_trajectories(scenario_2_outcome$trajectory, ylab_text = "") + 
  labs(tag = "E",  title =  "Scenario 2", subtitle = "Net Effect") + 
  annotate("text", y = 280, x =  Inf, hjust = 1, 
           label = latex2exp::TeX("$N_2$"), size = 5) + 
  annotate("text", y = 220, x =  Inf, hjust = 1, 
           label = latex2exp::TeX("$N_1$"), size = 5) 
traj_2_microbes <- plot_trajectories(scenario_2_outcome_NOCOMP$trajectory, ylab_text = "") + 
  labs(tag = "C",  title =  "Scenario 2", subtitle = "Microbes only") + 
  annotate("text", y = 575, x =  Inf, hjust = 1, 
           label = latex2exp::TeX("$N_2$"), size = 5) + 
  annotate("text", y = 80, x =  Inf, hjust = 1, 
           label = latex2exp::TeX("$N_1$"), size = 5) 
traj_2_comp <- plot_trajectories(scenario_2_outcome_NOMICR$trajectory, ylab_text = "") + 
  labs(tag = "D",  title =  "Scenario 2", subtitle = "Competition only") + 
  annotate("text", y = 720, x =  Inf, hjust = 1, 
           label = latex2exp::TeX("$N_1$"), size = 5) + 
  annotate("text", y = 80, x =  Inf, hjust = 1, 
           label = latex2exp::TeX("$N_2$"), size = 5) 

# Put them all together
figure_2 <- figure_2A / 
  {traj_1 + {traj_2_microbes + traj_2_comp + traj_2_net} + plot_layout(width = c(1/4, 3/4))} + 
  plot_layout(height = c(2/3, 1/3))


# Save the Environment as figure_2.RData, because it will
# be used to make the tables of parameter values in 
# Appendix S3. 
save.image(file = "figures_and_tables/figure_2.Rdata")

# Save the image as a PDF
ggsave("figures_and_tables/figures/figure-2.pdf", figure_2, width = 8, height = 7, units = "in")

