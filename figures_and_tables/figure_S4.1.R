# Appendix D ---------
# Partitioning microbe effects into those of specialist pathogens and generalist mutualists
rm(list = ls())

source("figures_and_tables/core-functions.R")

times <- seq(from = 0, to = 400, by = 0.1)

init <- c("Ni" = 40, "Nj" = 50, "Sk" = 15, "Sl" = 15)
f2_init <- c("Ni" = 40, "Nj" = 50, "Sk" = 15, "Sm" = 15, "Sb" = 10)

scenario_D1_pathOnly <- c(g1 = 1, g2 = 1, 
                          c11 = 0.01, c12 = 0.01, 
                          c21 = 0.01, c22 = 0.01,
                          m1A = -0.02, m1B = 0, 
                          m2A = 0, m2B = -0.01, 
                          vA1 = 0.001, vA2 = 0, 
                          vB1 = 0, vB2 = 0.001,
                          qA = 0.01, qB = 0.01)

scenario_D1_pathOnly_outcome <- do.call(predict_interaction_outcome, as.list(scenario_D1_pathOnly))

to_print_pathOnly <- scenario_D1_pathOnly[7:14]



scenario_D1_complete <- c(g1 = 1, g2 = 1, 
                          c11 = 0.01, c12 = 0.01, 
                          c21 = 0.01, c22 = 0.01,
                          
                          m1A = -0.02, m1B = 0,  m1C = 0.001, 
                          m2A = 0, m2B = -0.01, m2C = .02,
                          
                          
                          vA1 = 0.001, vA2 = 0, 
                          vB1 = 0, vB2 = 0.001, 
                          vC1 = 0.001, vC2 = 0.001,
                          
                          qA = 0.01, qB = 0.01, qC = 0.005)
scenario_D1_complete_outcome <- do.call(f2_predict_interaction_outcome, as.list(scenario_D1_complete))

to_print_ScenarioDComplete <- c(scenario_D1_complete[7:18]) 
to_print_pathOnly <- c(to_print_pathOnly[1:2], "-",
                       to_print_pathOnly[3:4], "-",
                       to_print_pathOnly[5:8], rep("-",2))
to_print <- rbind(to_print_pathOnly, to_print_ScenarioDComplete)
colnames(to_print) = c("$m_{1A}$", "$m_{1B}$", "$m_{1C}$", "$m_{2A}$", "$m_{2B}$", "$m_{2C}$",
                       "$v_{A1}$", "$v_{A2}$", "$v_{B1}$", "$v_{B2}$", "$v_{C1}$", "$v_{C2}$")

rownames(to_print) <- c("Pathogens Only", "All microbes")
table_D1 <- knitr::kable(to_print, "latex", booktabs = TRUE, 
                         escape = F, align=rep('c', 18), digits = 5) %>%
  kable_styling(latex_options = c("striped", "scale_down"))


figure_S4_1_b <- make_coex_cone_w_scenarios(list_of_scenarios = list(scenario_D1_pathOnly_outcome, scenario_D1_complete_outcome), 
                                          lab_adj = .1,
                                          scenario_names = c("Pathogens Only", "Pathogens\nand Mutualists")) +
  labs(tag = "B")

figure_S4_1_c <- ggplot(scenario_D1_pathOnly_outcome$trajectory) + 
  geom_line(aes(x = time, y= size, color = species), size = 1) + 
  scale_color_manual(values = c("black", "red")) + 
  theme_gsk() +
  annotate("text", y = 70, x =  Inf, hjust = 1, 
           label = latex2exp::TeX("$N_2$"), size = 5) + 
  annotate("text", y = 40, x =  Inf, hjust = 1, 
           label = latex2exp::TeX("$N_1$"), size = 5) + 
  theme(legend.position = "none") + 
  labs(tag = "C", subtitle = "Effects of pathogens\nonly") 
figure_S4_1_d <- ggplot(scenario_D1_complete_outcome$trajectory) + 
  geom_line(aes(x = time, y= size, color = species), size = 1) + 
  scale_color_manual(values = c("black", "red")) + 
  theme_gsk() + 
  theme(legend.position = "none") + 
  annotate("text", y = 160, x =  Inf, hjust = 1, 
           label = latex2exp::TeX("$N_2$"), size = 5) + 
  annotate("text", y = 25, x =  Inf, hjust = 1, 
           label = latex2exp::TeX("$N_1$"), size = 5) + 
  ylim(c(0, 180)) + 
  labs(tag = "D", subtitle = "Effects of pathogens\nand mutualist")

figure_S4_1_bcd <- figure_S4_1_b + {figure_S4_1_c / figure_S4_1_d} + plot_layout(widths = c(3,1))

# Save image as PDF

ggsave(filename = "figures_and_tables/figures/figure_S4.1_bcd.pdf", plot = figure_S4_1_bcd, 
       height = 4, width = 7, units = "in")


# NOTE! In the manuscript, this figure_S4_1_bcd generated above is combined with a 
# cartoon diagram of the model made outside of R.
