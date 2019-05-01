#### Header material ---------
### Final set of analyses for microbe/competition theory MS
### Gaurav Kandlikar
### Last edit: 18 April 2019  

### This file includes general purpose functions that 
### get used to make various figures and tables 
### for the manuscript

# Import libraries
library(tidyverse)


# Define functions for GENERAL PLOTTING UTILITIES --------------

# define new theme for ggplots
theme_gsk <- function() {
  theme_minimal()+
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.tag = element_text(face = "bold")
    ) 
}

# Make the "coexistence cone" used in Figure 2 
# Define vectors for niche differences and the corresponding 
# min/max fitness difference that permits coexistence

niche_differentiation <- seq(from = 0, to = 1, by = 0.001)
niche_overlap <- 1-niche_differentiation
fitness_ratio_min <- niche_overlap
fitness_ratio_max <- 1/niche_overlap

df <- data.frame(niche_diff = niche_differentiation,
                 min_fitness_ratio = fitness_ratio_min,
                 max_fitness_ratio = fitness_ratio_max)

# make the plot
coex_cone_truncated <- ggplot(data = df) + 
  geom_ribbon(aes(x = niche_diff, ymin = min_fitness_ratio, 
                  ymax = max_fitness_ratio)) +
  scale_y_log10(limits = c(10^-.6, 10^.6)) + 
  ylab(latex2exp::TeX("$\\frac{\\kappa_2}{\\kappa_1}$")) + 
  xlab(latex2exp::TeX("Niche difference ($1-\\rho$)")) +
  annotate("text", x = 0.68, y = 1, label = "Coexistence", hjust = 1,
           col = "white", size = 5, fontface = "bold") +
  annotate("text", x = 0.2, y = 3.75,
           label = "Exclusion by Species 2",
           size = 5, hjust = 0.5) +
  annotate("text", x = 0.2, y = .3,
           label = "Exclusion by Species 1",
           size = 5, hjust = 0.5) +
  xlim(c(0, 0.7)) +
  theme_gsk() +
  theme(axis.title.y = element_text(size = 15, vjust = 0.5),
        axis.title.x = element_text(size = 15))


# Define a function that adds scenarios onto a Coexistenice Cone plot 
# Inputs to the function are:
# which_base: choose whether to use coex_cone_truncated or coex_cone
# list_of_scenarios: a list created out of the outputs of the function 'predict_interaction_outcome'
# lab_adj: how far to move the label (in the x-direction) from the ND/FD point
# scenario_names: vector of labels for each scenario
make_coex_cone_w_scenarios <- function(list_of_scenarios,
                                       lab_adj = 0.03, scenario_names) {
  # list_of_scenarios is a list of outputs from the function predict_interaction_outcome() 
  # extract relevant values from the list:
  rhos <- sapply(list_of_scenarios, function(x) 1-x$rho, simplify = T)
  fitness_ratios <- sapply(list_of_scenarios, function(x) x$fitness_ratio, simplify = T)
  Scenario <- scenario_names
  outcome <- sapply(list_of_scenarios, function(x) x$coex_outcome, simplify = T)
  for_plot <- data.frame(rhos, fitness_ratios, Scenario, outcome)
  
  # make the plot using the base selected and the objects made above
  coex_cone_withScenarios <- coex_cone_truncated + 
    geom_point(data = for_plot, aes(x = rhos, y = fitness_ratios, fill = Scenario), 
               size = 7, shape = 21) +
    scale_fill_brewer(palette="Set1")+
    geom_text(data = for_plot, aes(x = rhos+lab_adj, 
                                   y = fitness_ratios, 
                                   label = Scenario, 
                                   color = outcome),
              size = 4, fontface = "bold.italic") +
    scale_color_manual(values = c("white", "black", "white")) +
    theme(legend.position = "null") 
  
  return(coex_cone_withScenarios)
  
}

# Define a function that plots trajectories from scenarios
# inputs are as named; 
# list_of_scenarios is a list made out of the output of 
# the function predict_interaction_outcomes()
plot_trajectories <- function(list_of_scenarios, ylab_text = "Population size",
                              sp_1_label = "N1", sp_2_label = "N2") {
  
  # If the list_of_scenarios contains more than one scenario,
  # then bind the different trajectories.
  if(is.data.frame(list_of_scenarios)) {
    combined <- list_of_scenarios
    combined$Scenario <- "1"
  } else {
    combined <- dplyr::bind_rows(lapply(list_of_scenarios, 
                                        function(x) x$trajectory), .id = "Scenario") 
  }
  
  # make the ggplot.
  trajectories <- ggplot(combined) +   
    geom_line(aes(x = time, y = size, color = species), size = 1) +
    scale_color_manual(values = c("#0072B2", "#D55E00")) + 
    facet_wrap(~Scenario, ncol = 2, scales = "free") +
    geom_text(aes(y = max(size)* 1.1, x = 10,  label="")) + 
    geom_text(aes(y = 0, x = 10,  label="")) + 
    ylab(ylab_text) +
    theme_gsk()+ 
    scale_y_continuous(expand = c(0, 0))  + 
    theme(legend.position = "none", 
          strip.background = element_blank(), strip.text.x = element_blank())
  
  # return the plot.
  return(trajectories)
}


# Microbe Density Based Model ----------
# Define functions that simulate dynamics and
# calculate the various metrics (e.g. alphas, rho, ..)
# for the PHENOMENOLOGICAL COMPETITION MODEL

# setup global variables 
times <- seq(from = 0, to = 150, by = 0.1)
init <- c("Ni" = 40, "Nj" = 50, "Sk" = 15, "Sl" = 15)

# calculate the low density of growth rates for each species
lowdens_grs <- function(g1, g2, ...) {
  r1 <- g1
  r2 <- g2
  to_return <- c("r1" = r1, "r2" = r2)
  return(to_return)
}

# calculate the interaction matrix for each species pair
calculate_alphas <- function(g1, g2, 
                             c11, c12, c21, c22,
                             m1A, m1B, m2A, m2B, 
                             vA1, vA2, vB1, vB2,
                             qA, qB) {
  
  plant_rs <- lowdens_grs(g1, g2)
  
  alpha_ii <- (c11 - (m1A*vA1/qA + m1B*vB1/qB))
  alpha_ij <- (c12 - (m1A*vA2/qA + m1B*vB2/qB))
  alpha_jj <- (c22 - (m2A*vA2/qA + m2B*vB2/qB))
  alpha_ji <- (c21 - (m2A*vA1/qA + m2B*vB1/qB))
  
  interaction_matrix <- matrix(c(alpha_ii, alpha_ij,
                                 alpha_ji, alpha_jj), byrow = TRUE, ncol = 2)
  colnames(interaction_matrix) <- c("p1", "p2")
  rownames(interaction_matrix) <- c("p1", "p2")
  
  # Due to competition alone
  alpha_ii_c <- (c11 - 0)
  alpha_ij_c <- (c12 - 0)
  alpha_jj_c <- (c22 - 0)
  alpha_ji_c <- (c21 - 0)
  interaction_matrix_c <- matrix(c(alpha_ii_c, alpha_ij_c,
                                   alpha_ji_c, alpha_jj_c), byrow = TRUE, ncol = 2)
  colnames(interaction_matrix_c) <- c("p1", "p2")
  rownames(interaction_matrix_c) <- c("p1", "p2")
  
  # Due to microbes alone
  alpha_ii_m <- (0 - (m1A*vA1/qA + m1B*vB1/qB))
  alpha_ij_m <- (0 - (m1A*vA2/qA + m1B*vB2/qB))
  alpha_jj_m <- (0 - (m2A*vA2/qA + m2B*vB2/qB))
  alpha_ji_m <- (0 - (m2A*vA1/qA + m2B*vB1/qB))
  interaction_matrix_m <- matrix(c(alpha_ii_m, alpha_ij_m,
                                   alpha_ji_m, alpha_jj_m), byrow = TRUE, ncol = 2)
  colnames(interaction_matrix_m) <- c("p1", "p2")
  rownames(interaction_matrix_m) <- c("p1", "p2")
  
  return(list(interaction_matrix = interaction_matrix,
              interaction_matrix_c = interaction_matrix_c,
              interaction_matrix_m = interaction_matrix_m))
  
}

# calculate the niche overlap given a matrix of alphas
calculate_rho <- function(alpha_matrix){
  niche_difference <- sqrt((alpha_matrix["p1","p2"]*alpha_matrix["p2","p1"])/
         (alpha_matrix["p1","p1"]*alpha_matrix["p2","p2"]))
  niche_difference
}

# calculate fitness ratio Kj/Ki given a matrix of alphas
calculate_fitness_ratio <- function(alpha_matrix, intrinsic_growths) {

  fitness_ratio <- sqrt((alpha_matrix["p1","p1"]*alpha_matrix["p1","p2"])/
                                  (alpha_matrix["p2","p2"]*alpha_matrix["p2","p1"]))
  
  fitness_ratio
}

# determine whether or not the plants with coexist 
coex_outcome <- function(niche_diff, fitness_diff) {
  if (niche_diff < fitness_diff & fitness_diff < 1/niche_diff) {
    "coexist"
  } else if (niche_diff > fitness_diff & fitness_diff > 1/niche_diff) {
    "priority effect"
  }
  else {
    "exclusion"
  }
}

# A function to generate trajectory of the system
# this gets used by a call to deSolve::ode()
modelv1.0 <-  function(t, y, parms) {
  
  with(as.list(c(y, parms)),{
    
    dNi <- g1*(1 - c11*Ni - c12*Nj + m1A*Sk + m1B*Sl)*Ni
    dNj <- g2*(1 - c22*Nj - c21*Ni + m2A*Sk + m2B*Sl)*Nj
    
    dSk <- (vA1*Ni + vA2*Nj - qA*Sk)*Sk
    dSl <- (vB1*Ni + vB2*Nj - qB*Sl)*Sl
    
    
    list(c(dNi,dNj,dSk,dSl))
  })
  
}

# a master-function that uses all of the functions above to analyze the system
predict_interaction_outcome <- function(g1, g2, 
                                        c11, c12, c21, c22,
                                        m1A, m1B, m2A, m2B, 
                                        vA1, vA2, vB1, vB2,
                                        qA, qB,
                                        make_trajectory = TRUE) {
  
  intrinsic_growths <- lowdens_grs(g1, g2)
  alpha_matrices <- calculate_alphas(g1, g2, c11, c12, c21, c22,
                                     m1A, m1B, m2A, m2B, 
                                     vA1, vA2, vB1, vB2,
                                     qA, qB)
  
  rho <-  calculate_rho(alpha_matrix = alpha_matrices$interaction_matrix)
  
  rho_comp <- calculate_rho(alpha_matrix = alpha_matrices$interaction_matrix_c)
  rho_micr <- calculate_rho(alpha_matrix = alpha_matrices$interaction_matrix_m)
  
  
  fitness_ratio <- calculate_fitness_ratio(alpha_matrix = alpha_matrices$interaction_matrix, 
                                           intrinsic_growths = intrinsic_growths)
  fitness_ratio_comp <- calculate_fitness_ratio(alpha_matrix = alpha_matrices$interaction_matrix_c, 
                                                intrinsic_growths = intrinsic_growths)
  
  fitness_ratio_micr <- calculate_fitness_ratio(alpha_matrix = alpha_matrices$interaction_matrix_m, 
                                                intrinsic_growths = intrinsic_growths)
  
  predicted_outcome <- coex_outcome(niche_diff = rho, fitness_diff = fitness_ratio)
  predicted_outcome_micr <- coex_outcome(niche_diff = rho_micr, fitness_diff = fitness_ratio_micr)
  predicted_outcome_comp <- coex_outcome(niche_diff = rho_comp, fitness_diff = fitness_ratio_comp)
  
  bevers_I <- round(m1A+m2B-m1B-m2A, digits = 5)
  bevers_stabilization = -0.5*bevers_I
  bevers_fitdiff <- round(0.5*(m1A+m1B-m2A-m2B), digits = 5)
  
  parameter_vector <- c(g1 = g1, g2 = g2, 
                        c11 = c11, c12 = c12, c21 = c21, c22 = c22, 
                        m1A = m1A, m1B = m1B, m2A = m2A, m2B = m2B, 
                        vA1 = vA1, vA2 = vA2, vB1 = vB1, vB2 = vB2,
                        qA = qA, qB = qB)
  if(make_trajectory == TRUE) {
    trajectory <-  data.frame(lsoda(y = init, times = times, 
                                    func = modelv1.0, parms = parameter_vector)) %>% 
      select(-Sk,-Sl) %>% gather(., species, size, -time)
  } else {
    trajectory <- "not generated"
  }
  to_return <- list(intrinsic_growths = intrinsic_growths,
                    alpha_matrix = alpha_matrices$interaction_matrix,
                    alpha_matrix_c = alpha_matrices$interaction_matrix_c,
                    alpha_matrix_m = alpha_matrices$interaction_matrix_m,
                    trajectory = trajectory,
                    
                    rho = rho,
                    fitness_ratio = fitness_ratio,
                    fitness_ratio_comp = fitness_ratio_comp,
                    fitness_ratio_micr = fitness_ratio_micr,
                    coex_outcome = predicted_outcome,
                    bevers_I = bevers_I,
                    bevers_stabilization = bevers_stabilization,
                    bevers_fitdiff = bevers_fitdiff,
                    rho_comp = rho_comp, 
                    rho_micr = rho_micr)
  
  return(to_return)
}


# Multispecies competition model -----
# First, we define the functions used to generate Figure S4.1 
# in Appendix S4.
# calculate the low density of growth rates
f2_lowdens_grs <- function(g1, g2, 
                           m1A, m1B, m2A, m2B, m1C, m2C, 
                          qA, qB, qC, ...) {
  r_i <- g1
  r_j <- g2
  to_return <- c("r1" = r_i, "r2" = r_j)
  return(to_return)
}

# calculate the alphas matrix
f2_calculate_alphas <- function(g1, g2, 
                                c11, c12, c21, c22,
                                m1A, m1B, m2A, m2B, m1C, m2C,
                                vA1, vA2, vB1, vB2, vC1, vC2,
                                fA, fB, fC, qA, qB, qC) {
  
  plant_rs <- f2_lowdens_grs(g1, g2, m1A, m1B, m2A, m2B, m1C, m2C, fA, fB, qC, qA, qB, qC)
  
  alpha_ii <- (g1*(c11 - (m1A*vA1/qA + m1B*vB1/qB + m1C*vC1/qC)))/plant_rs["r1"]
  alpha_ij <- (g1*(c12 - (m1A*vA2/qA + m1B*vB2/qB + m1C*vC2/qC)))/plant_rs["r1"]
  alpha_jj <- (g2*(c22 - (m2A*vA2/qA + m2B*vB2/qB + m2C*vC2/qC)))/plant_rs["r2"]
  alpha_ji <- (g2*(c21 - (m2A*vA1/qA + m2B*vB1/qB + m2C*vC1/qC)))/plant_rs["r2"]
  interaction_matrix <- matrix(c(alpha_ii, alpha_ij,
                                 alpha_ji, alpha_jj), byrow = TRUE, ncol = 2)
  colnames(interaction_matrix) <- c("p1", "p2")
  rownames(interaction_matrix) <- c("p1", "p2")
  
  # Due to competition alone
  alpha_ii_c <- (g1*(c11 - (0)))/plant_rs["r1"]
  alpha_ij_c <- (g1*(c12 - (0)))/plant_rs["r1"]
  alpha_jj_c <- (g2*(c22 - (0)))/plant_rs["r2"]
  alpha_ji_c <- (g2*(c21 - (0)))/plant_rs["r2"]
  interaction_matrix_c <- matrix(c(alpha_ii_c, alpha_ij_c,
                                   alpha_ji_c, alpha_jj_c), byrow = TRUE, ncol = 2)
  colnames(interaction_matrix_c) <- c("p1", "p2")
  rownames(interaction_matrix_c) <- c("p1", "p2")
  
  # Due to microbes alone
  alpha_ii_m <- (g1*(0 - (m1A*vA1/qA + m1B*vB1/qB + m1C*vC1/qC)))/plant_rs["r1"]
  alpha_ij_m <- (g1*(0 - (m1A*vA2/qA + m1B*vB2/qB + m1C*vC2/qC)))/plant_rs["r1"]
  alpha_jj_m <- (g2*(0 - (m2A*vA2/qA + m2B*vB2/qB + m2C*vC2/qC)))/plant_rs["r2"]
  alpha_ji_m <- (g2*(0 - (m2A*vA1/qA + m2B*vB1/qB + m2C*vC1/qC)))/plant_rs["r2"]
  interaction_matrix_m <- matrix(c(alpha_ii_m, alpha_ij_m,
                                   alpha_ji_m, alpha_jj_m), byrow = TRUE, ncol = 2)
  colnames(interaction_matrix_m) <- c("p1", "p2")
  rownames(interaction_matrix_m) <- c("p1", "p2")
  
  return(list(interaction_matrix = interaction_matrix,
              interaction_matrix_c = interaction_matrix_c,
              interaction_matrix_m = interaction_matrix_m))
  
}


# write a function that will return the trajectory of the syste
# this gets used by a call to deSolve::ode()
f2_modelv1.0 <-  function(t, y, parms) {
  
  with(as.list(c(y, parms)),{
    
    dNi <- g1*(1 - c11*Ni - c12*Nj + m1A*Sk + m1B*Sm + m1C*Sb)*Ni
    dNj <- g2*(1 - c22*Nj - c21*Ni + m2A*Sk + m2B*Sm + m2C*Sb)*Nj
    
    dSk <- (vA1*Ni + vA2*Nj - qA*Sk)*Sk
    dSm <- (vB1*Ni + vB2*Nj - qB*Sm)*Sm
    dSb <- (vC1*Ni + vC2*Nj - qC*Sb)*Sb
    
    
    list(c(dNi,dNj,dSk,dSm,dSb))
  })
  
}

# a master-function that uses all of the functions above to analyze the system
f2_predict_interaction_outcome <- function(g1, g2, 
                                           c11, c12, c21, c22,
                                           m1A, m1B, m2A, m2B, m1C, m2C,
                                           vA1, vA2, vB1, vB2, vC1, vC2,
                                           fA, fB, fC, qA, qB, qC) {
  
  intrinsic_growths <- f2_lowdens_grs(g1, g2, 
                                      m1A, m1B, m2A, m2B, m1C, m2C, 
                                      fA, fB, fC, qC, qA, qB, qC)
  alpha_matrices <- f2_calculate_alphas(g1, g2, 
                                        c11, c12, c21, c22,
                                        m1A, m1B, m2A, m2B, m1C, m2C,
                                        vA1, vA2, vB1, vB2, vC1, vC2,
                                        fA, fB, fC, qA, qB, qC)
  
  rho <-  calculate_rho(alpha_matrix = alpha_matrices$interaction_matrix)
  
  rho_comp <- calculate_rho(alpha_matrix = alpha_matrices$interaction_matrix_c)
  rho_micr <- calculate_rho(alpha_matrix = alpha_matrices$interaction_matrix_m)
  
  
  fitness_ratio <- calculate_fitness_ratio(alpha_matrix = alpha_matrices$interaction_matrix, 
                                           intrinsic_growths = intrinsic_growths)
  predicted_outcome <- coex_outcome(niche_diff = rho, fitness_diff = fitness_ratio)
  bevers_I <- m1A+m2B-m1B-m2A
  
  parameter_vector <- c(g1 = g1, g2 = g2, 
                        c11 = c11, c12 = c12, c21 = c21, c22 = c22, 
                        m1A = m1A, m1B = m1B, m2A = m2A, m2B = m2B, m1C = m1C, m2C = m2C,
                        vA1 = vA1, vA2 = vA2, vB1 = vB1, vB2 = vB2, vC1 = vC1, vC2 = vC2,
                        qA = qA, qB = qA, qC = qC)
  trajectory <-  data.frame(lsoda(y = f2_init, times = times, 
                                  func = f2_modelv1.0, parms = parameter_vector)) %>% 
    select(-Sk,-Sm, -Sb) %>% gather(., species, size, -time)
  
  to_return <- list(intrinsic_growths = intrinsic_growths,
                    alpha_matrix = alpha_matrices$interaction_matrix,
                    alpha_matrix_c = alpha_matrices$interaction_matrix_c,
                    alpha_matrix_m = alpha_matrices$interaction_matrix_m,
                    
                    rho = rho,
                    fitness_ratio = fitness_ratio,
                    coex_outcome = predicted_outcome,
                    bevers_I = bevers_I,
                    trajectory = trajectory,
                    rho_comp = rho_comp, 
                    rho_micr = rho_micr)
  
  return(to_return)
}

# Now, we define the function used to simulate a three-species system
# I call this function model_RPS because it is used to simulate the
# rock-paper-scissors scenario.
model_RPS <-  function(t, y, parms) {
  
  with(as.list(c(y, parms)),{
    
    dN1 <- g1*(1 - c11*N1 - c12*N2 - c13*N3 + a1A*SA + a1B*SB + a1C*SC)*N1
    dN2 <- g2*(1 - c21*N1 - c22*N2 - c23*N3 + a2A*SA + a2B*SB + a2C*SC)*N2
    dN3 <- g3*(1 - c31*N1 - c32*N2 - c33*N3 + a3A*SA + a3B*SB + a3C*SC)*N3
    
    dSA <- (vA1*N1 + vA2*N2 + vA3*N3 - qA*SA)*SA
    dSB <- (vB1*N1 + vB2*N2 + vB3*N3 - qB*SB)*SB
    dSC <- (vC1*N1 + vC2*N2 + vC3*N3 - qC*SC)*SC
    
    
    list(c(dN1, dN2, dN3, dSA, dSB, dSC))
  })
  
}

# Resource competition model Model ----------
# Define functions that simulate dynamics and
# calculate the various metrics (e.g. alphas, rho, ..)
# for the RESOURCE COMPETITION MODEL
lowdens_grs_RC <- function(u1l, u2l, u1n, u2n,
                           el, en, alpha_l, alpha_n,
                           m1A, m2A, m1B, m2B, qA, qB,
                           mu1, mu2, ...) {
  r_1 <- u1l*el/alpha_l + u1n*en/alpha_n - mu1
  r_2 <- u2l*el/alpha_l + u2n*en/alpha_n - mu2
  to_return <- c("r1" = r_1, "r2" = r_2)
  return(to_return)
}

calculate_alphas_RC <- function(u1l, u2l, u1n, u2n,
                                el, en, alpha_l, alpha_n,
                                m1A, m2A, m1B, m2B, qA, qB,
                                vA1, vA2, vB1, vB2,
                                mu1, mu2, rl, rn, ...) {
  
  plant_rs <- lowdens_grs_RC(u1l, u2l, u1n, u2n,
                             el, en, alpha_l, alpha_n,
                             m1A, m2A, m1B, m2B, qA, qB,
                             mu1, mu2)
  # Due to both interactions
  alpha_11 <- ((u1l*u1l*el)/(alpha_l*rl) + (u1n*u1n*en)/(alpha_n*rn) - (m1A*vA1/qA) - (m1B*vB1/qB)) / plant_rs["r1"]
  alpha_12 <- ((u1l*u2l*el)/(alpha_l*rl) + (u1n*u2n*en)/(alpha_n*rn) - (m1A*vA2/qA) - (m1B*vB2/qB)) / plant_rs["r1"]
  alpha_21 <- ((u2l*u1l*el)/(alpha_l*rl) + (u2n*u1n*en)/(alpha_n*rn) - (m2A*vA1/qA) - (m2B*vB1/qB)) / plant_rs["r2"]
  alpha_22 <- ((u2l*u2l*el)/(alpha_l*rl) + (u2n*u2n*en)/(alpha_n*rn) - (m2A*vA2/qA) - (m2B*vB2/qB)) / plant_rs["r2"]
  
  interaction_matrix <- matrix(c(alpha_11, alpha_12,
                                 alpha_21, alpha_22), byrow = TRUE, ncol = 2)
  colnames(interaction_matrix) <- c("p1", "p2")
  rownames(interaction_matrix) <- c("p1", "p2")
  
  # Due to competition alone
  alpha_11_c <- ((u1l*u1l*el)/(alpha_l*rl) + (u1n*u1n*en)/(alpha_n*rn) - 0) / plant_rs["r1"]
  alpha_12_c <- ((u1l*u2l*el)/(alpha_l*rl) + (u1n*u2n*en)/(alpha_n*rn) - 0) / plant_rs["r1"]
  alpha_21_c <- ((u2l*u1l*el)/(alpha_l*rl) + (u2n*u1n*en)/(alpha_n*rn) - 0) / plant_rs["r2"]
  alpha_22_c <- ((u2l*u2l*el)/(alpha_l*rl) + (u2n*u2n*en)/(alpha_n*rn) - 0) / plant_rs["r2"]
  
  interaction_matrix_c <- matrix(c(alpha_11_c, alpha_12_c,
                                   alpha_21_c, alpha_22_c), byrow = TRUE, ncol = 2)
  colnames(interaction_matrix_c) <- c("p1", "p2")
  rownames(interaction_matrix_c) <- c("p1", "p2")
  
  # Due to microbes alone
  alpha_11_m <- (0 - (m1A*vA1/qA) - (m1B*vB1/qB)) / plant_rs["r1"]
  alpha_12_m <- (0 - (m1A*vA2/qA) - (m1B*vB2/qB)) / plant_rs["r1"]
  alpha_21_m <- (0 - (m2A*vA1/qA) - (m2B*vB1/qB)) / plant_rs["r2"]
  alpha_22_m <- (0 - (m2A*vA2/qA) - (m2B*vB2/qB)) / plant_rs["r2"]
  
  interaction_matrix_m <- matrix(c(alpha_11_m, alpha_12_m,
                                   alpha_21_m, alpha_22_m), byrow = TRUE, ncol = 2)
  colnames(interaction_matrix_m) <- c("p1", "p2")
  rownames(interaction_matrix_m) <- c("p1", "p2")
  
  return(list(interaction_matrix = interaction_matrix,
              interaction_matrix_c = interaction_matrix_c,
              interaction_matrix_m = interaction_matrix_m))
}

# Niche difference and fitness difference is calculated in the same way as above

# Define the "master function" that uses the functions above
# NOTE that this master function does not simulate dynamics.

predict_interaction_outcome_RC <- function(u1l, u2l, u1n, u2n,
                                           el, en, alpha_l, alpha_n,
                                           m1A, m2A, m1B, m2B, qA, qB,
                                           vA1, vA2, vB1, vB2,
                                           mu1, mu2, rl, rn, ...) {
  
  plant_rs <- lowdens_grs_RC(u1l, u2l, u1n, u2n,
                             el, en, alpha_l, alpha_n,
                             m1A, m2A, m1B, m2B, qA, qB,
                             mu1, mu2)
  alpha_matrices <- calculate_alphas_RC(u1l, u2l, u1n, u2n,
                                        el, en, alpha_l, alpha_n,
                                        m1A, m2A, m1B, m2B, qA, qB,
                                        vA1, vA2, vB1, vB2,
                                        mu1, mu2, rl, rn)
  
  rho <- calculate_rho(alpha_matrix = alpha_matrices$interaction_matrix)
  rho_comp <- calculate_rho(alpha_matrix = alpha_matrices$interaction_matrix_c)
  rho_micr <- calculate_rho(alpha_matrix = alpha_matrices$interaction_matrix_m)
  
  fitness_ratio <- calculate_fitness_ratio(alpha_matrix = alpha_matrices$interaction_matrix, 
                                              intrinsic_growths = plant_rs)
  fitness_ratio_comp <- calculate_fitness_ratio(alpha_matrix = alpha_matrices$interaction_matrix_c, 
                                                   intrinsic_growths = plant_rs)
  fitness_ratio_micr <- calculate_fitness_ratio(alpha_matrix = alpha_matrices$interaction_matrix_m, 
                                                   intrinsic_growths = plant_rs)
  
  predicted_outcome <- coex_outcome(niche_diff = rho, fitness_diff = fitness_ratio)
  to_return <- list(intrinsic_growths = plant_rs,
                    alpha_matrix = alpha_matrices$interaction_matrix,
                    alpha_matrix_c = alpha_matrices$interaction_matrix_c,
                    alpha_matrix_m = alpha_matrices$interaction_matrix_m,
                    rho = rho,
                    fitness_ratio = fitness_ratio,
                    fitness_ratio_comp = fitness_ratio_comp,
                    fitness_ratio_micr = fitness_ratio_micr,
                    coex_outcome = predicted_outcome,
                    rho_comp = rho_comp, 
                    rho_micr = rho_micr)
  return(to_return)
}


