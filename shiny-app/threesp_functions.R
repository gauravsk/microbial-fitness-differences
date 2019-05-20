# Defines functions used to make plots on the three species pane

make_twosp_plots <- function(df) {
  ggplot(df) +
  geom_line(aes(y = size, x = time, color = species),
            size = 1) +
    ylab("Pop. size") +
    xlab("") +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(df$size)*1.05)) + 
    theme_gsk() +
    theme(legend.position = "none") + 
    NULL
}


make_threesp_plots <- function(initvals, times, parameters) {
  
  rps_proj <- data.frame(lsoda(y = initvals, times = times, 
                               func = model_RPS, parms = parameters)) %>% 
    select(-SA, -SB, -SC) %>% gather(., species, size, -time)
  yn1 <- filter(rps_proj, species == "N1") %>% 
    select(size) %>% tail(., n = 1) %>% unlist %>% unname
  yn2 <- filter(rps_proj, species == "N2") %>% 
    select(size) %>% tail(., n = 1) %>% unlist %>% unname
  yn3 <- filter(rps_proj, species == "N3") %>% 
    select(size) %>% tail(., n = 1) %>% unlist %>% unname
  
  all_three <- ggplot(rps_proj) +
    geom_line(aes(y = size, x = time, color = species), 
              size = 1) + 
    scale_color_manual(values = c("#000000", "#D55E00", "#0072B2")) + 
    ylab("Population size") + 
    geom_text(aes(y = max(size)* 1.05, x = 10,  label="")) +
    theme_gsk() + 
    scale_y_continuous(expand = c(0, 0))  + 
    scale_x_continuous(expand = c(0, 0)) + 
    labs(subtitle = "All three plants present in community") +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 12),
          axis.title = element_text(size = 15)) + 
    annotate("text", x = 1100, y = yn1*1.05, label = latex2exp::TeX("N_1"), size = 6,
             col = "#000000") + 
    annotate("text", x = 900, y = yn2*1.05, label = latex2exp::TeX("N_2"), size = 6,
             col = "#D55E00") +
    annotate("text", x = 750, y = yn3*1.05, label = latex2exp::TeX("N_3"), size = 6,
             col = "#0072B2") 
    
  
  init_sp1 <- initvals
  init_sp1["N1"] <- 0
  rps_proj_1 <- data.frame(lsoda(y = init_sp1, times = times,
                                 func = model_RPS, parms = parameters)) %>%
    select(-SA, -SB, -SC, -N1) %>% gather(., species, size, -time)
  
  init_sp2 <- initvals
  init_sp2["N2"] <- 0
  
  rps_proj_2 <- data.frame(lsoda(y = init_sp2, times = times,
                                 func = model_RPS, parms = parameters)) %>%
    select(-SA, -SB, -SC, -N2) %>% gather(., species, size, -time)
  
  init_sp3 <- initvals
  init_sp3["N3"] <- 0
  rps_proj_3 <- data.frame(lsoda(y = init_sp3, times = times,
                                 func = model_RPS, parms = parameters))  %>%
    select(-SA, -SB, -SC, -N3) %>% gather(., species, size, -time)
  
  
  sp1_missing <- make_twosp_plots(rps_proj_1) +
    scale_color_manual(values = c("#D55E00", "#0072B2")) + 
    labs(subtitle = "Species 1 absent")
    
  sp2_missing <- make_twosp_plots(rps_proj_2) +
    scale_color_manual(values = c("#000000",  "#0072B2")) +
    labs(subtitle = "Species 2 absent")
    
  sp3_missing <- make_twosp_plots(rps_proj_3) + 
    scale_color_manual(values = c("#000000", "#D55E00")) +
    labs(subtitle = "Species 3 absent")
    
    
  
  figure_3 <- all_three + 
  {sp1_missing/sp2_missing/sp3_missing} +
    plot_layout(widths = c(3, 1)) +
    NULL
  
  return(figure_3)
}


# For CONE ---------

make_3sp_cone <- function(parameters) {
  # print(scenario_3_parameters)
  pair_12 <-  c(g1 = parameters["g1"], g2 = parameters["g2"],
                c11 = parameters["c11"], c12 = parameters["c12"],
                c21 = parameters["c21"], c22 = parameters["c22"],
                
                m1A = parameters["m1A"], m1B = parameters["m1B"],
                m2A = parameters["m2A"], m2B = parameters["m2B"],
                
                vA1 = parameters["vA1"], vB1 = 0,
                vA2 = 0, vB2 = parameters["vB2"],
                qA = parameters["qA"], qB = parameters["qB"])
  
  
  pair_13 <-  c(g1 = parameters["g1"], g2 = parameters["g3"],
                c11 = parameters["c11"], c12 = parameters["c13"],
                c21 = parameters["c31"], c22 = parameters["c33"],
                
                m1A = parameters["m1A"], m1B = parameters["m1C"],
                m2A = parameters["m3A"], m2B = parameters["m3C"],
                
                vA1 = parameters["vA1"], vB1 = 0,
                vA2 = 0, vB2 = parameters["vC3"],
                qA = parameters["qA"], qB = parameters["qC"])

  pair_23 <-  c(g1 = parameters["g2"], g2 = parameters["g3"],
                c11 = parameters["c22"], c12 = parameters["c23"],
                c21 = parameters["c32"], c22 = parameters["c33"],

                m1A = parameters["m2B"], m1B = parameters["m2C"],
                m2A = parameters["m3B"], m2B = parameters["m3C"],

                vA1 = parameters["vB2"], vB1 = 0,
                vA2 = 0, vB2 = parameters["vC3"],
                qA = parameters["qB"], qB = parameters["qC"])
  
  namevec <- c("g1", "g2", "c11", "c12", "c21", "c22", 
               "m1A", "m1B", "m2A", "m2B", "vA1", "vB1", "vA2", "vB2", 
               "qA", "qB")
  
  names(pair_12) <- namevec
  names(pair_13) <- namevec
  names(pair_23) <- namevec
  
  pair_12_outcome <- do.call(predict_interaction_outcome, as.list(pair_12))
  pair_13_outcome <- do.call(predict_interaction_outcome, as.list(pair_13))
  pair_23_outcome <- do.call(predict_interaction_outcome, as.list(pair_23))

  p <- make_coex_cone_w_scenarios(which_base = coex_cone_for_multisp,
                             list_of_scenarios = list(pair_12_outcome, pair_13_outcome, pair_23_outcome),
                             scenario_names = c("S3, i,j = 1,2",
                                                "S3, i,j = 1,3",
                                                "S3, i,j = 2,3"),
                             lab_adj = 0.02) +
    # scale_color_manual(values = c("white", "black"))
    NULL
  
  if (pair_12_outcome$coex_outcome == "coexist" & 
           pair_13_outcome$coex_outcome == "coexist" & 
           pair_23_outcome$coex_outcome == "coexist") {
    p <- p + scale_color_manual(values = "white")
  }

  if (pair_12_outcome$coex_outcome == "exclusion" & 
           pair_13_outcome$coex_outcome == "exclusion" & 
           pair_23_outcome$coex_outcome == "exclusion") {
    p <- p + scale_color_manual(values = "black")
  }
  p
  }

