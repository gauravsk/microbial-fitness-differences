library(tidyverse)
library(patchwork)
library(deSolve)
source("core-functions-copy.R")
source("twosp_functions.R")
source("threesp_functions.R")

scenario_1_parameters <- c(g1 = 1, g2 = 1,
                           c11 = 0.003, c12 = 0.0024,
                           c21 = 0.002, c22 = 0.004,
                           m1A = -0.05, m1B = -0.04,
                           m2A = -0.01, m2B = -0.021,
                           vA1 = 0.005, vA2 = 0, vB1 = 0, vB2 = 0.005,
                           qA = 0.01, qB = 0.01, for_app = TRUE)

scenario_2_parameters <- c(g1 = 1, g2 = 1,
                           c11 = 0.0005, c12 = 0.001, 
                           c21 = 0.001, c22 = 0.002,
                           m1A = -.025, m1B = -.025,
                           m2A = -.01, m2B = -.01,
                           vA1 = 0.005, vA2 = 0, vB1 = 0, vB2 = 0.005,
                           qA = 0.1, qB = 0.1, for_app = TRUE)


scenario_3_parameters <- c(g1 = .2, g2 = .2, g3 = .2,   
                      ## COMPETITION
                      c11 = .001, c12 = .001, c13 = .001,
                      c21 = .001, c22 = .001, c23 = .001,
                      c31 = .001, c32 = .001, c33 = .001,
                      
                      # MICROBE EFFECTS
                      m1A = -0.012/2, m1B = -0.01/2,  m1C = -0.03/2.25,
                      m2A = -0.013/2, m2B = -0.017/2, m2C = -0.015/2,
                      m3A = -.0075/2, m3B = -0.018/2, m3C = -0.02/2,
                      
                      
                      ## MICROBE INTRINSIC
                      qA = .01, qB = .01, qC = .01,
                      
                      ## PLANT EFFECTS ON MICROBES
                      vA1 = 0.01, vB1 = 0, vC1 = 0,
                      vA2 = 0, vB2 = 0.01, vC2 = 0,
                      vA3 = 0, vB3 = 0, vC3 = 0.01)



times <- seq(from = 0, to = 85, by = 0.1)

init_3sp <- c("N1" = 50, "N2" = 20, "N3" = 10,
          "SA" = 10, "SB" = 10, "SC" = 10)

server <- function(input, output, session) {
  
  # Two species panel: Reset to Scenario 1 ----
  observeEvent(input$scenario1_reset, {
    source("reset_to_s1.R", local = TRUE)
  })
  
  # Two species panel: Reset to Scenario 2 ----
  observeEvent(input$scenario2_reset, {
    source("reset_to_s2.R", local = TRUE)
  })
  
  # Two species panel: Set parameter values -----
  current_parameters <- reactive({
    c(g1 = input$g1, g2 = input$g2, 
      c11 = input$c11, c12 = input$c12, 
      c21 = input$c21, c22 = input$c22,
      m1A = input$m1A, m1B = input$m1B, 
      m2A = input$m2A, m2B = input$m2B, 
      vA1 = input$vA1, vA2 = 0, 
      vB1 = 0, vB2 = input$vB2,
      qA = input$qA, qB = input$qB,
      for_app = TRUE)
  })
  
  # Two species panel: Scenario outcomes ----
  current_outcome <- reactive({
    do.call(predict_interaction_outcome, as.list(current_parameters()))
  })
  
  # Two species panel: Make plot ------
  plot_df <- reactive({
    data.frame(nd = c(1-current_outcome()$rho,
                      1-current_outcome()$rho_micr,
                      1-current_outcome()$rho_comp),
               fd = c(current_outcome()$fitness_ratio,
                      current_outcome()$fitness_ratio_micr,
                      current_outcome()$fitness_ratio_comp),
               outcome = c(current_outcome()$coex_outcome,
                           current_outcome()$coex_outcome_micr,
                           current_outcome()$coex_outcome_comp),
               which = c("Net outcome", "Microbes Only", "Competition Only"))
  })  
  
  
  output$twosp_cone <- renderPlot({
    twospecies_cone(plot_df())
  })
  
  output$twosp_traj <- renderPlot({
    twospecies_trajectory(current_outcome()$trajectory)
  })
  
  # Three species panel -------
  observeEvent(input$scenario3_reset, {
    source("reset_to_s3.R", local = TRUE)
  })
  times <- seq(from = 0, to = 1200, by = .2)
  
  current_parameters_3sp <- reactive({    
    c(g1 = input$g1_3,g2 = input$g2_3,g3 = input$g3_3,
      c11 = input$c11_3,c12 = input$c12_3,c13 = input$c13_3,
      c21 = input$c21_3,c22 = input$c22_3,c23 = input$c23_3,
      c31 = input$c31_3,c32 = input$c32_3,c33 = input$c33_3,
      m1A = input$m1A_3,m1B = input$m1B_3,m1C = input$m1C_3,
      m2A = input$m2A_3,m2B = input$m2B_3,m2C = input$m2C_3,
      m3A = input$m3A_3,m3B = input$m3B_3,m3C = input$m3C_3,
      
      vA1 = input$vA1_3, vA2 = 0, vA3 = 0,
      vB1 = 0, vB2 = input$vB2_3, vB3 = 0,
      vC1 = 0, vC2 = 0, vC3 = input$vC3_3,
      
      qA = input$qA_3,qB = input$qB_3,qC = input$qC_3)
  })
 
  output$threesp_traj <- renderPlot({
    make_threesp_plots(initvals = init_3sp, times = times, parameters = current_parameters_3sp())
  })
  
  output$threesp_cone <- renderPlot({
    make_3sp_cone(parameters = current_parameters_3sp())
  })
  
}