library(tidyverse)
library(patchwork)
library(deSolve)
source("core-functions-copy.R")
source("twosp_functions.R")
source("threesp_functions.R")

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
  observeEvent(input$scenario3.2_reset, {
    source("reset_to_s3.2.R", local = TRUE)
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
  
  
  # Resource competition panel --------
  # Set parameter values
  current_parameters_RC <- reactive({
    c(u1l = input$u1l_RC, u2l = input$u2l_RC, 
      u1n = input$u1n_RC, u2n = input$u2n_RC, 
      m1A = input$m1A_RC, m1B = input$m1B_RC, 
      m2A = input$m2A_RC, m2B = input$m2B_RC, 
      vA1 = input$vA1_RC, vA2 = 0, 
      vB1 = 0, vB2 = input$vB2_RC,
      qA = input$qA_RC, qB = input$qB_RC,
      mu1 = input$mu1_RC, mu2 = input$mu2_RC,
      rl = input$rl_RC, rn = input$rn_RC,
      s_l = input$sl_RC, s_n = input$sn_RC
      )
  })
  current_outcome_RC <- reactive({
    do.call(predict_interaction_outcome_RC, as.list(current_parameters_RC()))
  })
  
  # Two species panel: Make plot ------
  plot_df_RC <- reactive({
    data.frame(nd = c(1-current_outcome_RC()$rho,
                      1-current_outcome_RC()$rho_micr,
                      1-current_outcome_RC()$rho_comp),
               fd = c(current_outcome_RC()$fitness_ratio,
                      current_outcome_RC()$fitness_ratio_micr,
                      current_outcome_RC()$fitness_ratio_comp),
               outcome = c(current_outcome_RC()$coex_outcome,
                           current_outcome_RC()$coex_outcome_micr,
                           current_outcome_RC()$coex_outcome_comp),
               which = c("Net outcome", "Microbes Only", "Competition Only"))
  })  
  output$resourcomp_cone <- renderPlot({
    twospecies_cone(plot_df_RC()) 
  })
  
}