library(tidyverse)
library(patchwork)
library(deSolve)
source("../figures_and_tables/core-functions.R")
scenario_1_parameters <- c(g1 = 1, g2 = 1,
                           c11 = 0.003, c12 = 0.0024,
                           c21 = 0.002, c22 = 0.004,
                           m1A = -0.05, m1B = -0.04,
                           m2A = -0.01, m2B = -0.021,
                           vA1 = 0.005, vA2 = 0, vB1 = 0, vB2 = 0.005,
                           qA = 0.01, qB = 0.01)

scenario_2_parameters <- c(g1 = 1, g2 = 1,
                           c11 = 0.0005, c12 = 0.001, 
                           c21 = 0.001, c22 = 0.002,
                           m1A = -.025, m1B = -.025,
                           m2A = -.01, m2B = -.01,
                           vA1 = 0.005, vA2 = 0, vB1 = 0, vB2 = 0.005,
                           qA = 0.1, qB = 0.1)
times <- seq(from = 0, to = 85, by = 0.1)


server <- function(input, output, session) {
  
  # Reset to Scenario 1 ----
  observeEvent(input$scenario1_reset, {
    updateSliderInput(session, "m1A", value = unname(scenario_1_parameters["m1A"]))
    updateSliderInput(session, "m1B", value = unname(scenario_1_parameters["m1B"]))
    updateSliderInput(session, "m2A", value = unname(scenario_1_parameters["m2A"]))
    updateSliderInput(session, "m2B", value = unname(scenario_1_parameters["m2B"]))
    
    updateSliderInput(session, "c11", value = unname(scenario_1_parameters["c11"]))
    updateSliderInput(session, "c12", value = unname(scenario_1_parameters["c12"]))
    updateSliderInput(session, "c21", value = unname(scenario_1_parameters["c21"]))
    updateSliderInput(session, "c22", value = unname(scenario_1_parameters["c22"]))

    updateSliderInput(session, "qA", value = unname(scenario_1_parameters["qA"]))
    updateSliderInput(session, "qB", value = unname(scenario_1_parameters["qB"]))
    
    updateSliderInput(session, "vA1", value = unname(scenario_1_parameters["vA1"]))
    updateSliderInput(session, "vB2", value = unname(scenario_1_parameters["vB2"]))
    
    updateSliderInput(session, "g1", value = unname(scenario_1_parameters["g1"]))
    updateSliderInput(session, "g2", value = unname(scenario_1_parameters["g2"]))
  })
  
  # Reset to Scenario 2 ----
  observeEvent(input$scenario2_reset, {
    updateSliderInput(session, "m1A", value = unname(scenario_2_parameters["m1A"]))
    updateSliderInput(session, "m1B", value = unname(scenario_2_parameters["m1B"]))
    updateSliderInput(session, "m2A", value = unname(scenario_2_parameters["m2A"]))
    updateSliderInput(session, "m2B", value = unname(scenario_2_parameters["m2B"]))
    
    updateSliderInput(session, "c11", value = unname(scenario_2_parameters["c11"]))
    updateSliderInput(session, "c12", value = unname(scenario_2_parameters["c12"]))
    updateSliderInput(session, "c21", value = unname(scenario_2_parameters["c21"]))
    updateSliderInput(session, "c22", value = unname(scenario_2_parameters["c22"]))
    
    updateSliderInput(session, "qA", value = unname(scenario_2_parameters["qA"]))
    updateSliderInput(session, "qB", value = unname(scenario_2_parameters["qB"]))
    
    updateSliderInput(session, "vA1", value = unname(scenario_2_parameters["vA1"]))
    updateSliderInput(session, "vB2", value = unname(scenario_2_parameters["vB2"]))
    
    updateSliderInput(session, "g1", value = unname(scenario_2_parameters["g1"]))
    updateSliderInput(session, "g2", value = unname(scenario_2_parameters["g2"]))
  })
  
  # Set parameter values -----
  current_parameters <- reactive({
    c(g1 = input$g1, g2 = input$g2, 
      c11 = input$c11, c12 = input$c12, 
      c21 = input$c21, c22 = input$c22,
      m1A = input$m1A, m1B = input$m1B, 
      m2A = input$m2A, m2B = input$m2B, 
      vA1 = input$vA1, vA2 = 0, 
      vB1 = 0, vB2 = input$vB2,
      qA = input$qA, qB = input$qB)
  })
  
  # Scenario outcomes ----
  scenario_1_outcome <- reactive({
    do.call(predict_interaction_outcome, as.list(current_parameters()))
  })
  
  # Make plot ------
  plot_df <- reactive({
    data.frame(nd = c(1-scenario_1_outcome()$rho,
                      1-scenario_1_outcome()$rho_micr,
                      1-scenario_1_outcome()$rho_comp),
               fd = c(scenario_1_outcome()$fitness_ratio,
                      scenario_1_outcome()$fitness_ratio_micr,
                      scenario_1_outcome()$fitness_ratio_comp),
               outcome = c(scenario_1_outcome()$coex_outcome,
                           scenario_1_outcome()$coex_outcome_micr,
                           scenario_1_outcome()$coex_outcome_comp),
               which = c("Net outcome", "Microbes Only", "Competition Only"))
  })  
  
  
  output$cone <- renderPlot({
    coex_cone_truncated + 
      geom_point(data = plot_df(), aes(x = nd, y = fd, fill = factor(which), size = which),
                 pch = 21, stroke = 1.2) + 
      scale_fill_manual(values = c("#CC79A7", "#CC79A7", "#F0E442")) +
      scale_size_manual(values = c(3,3,6)) + 
      geom_text(data = plot_df(), aes(x = nd +.01, y = fd, label = which, color = outcome), hjust = 0,
                size = c(7,4,4), fontface = "bold.italic") +
      scale_color_manual(values = c("white", "black")) + 
      theme(legend.position = "none")
  })
  
  output$table1 <- renderTable({plot_df()})
  
}