library(tidyverse)
library(patchwork)
library(deSolve)
source("../figures_and_tables/core-functions.R")

times <- seq(from = 0, to = 85, by = 0.1)


server <- function(input, output) {
  
  scenario_1_parameters <- reactive({
    c(g1 = input$g1, g2 = input$g2, 
      c11 = input$c11, c12 = input$c12, 
      c21 = input$c21, c22 = input$c22,
      m1A = input$m1A, m1B = input$m1B, 
      m2A = input$m2A, m2B = input$m2B, 
      vA1 = input$vA1, vA2 = 0, 
      vB1 = 0, vB2 = input$vB2,
      qA = input$qA, qB = input$qB)
  })
  scenario_1_outcome <- reactive({
    do.call(predict_interaction_outcome, as.list(scenario_1_parameters()))
  })
  
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