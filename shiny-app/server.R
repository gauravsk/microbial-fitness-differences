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
  
  
  
  output$cone <- renderPlot({
    
    plot_df <- data.frame(nd = c(scenario_1_outcome()$rho,
                                scenario_1_outcome()$rho_micr,
                                scenario_1_outcome()$rho_comp),
                          fd = c(scenario_1_outcome()$fitness_ratio,
                                 scenario_1_outcome()$fitness_ratio_micr,
                                 scenario_1_outcome()$fitness_ratio_comp),
                          outcome = c(scenario_1_outcome()$coex_outcome))
    
    coex_cone_truncated + 
      geom_point(data = plot_df, aes(x = nd, y = fd))
    # p2 <- make_coex_cone_w_scenarios(scenario_list(),
    #                            scenario_names = c("Net outcome",
    #                                               "Competition only", "Microbes only"),
    #                            lab_adj = .06) +
    #   scale_fill_manual(values = c( "#CC79A7", "#CC79A7", "#F0E442")) +
    #   scale_y_log10(limits = c(10^-1, 10^1)) +
    #   scale_x_continuous(limits = c(0, 1))
    # p1 + p2
  })
  
  output$table <- renderTable({
    rho_vec <- c(map_dbl(scenario_list(), "rho"),
                 map_dbl(scenario_list, "rho_comp"),
                 map_dbl(scenarios_1and2, "rho_micr"))
    
    
  })
  
}