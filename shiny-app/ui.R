library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Coexistence with competition and plant-soil feedbacks"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Two species system", tabName = "twosp", icon = icon("seedling")),
      menuItem("Three species system", tabName = "threesp", icon = icon("seedling")),
      menuItem("Resource competition model", tabName = "resourceComp", icon = icon("seedling"))
      
    )
  ),

      
  dashboardBody(
    tabItems(
      # First tab content -----
      tabItem(tabName = "twosp",
              h2("Dynamics of a system with two plant species (phenomenological competition)"),
              (includeMarkdown("docs/test.md")),
              # First row ----
              fluidRow(box(title = "Note: app under development as of 13 May 2019",
                           actionButton("scenario1_reset", label = "Reset to Scenario 1"),
                           actionButton("scenario2_reset", label = "Reset to Scenario 2"))),
              
              # Second row -----
              fluidRow(
                
                # Box of Intrinsic rates ---------
                box(width = 4,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Intrinsic rates and plant effects on microbes",
                    column(4,
                           sliderInput(inputId = "g1",
                                       label = withMathJax("Intrinsic growth of species 1 (\\(g_1\\))"),
                                       min = 0.1,
                                       max = 2,
                                       value = 1),
                           sliderInput(inputId = "g2",
                                       label = withMathJax("Intrinsic growth of species 1 (\\(g_2\\))"),
                                       min = 0.1,
                                       max = 2,
                                       value = 1)),
                    column(4,
                           sliderInput(inputId = "vA1",
                                       label = withMathJax("Effect of species 1 on microbes A (\\(v_{A1}\\))"),
                                       min = 0.0001,
                                       max = 0.01,
                                       value = 0.005),
                           sliderInput(inputId = "vB2",
                                       label = withMathJax("Effect of species 2 on microbes B (\\(v_{B2}\\))"),
                                       min = 0.0001,
                                       max = 0.01,
                                       value = 0.005)
                    ),
                    column(4,
                           sliderInput(inputId = "qA",
                                       label = withMathJax("Mortality rates of microbes A \\((q_A\\))"),
                                       min = 0.0001,
                                       max = 0.1,
                                       value = .01),
                           sliderInput(inputId = "qB",
                                       label = withMathJax("Mortality rates of microbes B \\((q_B\\))"),
                                       min = 0.0001,
                                       max = 0.1,
                                       value = .01))
                ),
                # Box of competition parameters -------
                box(width = 4,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Intra- and inter-specific competition parameters",
                    column(6,
                           sliderInput(inputId = "c11",
                                       label = withMathJax("Intraspecific competition- species 1 (\\(c_{11}\\))"),
                                       min = 0.0001,
                                       max = 0.01,
                                       value = .003, step = .0001),
                           sliderInput(inputId = "c12",
                                       label = withMathJax("Interspecific competition- effect of species 2 on species 1 (\\(c_{12}\\))"),
                                       min = 0.0001,
                                       max = 0.01,
                                       value = .0024, step = .0001)
                    ),
                    column(6,
                           sliderInput(inputId = "c21",
                                       label = withMathJax("Interspecific competition- effect of species 1 on species 2 (\\(c_{21}\\))"),
                                       min = 0.0001,
                                       max = 0.01,
                                       value = .002, step = .0001),
                           sliderInput(inputId = "c22",
                                       label = withMathJax("Intraspecific competition- species 2 (\\(c_{22}\\))"),
                                       min = 0.0001,
                                       max = 0.01,
                                       value = .004, step = .0001)
                    )),
                
                # Box of Microbe effects on plants ------
                box(width = 4,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Microbe effects on plants",
                    column(6,
                           sliderInput(inputId = "m1A",
                                       label = withMathJax("Effect of microbes A on species 1 (\\(m_{1A}\\))"),
                                       min = -0.1,
                                       max = 0.1,
                                       value = -0.05),
                           sliderInput(inputId = "m1B",
                                       label = withMathJax("Effect of microbes B on species 1 (\\(m_{1B}\\))"),
                                       min = -0.1,
                                       max = 0.1,
                                       value = -0.04)
                    ),
                    column(6,
                           sliderInput(inputId = "m2A",
                                       label = withMathJax("Effect of microbes A on species 2 (\\(m_{2A}\\))"),
                                       min = -0.1,
                                       max = 0.1,
                                       value = -0.01),
                           sliderInput(inputId = "m2B",
                                       label = withMathJax("Effect of microbes B on species 2 (\\(m_{2B}\\))"),
                                       min = -0.1,
                                       max = 0.1,
                                       value = -0.021)
                    ))
              ),
              
              # Third row -------
              fluidRow(
                # Output box for plot -----
                box(width = 8, plotOutput("twosp_cone")),
                box(width = 4, plotOutput("twosp_traj"))
              )
      ),
      
      
      
      # Second tab content ------
      tabItem(tabName = "threesp",
              h2("Dynamics of a system with three plant species"),
              
              # First row ----
              fluidRow(box(actionButton("scenario3_reset", label = "Reset to Scenario 3"),
                           actionButton("scenario3.2_reset", label = "Reset to Scenario 3.2"))),
              
              # Second row: inputs -----
              fluidRow(
                
                # Box of Intrinsic rates ---------
                box(width = 4,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Intrinsic rates and plant effects on microbes",
                    column(4,
                           sliderInput(inputId = "g1_3",
                                       label = withMathJax("Intrinsic growth of species 1 (\\(g_1\\))"),
                                       min = 0.1,
                                       max = 2,
                                       value = .2),
                           sliderInput(inputId = "g2_3",
                                       label = withMathJax("Intrinsic growth of species 1 (\\(g_2\\))"),
                                       min = 0.1,
                                       max = 2,
                                       value = .2),
                           sliderInput(inputId = "g3_3",
                                       label = withMathJax("Intrinsic growth of species 1 (\\(g_2\\))"),
                                       min = 0.1,
                                       max = 2,
                                       value = .2)),
                    column(4,
                           sliderInput(inputId = "vA1_3",
                                       label = withMathJax("Effect of species 1 on microbes A (\\(v_{A1}\\))"),
                                       min = 0.0001,
                                       max = 0.01,
                                       value = 0.01),
                           sliderInput(inputId = "vB2_3",
                                       label = withMathJax("Effect of species 2 on microbes B (\\(v_{B2}\\))"),
                                       min = 0.0001,
                                       max = 0.01,
                                       value = 0.01),
                           sliderInput(inputId = "vC3_3",
                                       label = withMathJax("Effect of species 3 on microbes C (\\(v_{C3}\\))"),
                                       min = 0.0001,
                                       max = 0.01,
                                       value = 0.01)
                    ),
                    column(4,
                           sliderInput(inputId = "qA_3",
                                       label = withMathJax("Mortality rates of microbes A \\((q_A\\))"),
                                       min = 0.0001,
                                       max = 0.1,
                                       value = .01),
                           sliderInput(inputId = "qB_3",
                                       label = withMathJax("Mortality rates of microbes B \\((q_B\\))"),
                                       min = 0.0001,
                                       max = 0.1,
                                       value = .01),
                           sliderInput(inputId = "qC_3",
                                       label = withMathJax("Mortality rates of microbes C \\((q_C\\))"),
                                       min = 0.0001,
                                       max = 0.1,
                                       value = .01)
                    )
                ),
                
                # Box of competition parameters -------
                box(width = 4,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Intra- and inter-specific competition parameters",
                    column(4,
                           sliderInput(inputId = "c11_3",
                                       label = withMathJax("Intraspecific competition- species 1 (\\(c_{11}\\))"),
                                       min = 0.0001,
                                       max = 0.01,
                                       value = .001, step = .0001),
                           sliderInput(inputId = "c12_3",
                                       label = withMathJax("Interspecific competition- effect of species 2 on species 1 (\\(c_{12}\\))"),
                                       min = 0.0001,
                                       max = 0.01,
                                       value = .001, step = .0001),
                           sliderInput(inputId = "c13_3",
                                       label = withMathJax("Interspecific competition- effect of species 3 on species 1 (\\(c_{13}\\))"),
                                       min = 0.0001,
                                       max = 0.01,
                                       value = .001, step = .0001)
                           
                    ),
                    column(4,
                           sliderInput(inputId = "c21_3",
                                       label = withMathJax("Interspecific competition- effect of species 1 on species 2 (\\(c_{21}\\))"),
                                       min = 0.0001,
                                       max = 0.01,
                                       value = .001, step = .0001),
                           sliderInput(inputId = "c22_3",
                                       label = withMathJax("Intraspecific competition- species 2 (\\(c_{22}\\))"),
                                       min = 0.0001,
                                       max = 0.01,
                                       value = .001, step = .0001),
                           sliderInput(inputId = "c23_3",
                                       label = withMathJax("Interspecific competition- effect of species 3 on species 2 (\\(c_{23}\\))"),
                                       min = 0.0001,
                                       max = 0.01,
                                       value = .001, step = .0001)
                    ),
                    column(4,
                           sliderInput(inputId = "c31_3",
                                       label = withMathJax("Interspecific competition- effect of species 3 on species 1 (\\(c_{31}\\))"),
                                       min = 0.0001,
                                       max = 0.01,
                                       value = .001, step = .0001),
                           sliderInput(inputId = "c32_3",
                                       label = withMathJax("Interspecific competition- effect of species 2 on species 1 (\\(c_{21}\\))"),
                                       min = 0.0001,
                                       max = 0.01,
                                       value = .001, step = .0001),
                           sliderInput(inputId = "c33_3",
                                       label = withMathJax("Intraspecific competition- species 3 (\\(c_{33}\\))"),
                                       min = 0.0001,
                                       max = 0.01,
                                       value = .001, step = .0001)
                           
                    )
                ),
                # Box of Microbe effects on plants ------
                box(width = 4,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Microbe effects on plants",
                    column(4,
                           sliderInput(inputId = "m1A_3",
                                       label = withMathJax("Effect of microbes A on species 1 (\\(m_{1A}\\))"),
                                       min = -0.1,
                                       max = 0.1,
                                       value = -0.012/2),
                           sliderInput(inputId = "m1B_3",
                                       label = withMathJax("Effect of microbes B on species 1 (\\(m_{1B}\\))"),
                                       min = -0.1,
                                       max = 0.1,
                                       value = -0.01/2),
                           sliderInput(inputId = "m1C_3",
                                       label = withMathJax("Effect of microbes B on species 1 (\\(m_{1C}\\))"),
                                       min = -0.1,
                                       max = 0.1,
                                       value = -0.03/2.25)
                           
                    ),
                    column(4,
                           sliderInput(inputId = "m2A_3",
                                       label = withMathJax("Effect of microbes A on species 2 (\\(m_{2A}\\))"),
                                       min = -0.1,
                                       max = 0.1,
                                       value = -0.013/2),
                           sliderInput(inputId = "m2B_3",
                                       label = withMathJax("Effect of microbes B on species 2 (\\(m_{2B}\\))"),
                                       min = -0.1,
                                       max = 0.1,
                                       value = -0.017/2),
                           sliderInput(inputId = "m2C_3",
                                       label = withMathJax("Effect of microbes B on species 2 (\\(m_{2C}\\))"),
                                       min = -0.1,
                                       max = 0.1,
                                       value = -0.015/2)
                           
                    ),
                    column(4,
                           sliderInput(inputId = "m3A_3",
                                       label = withMathJax("Effect of microbes A on species 3 (\\(m_{3A}\\))"),
                                       min = -0.1,
                                       max = 0.1,
                                       value = -.0075/2),
                           sliderInput(inputId = "m3B_3",
                                       label = withMathJax("Effect of microbes B on species 3 (\\(m_{3B}\\))"),
                                       min = -0.1,
                                       max = 0.1,
                                       value = -0.018/2),
                           sliderInput(inputId = "m3C_3",
                                       label = withMathJax("Effect of microbes B on species 3 (\\(m_{3C}\\))"),
                                       min = -0.1,
                                       max = 0.1,
                                       value = -0.02/2)
                           
                    )
                )
              ),
              
              
              #  Third row: plots ---------
              
              fluidRow(
                box(width = 6, plotOutput("threesp_cone")),
                box(width = 6, plotOutput("threesp_traj"))
                
              )
              
      ),
      
      # Third tab content-------
      
      tabItem(tabName = "resourceComp",
              
              h2("Dynamics of a system with explicit resource competition among plants"),
              
              fluidRow(box(actionButton("resourcecomp_reset", label = "Reset to default values"))),
              
              fluidRow(
                # Box for plant uptake of resources --------
                box(
                  width = 3,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  title = "Plant resource uptake rates",
                  column(6,
                         sliderInput(inputId = "u1l_RC",
                                     label = withMathJax("Per-capita of resource \\(l\\) by plant 1 (\\(u_{1l}\\))"),
                                     min = 0.0001,
                                     max = 0.01,
                                     value = 0.002),
                         sliderInput(inputId = "u2l_RC",
                                     label = withMathJax("Per-capita of resource \\(l\\) by plant 2 (\\(u_{2l}\\))"),
                                     min = 0.0001,
                                     max = 0.01,
                                     value = 0.0002)),
                  column(6,
                         sliderInput(inputId = "u1n_RC",
                                     label = withMathJax("Per-capita of resource \\(n\\) by plant1 (\\(u_{1n}\\))"),
                                     min = 0.0001,
                                     max = 0.01,
                                     value = 0.0002),
                         sliderInput(inputId = "u2n_RC",
                                     label = withMathJax("Per-capita of resource \\(n\\) by plant 2 (\\(u_{2n}\\))"),
                                     min = 0.0001,
                                     max = 0.01,
                                     value = 0.002))
                ), # closes box ----
                # Box for microbe effects on plants ------
                box(width = 3,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Microbe effects on plants and plant effects on microbes",
                    column(4,
                           sliderInput(inputId = "m1A_RC",
                                       label = withMathJax("Effect of microbes A on species 1 (\\(m_{1A}\\))"),
                                       min = -0.1,
                                       max = 0.1,
                                       value = -.005),
                           sliderInput(inputId = "m1B_RC",
                                       label = withMathJax("Effect of microbes B on species 1 (\\(m_{1B}\\))"),
                                       min = -0.1,
                                       max = 0.1,
                                       value = -0.0048)
                    ),
                    column(4,
                           sliderInput(inputId = "m2A_RC",
                                       label = withMathJax("Effect of microbes A on species 2 (\\(m_{2A}\\))"),
                                       min = -0.1,
                                       max = 0.1,
                                       value = -0.0048),
                           sliderInput(inputId = "m2B_RC",
                                       label = withMathJax("Effect of microbes B on species 2 (\\(m_{2B}\\))"),
                                       min = -0.1,
                                       max = 0.1,
                                       value = -0.005)
                    ),
                    column(4,
                           sliderInput(inputId = "vA1_RC",
                                       label = withMathJax("Effect of species 1 on microbes A (\\(v_{A1}\\))"),
                                       min = 0.0001,
                                       max = 0.01,
                                       value = 0.005),
                           sliderInput(inputId = "vB2_RC",
                                       label = withMathJax("Effect of species 2 on microbes B (\\(v_{B2}\\))"),
                                       min = 0.0001,
                                       max = 0.01,
                                       value = 0.005)
                    )
                ), # closes box ----
                # Box for microbe and plant mortality rates ----
                
                box(width = 3,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Intrinsic mortality rates for microbes and plants",
                    column(6,
                           sliderInput(inputId = "mu1_RC",
                                       label = withMathJax("Intrinsic mortality rate of species 1 (\\(\\mu_1\\))"),
                                       min = .0001,
                                       max = .1,
                                       value = .001),
                           sliderInput(inputId = "mu2_RC",
                                       label = withMathJax("Intrinsic mortality rate of species 2 (\\(\\mu_2\\))"),
                                       min = .0001,
                                       max = .1,
                                       value = .001)),
                    column(6,
                           sliderInput(inputId = "qA_RC",
                                       label = withMathJax("Mortality rates of microbes A \\((q_A\\))"),
                                       min = 0.0001,
                                       max = 0.1,
                                       value = .005),
                           sliderInput(inputId = "qB_RC",
                                       label = withMathJax("Mortality rates of microbes B \\((q_B\\))"),
                                       min = 0.0001,
                                       max = 0.1,
                                       value = .005))
                ), # closes box -----
                # Box for intrinsic growth and limitation for resources ----
                box(width = 3,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Intrinsic growth and limitation for resources",
                    column(6,
                           sliderInput(inputId = "rl_RC",
                                       label = withMathJax("Intrinsic growth rate of resource \\(l\\) (\\(r_l\\))"),
                                       min = 0.001,
                                       max = .1,
                                       value = 0.03),
                           sliderInput(inputId = "rn_RC",
                                       label = withMathJax("Intrinsic growth rate of resource \\(n\\) (\\(r_n\\))"),
                                       min = 0.001,
                                       max = .1,
                                       value = 0.03)),
                    column(6,
                           sliderInput(inputId = "sl_RC",
                                       label = withMathJax("Saturation rate of resource \\(l\\) (\\(s_l\\))"),
                                       min = 0.0001,
                                       max = 0.1,
                                       value = .001),
                           sliderInput(inputId = "sn_RC",
                                       label = withMathJax("Saturation rate of resource \\(n\\) (\\(s_n\\))"),
                                       min = 0.0001,
                                       max = 0.1,
                                       value = .001)
                    ) # closes column
                ) # closes box  -----
              ), # closes row
              
              # Next row ----
              
              fluidRow(
                box(width = 8, plotOutput("resourcomp_cone"))
              )
              
      )
    )
  )
)
