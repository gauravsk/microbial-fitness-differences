library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Coexistence with competition and plant-soil feedbacks"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "bloop", icon = icon("dashboard")),
      menuItem("Dashboard2", tabName = "blooper", icon = icon("dashboard"))
      
  )),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    # Tab 1
    tabItems(
      tabItem(tabName = "bloop",
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
                                       max = 0.1,
                                       value = 0.005),
                           sliderInput(inputId = "vB2",
                                       label = withMathJax("Effect of species 2 on microbes B (\\(v_{B2}\\))"),
                                       min = 0.0001,
                                       max = 0.1,
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
                                       max = 0.1,
                                       value = .003),
                           sliderInput(inputId = "c12",
                                       label = withMathJax("Interspecific competition- effect of species 2 on species 1 (\\(c_{12}\\))"),
                                       min = 0.0001,
                                       max = 0.1,
                                       value = .0024)
                    ),
                    column(6,
                           sliderInput(inputId = "c21",
                                       label = withMathJax("Interspecific competition- effect of species 1 on species 2 (\\(c_{21}\\))"),
                                       min = 0.0001,
                                       max = 0.1,
                                       value = .0024),
                           sliderInput(inputId = "c22",
                                       label = withMathJax("Intraspecific competition- species 2 (\\(c_{22}\\))"),
                                       min = 0.0001,
                                       max = 0.1,
                                       value = .004)
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
                    )),
                # Output box for plot ------
                box(plotOutput("cone"))
              )
      ),
      
      tabItem(tabName = "blooper",
              fluidRow(
                  h3("test")
              ))
    )
  )
)