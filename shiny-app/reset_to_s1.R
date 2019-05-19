scenario_1_parameters <- c(g1 = 1, g2 = 1,
                           c11 = 0.003, c12 = 0.0024,
                           c21 = 0.002, c22 = 0.004,
                           m1A = -0.05, m1B = -0.04,
                           m2A = -0.01, m2B = -0.021,
                           vA1 = 0.005, vA2 = 0, vB1 = 0, vB2 = 0.005,
                           qA = 0.01, qB = 0.01, for_app = TRUE)


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
