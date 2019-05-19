scenario_2_parameters <- c(g1 = 1, g2 = 1,
                           c11 = 0.0005, c12 = 0.001, 
                           c21 = 0.001, c22 = 0.002,
                           m1A = -.025, m1B = -.025,
                           m2A = -.01, m2B = -.01,
                           vA1 = 0.005, vA2 = 0, vB1 = 0, vB2 = 0.005,
                           qA = 0.1, qB = 0.1, for_app = TRUE)


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
