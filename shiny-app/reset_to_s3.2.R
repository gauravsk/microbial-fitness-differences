scenario_S3.2_parameters <- c(g1 = .2, g2 = .2, g3 = .2,   
                      ## COMPETITION
                      c11 = .001, c12 = .001, c13 = .001,
                      c21 = .001, c22 = .001, c23 = .001,
                      c31 = .001, c32 = .001, c33 = .001,
                      
                      # MICROBE EFFECTS
                      m1A = -0.015/2, m1B = -0.01/2,  m1C = -0.01/2,
                      m2A = -0.01/2,  m2B = -0.013/2, m2C = -0.01/2,
                      m3A = -0.014/2, m3B = -0.012/2, m3C = -0.012/2,
                      
                      
                      ## MICROBE INTRINSIC
                      qA = .01, qB = .01, qC = .01,
                      
                      ## PLANT EFFECTS ON MICROBES
                      vA1 = 0.01, vB1 = 0, vC1 = 0,
                      vA2 = 0, vB2 = 0.01, vC2 = 0,
                      vA3 = 0, vB3 = 0, vC3 = 0.01)

updateSliderInput(session, "m1A_3", value = unname(scenario_S3.2_parameters["m1A"]))
updateSliderInput(session, "m1B_3", value = unname(scenario_S3.2_parameters["m1B"]))
updateSliderInput(session, "m1C_3", value = unname(scenario_S3.2_parameters["m1C"]))
updateSliderInput(session, "m2A_3", value = unname(scenario_S3.2_parameters["m2A"]))
updateSliderInput(session, "m2B_3", value = unname(scenario_S3.2_parameters["m2B"]))
updateSliderInput(session, "m2C_3", value = unname(scenario_S3.2_parameters["m2C"]))
updateSliderInput(session, "m3A_3", value = unname(scenario_S3.2_parameters["m3A"]))
updateSliderInput(session, "m3B_3", value = unname(scenario_S3.2_parameters["m3B"]))
updateSliderInput(session, "m3C_3", value = unname(scenario_S3.2_parameters["m3C"]))
updateSliderInput(session, "c11_3", value = unname(scenario_S3.2_parameters["c11"]))
updateSliderInput(session, "c12_3", value = unname(scenario_S3.2_parameters["c12"]))
updateSliderInput(session, "c13_3", value = unname(scenario_S3.2_parameters["c13"]))
updateSliderInput(session, "c21_3", value = unname(scenario_S3.2_parameters["c21"]))
updateSliderInput(session, "c22_3", value = unname(scenario_S3.2_parameters["c22"]))
updateSliderInput(session, "c23_3", value = unname(scenario_S3.2_parameters["c23"]))
updateSliderInput(session, "c31_3", value = unname(scenario_S3.2_parameters["c31"]))
updateSliderInput(session, "c32_3", value = unname(scenario_S3.2_parameters["c32"]))
updateSliderInput(session, "c33_3", value = unname(scenario_S3.2_parameters["c33"]))
updateSliderInput(session, "vA1_3", value = unname(scenario_S3.2_parameters["vA1"]))
updateSliderInput(session, "vB2_3", value = unname(scenario_S3.2_parameters["vB2"]))
updateSliderInput(session, "vC3_3", value = unname(scenario_S3.2_parameters["vC3"]))
updateSliderInput(session, "g1_3", value = unname(scenario_S3.2_parameters["g1"]))
updateSliderInput(session, "g2_3", value = unname(scenario_S3.2_parameters["g2"]))
updateSliderInput(session, "g3_3", value = unname(scenario_S3.2_parameters["g3"]))
updateSliderInput(session, "qA_3", value = unname(scenario_S3.2_parameters["qA"]))
updateSliderInput(session, "qB_3", value = unname(scenario_S3.2_parameters["qB"]))
updateSliderInput(session, "qC_3", value = unname(scenario_S3.2_parameters["qC"]))
