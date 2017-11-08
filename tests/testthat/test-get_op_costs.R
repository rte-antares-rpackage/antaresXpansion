context("Function get_op_costs()")


test_that("get_op_costs() works", {
  
  current_it <- list()
  exp_options <- list()
  opts_fast <- setSimulationPath(paste0(study_path, "/output/20171108-1548eco-fast/"))
  opts_accurate <- setSimulationPath(paste0(study_path, "/output/20171108-1647eco-acc/"))
  
  #if current iteration is not full, return NA
  current_it$full <- FALSE
  exp_options$uc_type <- "expansion_fast"
  costs <- get_op_costs(opts_fast, current_it, exp_options)
  expect_true(is.na(costs))
  
  #in expansion_fast mode, return OV.COST + HURDLE COST - NP COST
  current_it$full <- TRUE
  exp_options$uc_type <- "expansion_fast"
  costs <- get_op_costs(opts_fast, current_it, exp_options)
  expect_equal(costs, 1132476127)

  # in expansion_accurate mode, equal to sum of optimization criteria
  current_it$full <- TRUE
  current_it$mc_years <- c(1,2,3)
  exp_options$uc_type <- "expansion_accurate"
  costs <- get_op_costs(opts_accurate, current_it, exp_options)
  expect_equal(costs, 1133447807)
  
  
  
})

