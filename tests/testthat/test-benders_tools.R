context("Benders toolbox functions")

opts <- setSimulationPath(study_path, 1)


test_that("Test that set_antares_options() works ", {
  set_antares_options(benders_options =read_options(paste0(opts$studyPath,"/user/expansion/settings.ini"), opts), 
                      candidates = read_candidates(paste0(opts$studyPath,"/user/expansion/candidates.ini"), opts), 
                      opts)
  

  set_antares_options(benders_options = read_options(paste0(opts$studyPath,"/user/expansion/other_inputs_for_test/settings-2.ini"), opts), 
                      candidates = read_candidates(paste0(opts$studyPath,"/user/expansion/candidates.ini"), opts), 
                      opts)
  
  set_antares_options(benders_options = read_options(paste0(opts$studyPath,"/user/expansion/other_inputs_for_test/settings-3.ini"), opts), 
                      candidates = read_candidates(paste0(opts$studyPath,"/user/expansion/candidates.ini"), opts), 
                      opts)
})



test_that("Convergence ", {
  
  # opt_gap = 0.0001%, relaxed_opt_gap = 1%
  benders_options_1 <-  read_options(paste0(opts$studyPath,"/user/expansion/other_inputs_for_test/settings-1.ini"), opts)
  
  # opt_gap = 1000000, relaxed_opt_gap = 0.1%
  benders_options_2 <-  read_options(paste0(opts$studyPath,"/user/expansion/other_inputs_for_test/settings-2.ini"), opts)
  
  # opt_gap = -Inf, relaxed_opt_gap = 1000000
  benders_options_3 <-  read_options(paste0(opts$studyPath,"/user/expansion/other_inputs_for_test/settings-3.ini"), opts)
  
  #
  expect_true(convergence(10.0000005, 10, benders_options_1))
  expect_true(convergence_relaxed(10.01, 10, benders_options_1))
  expect_false(convergence(10.0005, 10, benders_options_1))
  expect_false(convergence_relaxed(11, 10, benders_options_1))
  
  expect_true(convergence(1000000-1, 0, benders_options_2))
  expect_true(convergence_relaxed(10.00005, 10, benders_options_2))
  expect_false(convergence(1000000+1, 0, benders_options_2))
  expect_false(convergence_relaxed(1000000*1.1+1, 0, benders_options_2))
  
  expect_true(convergence_relaxed(1000000-1, 0, benders_options_3))
  expect_false(convergence(0, 100, benders_options_3))
  expect_false(convergence_relaxed(1000001, 0, benders_options_3))
  
})
  

