context("Function get_rentability()")


test_that("get_rentability() works", {
  
  # prepare inputs
  n_w <- 52
  opts_fast <- setSimulationPath(paste0(study_path, "/output/20171108-1548eco-fast/"))
  current_it <- list()
  current_it$full <- TRUE
  candidates <- read_candidates(file = paste0(opts_fast$studyPath, "/user/expansion/candidates.ini"), opts_fast)
  
  # launch function
  rentability <- antaresXpansion:::get_expected_rentability(opts_fast, current_it, candidates, n_w)
  
  # compare to some known values
  expect_true(all(abs(rentability - c(82231.00,  -5726.00,  13059.37 ,-16497.00  ,-9910.00)) < 0.01))
  
})