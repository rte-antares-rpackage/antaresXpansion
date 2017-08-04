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



test_that("Test that update_average_cuts() works ", {
  
  # define inputs of the function
  current_it <- list()
  current_it$n <- 1  
  current_it$id <- "it1"  
  current_it$full <- TRUE  
  current_it$mc_years <- length(opts$mcYears)
  first_sim_week <- 1 + ceiling((opts$parameters$general$simulation.start - 1)/7)
  n_w <- floor((opts$parameters$general$simulation.end - opts$parameters$general$simulation.start + 1)/7) # number of weeks 
  current_it$weeks <- first_sim_week:(first_sim_week + n_w - 1)
  current_it$cut_type <- "average"
  ov_cost <- 1000
  n_w <- length(current_it$weeks)
  tmp_folder <- paste0(opts$studyPath, "/user/expansion/temp")
  candidates <- read_candidates(paste0(opts$studyPath,"/user/expansion/candidates.ini"), opts)
  benders_options <-  read_options(paste0(opts$studyPath,"/user/expansion/settings.ini"), opts)
  output_link_s <- antaresRead::readAntares(areas = NULL, links = "all", mcYears = NULL, 
                                            timeStep = "annual", opts = opts, showProgress = FALSE)
  if (length(with_profile(candidates)) > 0 )
  {
    output_link_h_s <- readAntares(areas = NULL, links = with_profile(candidates), mcYears = NULL, 
                                 timeStep = "hourly", opts = opts, showProgress = FALSE)
  }
  # launch function
  initiate_master(candidates, benders_options , opts)
  update_average_cuts(current_it, candidates, output_link_s, output_link_h_s, ov_cost, n_w, tmp_folder, benders_options)
  
  # checks :
  expect_file_exists(paste0(tmp_folder, "/in_avgrentability.txt"))
  expect_file_exists(paste0(tmp_folder, "/in_avgcuts.txt"))
  
  
  
  data_rentability <- scan(paste0(tmp_folder, "/in_avgrentability.txt"), what=character(), sep="/", quiet = TRUE)
  data_cost <- scan(paste0(tmp_folder, "/in_avgcuts.txt"), what=character(), sep="/", quiet = TRUE)
  
  
  expect_length(data_rentability, length(candidates))
  expect_length(data_cost, 1)
})




test_that("Test that update_yearly_cuts() works ", {
  
  # define inputs of the function
  current_it <- list()
  current_it$n <- 1  
  current_it$id <- "it1"  
  current_it$full <- TRUE  
  current_it$mc_years <- opts$mcYears
  first_sim_week <- 1 + ceiling((opts$parameters$general$simulation.start - 1)/7)
  n_w <- floor((opts$parameters$general$simulation.end - opts$parameters$general$simulation.start + 1)/7) # number of weeks 
  current_it$weeks <- first_sim_week:(first_sim_week + n_w - 1)
  current_it$cut_type <- "yearly"
  inv_cost <- 1000
  n_w <- length(current_it$weeks)
  tmp_folder <- paste0(opts$studyPath, "/user/expansion/temp")
  candidates <- read_candidates(paste0(opts$studyPath,"/user/expansion/candidates.ini"), opts)
  benders_options <-  read_options(paste0(opts$studyPath,"/user/expansion/settings.ini"), opts)

  output_area_y <-  antaresRead::readAntares(areas = "all", links = NULL, mcYears = current_it$mc_years, 
                                           timeStep = "annual", opts = opts, showProgress = FALSE)
  output_link_y <-  antaresRead::readAntares(areas = NULL, links = "all", mcYears = current_it$mc_years, 
                                           timeStep = "annual", opts = opts, showProgress = FALSE)
  if (length(with_profile(candidates)) > 0 )
  {
    output_link_h = readAntares(areas = NULL, links = with_profile(candidates), mcYears = current_it$mc_years, 
                                timeStep = "hourly", opts = opts, showProgress = FALSE)
  }
  
  
  # launch function
  initiate_master(candidates, benders_options , opts)
  update_yearly_cuts(current_it, candidates, output_area_y,output_link_y, output_link_h, inv_cost, n_w, tmp_folder, benders_options)

  # checks :
  expect_file_exists(paste0(tmp_folder, "/in_yearlyrentability.txt"))
  expect_file_exists(paste0(tmp_folder, "/in_yearlycuts.txt"))
  
  
  
  data_rentability <- scan(paste0(tmp_folder, "/in_yearlyrentability.txt"), what=character(), sep="/", quiet = TRUE)
  data_cost <- scan(paste0(tmp_folder, "/in_yearlycuts.txt"), what=character(), sep="/", quiet = TRUE)

  expect_length(data_rentability, length(candidates) * length(current_it$mc_years))
  expect_length(data_cost, length(current_it$mc_years))
})



test_that("Test that update_weekly_cuts() works ", {
  
  # define inputs of the function
  current_it <- list()
  current_it$n <- 1  
  current_it$id <- "it1"  
  current_it$full <- TRUE  
  current_it$mc_years <- opts$mcYears
  first_sim_week <- 1 + ceiling((opts$parameters$general$simulation.start - 1)/7)
  n_w <- floor((opts$parameters$general$simulation.end - opts$parameters$general$simulation.start + 1)/7) # number of weeks 
  current_it$weeks <- first_sim_week:(first_sim_week + n_w - 1)
  current_it$cut_type <- "average"
  inv_cost <- 1000
  n_w <- length(current_it$weeks)
  tmp_folder <- paste0(opts$studyPath, "/user/expansion/temp")
  candidates <- read_candidates(paste0(opts$studyPath,"/user/expansion/candidates.ini"), opts)
  benders_options <-  read_options(paste0(opts$studyPath,"/user/expansion/settings.ini"), opts)

  output_area_w = antaresRead::readAntares(areas = "all", links = NULL, mcYears = current_it$mc_years, 
                                           timeStep = "weekly", opts = opts, showProgress = FALSE)
  output_link_w = antaresRead::readAntares(areas = NULL, links = "all", mcYears = current_it$mc_years, 
                                           timeStep = "weekly", opts = opts, showProgress = FALSE)
  if (length(with_profile(candidates)) > 0 )
  {
    output_link_h = readAntares(areas = NULL, links = with_profile(candidates), mcYears = current_it$mc_years, 
                                timeStep = "hourly", opts = opts, showProgress = FALSE)
  }
  
  # launch function
  initiate_master(candidates, benders_options , opts)
  update_weekly_cuts(current_it, candidates, output_area_w, output_link_w, output_link_h, inv_cost, n_w, tmp_folder, benders_options)
    
  # checks :
  expect_file_exists(paste0(tmp_folder, "/in_weeklyrentability.txt"))
  expect_file_exists(paste0(tmp_folder, "/in_weeklycuts.txt"))
  
  
  
  data_rentability <- scan(paste0(tmp_folder, "/in_weeklyrentability.txt"), what=character(), sep="/", quiet = TRUE)
  data_cost <- scan(paste0(tmp_folder, "/in_weeklycuts.txt"), what=character(), sep="/", quiet = TRUE)
  
  
  expect_length(data_rentability, length(candidates) * length(current_it$mc_years) * n_w)
  expect_length(data_cost, length(current_it$mc_years) * n_w)
})


test_that("Convergence ", {
  
  # opt_gap = 0.0001%, relaxed_opt_gap = 1%
  benders_options_1 <-  read_options(paste0(opts$studyPath,"/user/expansion/settings.ini"), opts)
  
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
  

