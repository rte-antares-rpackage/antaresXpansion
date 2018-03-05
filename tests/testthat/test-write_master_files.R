context("Function write_master_files()")


test_that("write_master_files() works", {
  
  
  # define some inputs
  tmp_folder <- tempdir()
  
  opts_fast <- setSimulationPath(paste0(study_path, "/output/20171108-1548eco-fast/"))
  opts_accurate <- setSimulationPath(paste0(study_path, "/output/20171108-1647eco-acc/"))
  
  current_it <- list()
  current_it$n <- 1  
  current_it$id <- "it1"  
  current_it$full <- TRUE  
  current_it$mc_years <- opts_fast$mcYears
  current_it$weeks <- 1:52
  current_it$cut_type <- "average"
  
  candidates <- read_candidates(paste0(opts_fast$studyPath,"/user/expansion/candidates.ini"), opts = opts_fast)
  exp_options <-  read_options(paste0(opts_fast$studyPath,"/user/expansion/settings.ini"), opts_fast)
  
  n_w <- length(current_it$weeks)
  
  x <- list()
  x$invested_capacities <- initiate_candidate_capacities(candidates, 2020)
  x$costs <- data.frame(
    it = 1,
    year = 2020,
    investment_costs = 6000,
    operation_costs = 4000,
    overall_costs = 10000
  )
  
  # 1. test expansion_fast AND average cuts
  exp_options$uc_type <- "expansion_fast"
  current_it$cut_type <- "average"
  folder <- paste0(tmp_folder, "\\test1")
  dir.create(folder)
  write_master_files(folder, opts_fast, current_it, candidates, exp_options, x, n_w)
    
  expect_file_exists(paste0(folder, "/in_avgrentability.txt"))
  expect_file_exists(paste0(folder, "/in_avgcuts.txt"))
  expect_file_exists(paste0(folder, "/in_z0.txt"))
  expect_file_exists(paste0(folder, "/in_iterations.txt"))
  
  data_rentability <- scan(paste0(folder, "/in_avgrentability.txt"), what=character(), sep="/", quiet = TRUE)
  data_cost <- scan(paste0(folder, "/in_avgcuts.txt"), what=character(), sep="/", quiet = TRUE)
  data_iteration <- scan(paste0(folder, "/in_iterations.txt"), what=character(), sep="/", quiet = TRUE)
  data_z0 <- scan(paste0(folder, "/in_z0.txt"), what=character(), sep="/", quiet = TRUE)
  
  expect_length(data_iteration,1)
  expect_length(data_z0,length(candidates))
  expect_length(data_rentability, length(candidates))
  expect_length(data_cost, 1)
  
  # 2. test expansion_accurate AND average cuts
  exp_options$uc_type <- "expansion_accurate"
  current_it$cut_type <- "average"
  folder <- paste0(tmp_folder, "\\test2")
  dir.create(folder)
  write_master_files(folder, opts_accurate, current_it, candidates, exp_options, x, n_w)
  
  expect_file_exists(paste0(folder, "/in_avgrentability.txt"))
  expect_file_exists(paste0(folder, "/in_avgcuts.txt"))
  expect_file_exists(paste0(folder, "/in_z0.txt"))
  expect_file_exists(paste0(folder, "/in_iterations.txt"))
  
  data_rentability <- scan(paste0(folder, "/in_avgrentability.txt"), what=character(), sep="/", quiet = TRUE)
  data_cost <- scan(paste0(folder, "/in_avgcuts.txt"), what=character(), sep="/", quiet = TRUE)
  data_iteration <- scan(paste0(folder, "/in_iterations.txt"), what=character(), sep="/", quiet = TRUE)
  data_z0 <- scan(paste0(folder, "/in_z0.txt"), what=character(), sep="/", quiet = TRUE)
  
  expect_length(data_iteration,1)
  expect_length(data_z0,length(candidates))
  expect_length(data_rentability, length(candidates))
  expect_length(data_cost, 1)
  
  # 3. test expansion_fast AND yearly cuts
  exp_options$uc_type <- "expansion_fast"
  current_it$cut_type <- "yearly"
  folder <- paste0(tmp_folder, "\\test3")
  dir.create(folder)
  write_master_files(folder, opts_fast, current_it, candidates, exp_options, x, n_w)
  
  expect_file_exists(paste0(folder, "/in_yearlyrentability.txt"))
  expect_file_exists(paste0(folder, "/in_yearlycuts.txt"))
  expect_file_exists(paste0(folder, "/in_z0.txt"))
  expect_file_exists(paste0(folder, "/in_iterations.txt"))
  
  data_rentability <- scan(paste0(folder, "/in_yearlyrentability.txt"), what=character(), sep="/", quiet = TRUE)
  data_cost <- scan(paste0(folder, "/in_yearlycuts.txt"), what=character(), sep="/", quiet = TRUE)
  data_iteration <- scan(paste0(folder, "/in_iterations.txt"), what=character(), sep="/", quiet = TRUE)
  data_z0 <- scan(paste0(folder, "/in_z0.txt"), what=character(), sep="/", quiet = TRUE)
  
  expect_length(data_iteration,1)
  expect_length(data_z0,length(candidates))
  expect_length(data_rentability, length(candidates)*length(current_it$mc_years))
  expect_length(data_cost, 1*length(current_it$mc_years))
  
  # 4. test expansion_accurate AND yearly cuts
  exp_options$uc_type <- "expansion_accurate"
  current_it$cut_type <- "yearly"
  folder <- paste0(tmp_folder, "\\test4")
  dir.create(folder)
  write_master_files(folder, opts_accurate, current_it, candidates, exp_options, x, n_w)
  
  expect_file_exists(paste0(folder, "/in_yearlyrentability.txt"))
  expect_file_exists(paste0(folder, "/in_yearlycuts.txt"))
  expect_file_exists(paste0(folder, "/in_z0.txt"))
  expect_file_exists(paste0(folder, "/in_iterations.txt"))
  
  data_rentability <- scan(paste0(folder, "/in_yearlyrentability.txt"), what=character(), sep="/", quiet = TRUE)
  data_cost <- scan(paste0(folder, "/in_yearlycuts.txt"), what=character(), sep="/", quiet = TRUE)
  data_iteration <- scan(paste0(folder, "/in_iterations.txt"), what=character(), sep="/", quiet = TRUE)
  data_z0 <- scan(paste0(folder, "/in_z0.txt"), what=character(), sep="/", quiet = TRUE)
  
  expect_length(data_iteration,1)
  expect_length(data_z0,length(candidates))
  expect_length(data_rentability, length(candidates)*length(current_it$mc_years))
  expect_length(data_cost, 1*length(current_it$mc_years))
  
  #5. test expansion_fast AND weekly cuts
  exp_options$uc_type <- "expansion_fast"
  current_it$cut_type <- "weekly"
  folder <- paste0(tmp_folder, "\\test5")
  dir.create(folder)
  write_master_files(folder, opts_fast, current_it, candidates, exp_options, x, n_w)
  
  expect_file_exists(paste0(folder, "/in_weeklyrentability.txt"))
  expect_file_exists(paste0(folder, "/in_weeklycuts.txt"))
  expect_file_exists(paste0(folder, "/in_z0.txt"))
  expect_file_exists(paste0(folder, "/in_iterations.txt"))
  
  data_rentability <- scan(paste0(folder, "/in_weeklyrentability.txt"), what=character(), sep="/", quiet = TRUE)
  data_cost <- scan(paste0(folder, "/in_weeklycuts.txt"), what=character(), sep="/", quiet = TRUE)
  data_iteration <- scan(paste0(folder, "/in_iterations.txt"), what=character(), sep="/", quiet = TRUE)
  data_z0 <- scan(paste0(folder, "/in_z0.txt"), what=character(), sep="/", quiet = TRUE)
  
  expect_length(data_iteration,1)
  expect_length(data_z0,length(candidates))
  expect_length(data_rentability, length(candidates)*length(current_it$mc_years)*length(current_it$weeks))
  expect_length(data_cost, 1*length(current_it$mc_years)*length(current_it$weeks))
  
  
  #6. test expansion_accurate AND weekly cuts
  exp_options$uc_type <- "expansion_accurate"
  current_it$cut_type <- "weekly"
  folder <- paste0(tmp_folder, "\\test6")
  dir.create(folder)
  write_master_files(folder, opts_accurate, current_it, candidates, exp_options, x, n_w)
  
  expect_file_exists(paste0(folder, "/in_weeklyrentability.txt"))
  expect_file_exists(paste0(folder, "/in_weeklycuts.txt"))
  expect_file_exists(paste0(folder, "/in_z0.txt"))
  expect_file_exists(paste0(folder, "/in_iterations.txt"))
  
  data_rentability <- scan(paste0(folder, "/in_weeklyrentability.txt"), what=character(), sep="/", quiet = TRUE)
  data_cost <- scan(paste0(folder, "/in_weeklycuts.txt"), what=character(), sep="/", quiet = TRUE)
  data_iteration <- scan(paste0(folder, "/in_iterations.txt"), what=character(), sep="/", quiet = TRUE)
  data_z0 <- scan(paste0(folder, "/in_z0.txt"), what=character(), sep="/", quiet = TRUE)
  
  expect_length(data_iteration,1)
  expect_length(data_z0,length(candidates))
  expect_length(data_rentability, length(candidates)*length(current_it$mc_years)*length(current_it$weeks))
  expect_length(data_cost, 1*length(current_it$mc_years)*length(current_it$weeks))
  
  #7. expect error when folder does not exist
  folder <- paste0(tmp_folder, "\\test7\\unknownfolder\\blablabla\\75464M")
  expect_error(write_master_files(folder, opts_accurate, current_it, candidates, exp_options, x, n_w))
})
  