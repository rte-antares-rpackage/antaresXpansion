context("Functions related to master problem")

opts <- setSimulationPath(study_path,0)


test_that("Function initiate_master() works and all files are created", {
  initiate_master(candidates = read_candidates(paste0(opts$studyPath,"/user/expansion/candidates.ini"), opts =  opts), 
                  exp_options = read_options(paste0(opts$studyPath,"/user/expansion/settings.ini"), opts), 
                  opts = antaresRead::simOptions())
  
  tmp_folder <- paste(opts$studyPath,"/user/expansion/temp",sep="")
  expect_true(dir.exists(tmp_folder))
  expect_file_exists(paste0(tmp_folder, "/in_mc.txt"))
  expect_file_exists(paste0(tmp_folder, "/in_week.txt"))
  expect_file_exists(paste0(tmp_folder, "/in_candidates.txt"))
  expect_file_exists(paste0(tmp_folder, "/in_iterations.txt"))
  expect_file_exists(paste0(tmp_folder, "/in_z0.txt"))
  expect_file_exists(paste0(tmp_folder, "/in_avgrentability.txt"))
  expect_file_exists(paste0(tmp_folder, "/in_yearlyrentability.txt"))
  expect_file_exists(paste0(tmp_folder, "/in_weeklyrentability.txt"))
  expect_file_exists(paste0(tmp_folder, "/in_avgcuts.txt"))
  expect_file_exists(paste0(tmp_folder, "/in_yearlycuts.txt"))
  expect_file_exists(paste0(tmp_folder, "/in_weeklycuts.txt"))
  expect_file_exists(paste0(tmp_folder, "/in_options.txt"))
  expect_file_exists(paste0(tmp_folder, "/out_solutionmaster.txt"))
  expect_file_exists(paste0(tmp_folder, "/out_underestimator.txt"))
  expect_file_exists(paste0(tmp_folder, "/out_log.txt"))
  expect_file_exists(paste0(tmp_folder, "/out_dualaveragecut.txt"))
  expect_file_exists(paste0(tmp_folder, "/out_dualyearlycut.txt"))
  expect_file_exists(paste0(tmp_folder, "/out_dualweeklycut.txt"))
  expect_file_exists(paste0(tmp_folder, "/out_theta.txt"))
  expect_file_exists(paste0(tmp_folder, "/master_run.ampl"))
  expect_file_exists(paste0(tmp_folder, "/master_mod.ampl"))
  expect_file_exists(paste0(tmp_folder, "/master_dat.ampl"))
})
