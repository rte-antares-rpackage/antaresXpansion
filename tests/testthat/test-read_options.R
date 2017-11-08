context("Function read_options()")

opts <- setSimulationPath(study_path)



test_that("read_options() works", {
  option_file <- paste(opts$studyPath,"/user/expansion/settings.ini",sep="") 
  options <- read_options(option_file, opts)
  expect_is(options, "list")
})



test_that("check default values", {
  option_file <- paste(opts$studyPath,"/user/expansion/other_inputs_for_test/settings-empty.ini",sep="") 
  options <- read_options(option_file, opts)
  
  expect_equal(options$method, "benders_decomposition")
  expect_equal(options$uc_type, "expansion_fast")
  expect_equal(options$master, "integer")
  expect_equal(options$optimality_gap, 0)
  expect_equal(options$max_iteration, Inf)
  expect_equal(options$cut_type, "yearly")
  expect_equal(options$week_selection, FALSE)
  expect_equal(options$relaxed_optimality_gap, "0.01%")
})




test_that("return warning when one option is not known", {
  option_file <- paste(opts$studyPath,"/user/expansion/other_inputs_for_test/settings-unknownopt.ini",sep="") 
  expect_warning(read_options(option_file, opts))
  
})

test_that("return warning or error when modes which are not used since v0.7 are selected", {
  # relaxed_fast mode (now renamed to expansion_fast)
  option_file <- paste(opts$studyPath,"/user/expansion/other_inputs_for_test/settings-relaxed_fast.ini",sep="") 
  expect_warning(read_options(option_file, opts))
  
  # relaxed_accurate mode (now renamed to expansion_accurate)
  option_file <- paste(opts$studyPath,"/user/expansion/other_inputs_for_test/settings-relaxed_accurate.ini",sep="") 
  expect_warning(read_options(option_file, opts))
  
  #accurate mode (banned since v0.7)
  option_file <- paste(opts$studyPath,"/user/expansion/other_inputs_for_test/settings-accurate.ini",sep="") 
  expect_error(read_options(option_file, opts))
  
  #fast mode (banned since v0.7)
  option_file <- paste(opts$studyPath,"/user/expansion/other_inputs_for_test/settings-fast.ini",sep="") 
  expect_error(read_options(option_file, opts))
})



test_that("return errors when expected", {
  #optimality gap is non numeric
  option_file <- paste(opts$studyPath,"/user/expansion/other_inputs_for_test/settings-nonnumericgap.ini",sep="") 
  expect_error(read_options(option_file, opts))
  
  #unknown cut type
  option_file <- paste(opts$studyPath,"/user/expansion/other_inputs_for_test/settings-unknowncuttype.ini",sep="") 
  expect_error(read_options(option_file, opts))
  
  #not a true false when logical is expected
  option_file <- paste(opts$studyPath,"/user/expansion/other_inputs_for_test/settings-nottruefalse.ini",sep="") 
  expect_error(read_options(option_file, opts))
})
