context("Function read_candidates()")


opts <- setSimulationPath(study_path)



test_that("read_candidates() works", {
  candidate_file <- paste(opts$studyPath,"/user/expansion/candidates.ini",sep="") 
  candidates <- read_candidates(candidate_file, opts)
  
  expect_is(candidates, "list")
  expect_length(candidates, 5)
  
  for(c in candidates)
  {
    expect_equal(c$unit_size * c$max_unit, c$max_invest)
  }
})



test_that("candidates names do not contain blank spaces", {
  candidate_file <- paste(opts$studyPath,"/user/expansion/candidates.ini",sep="") 
  candidates <- read_candidates(candidate_file, opts)
  
  names <- sapply(candidates, FUN = function(c){c$name})
  have_space <- any(grepl(" ", names))
  expect_false(have_space)
})



test_that("candidates links exist in the Antares Study", {
  candidate_file <- paste(opts$studyPath,"/user/expansion/candidates.ini",sep="") 
  candidates <- read_candidates(candidate_file, opts)
  
  links <- sapply(candidates, FUN = function(c){c$link})
  expect_true(any(links %in% opts$linkList))
})



test_that("candidate link_profile exists and contains 8760 values", {
  candidate_file <- paste(opts$studyPath,"/user/expansion/candidates.ini",sep="") 
  candidates <- read_candidates(candidate_file, opts)
  
  for(c in candidates)
  {
    expect_is(c$link_profile, "numeric")

    if(c$has_link_profile)
    {
      expect_length(c$link_profile, 8760)
    } else {
      expect_length(c$link_profile, 1)
    }
  }
})



test_that("Return warning when unknown characteristics is given", {
  candidate_file <- paste(opts$studyPath,"/user/expansion/other_inputs_for_test/candidates-unknownarg.ini",sep="") 
  expect_warning(read_candidates(candidate_file, opts))
})
  


test_that("Return error when link does not belong to the study", {
  candidate_file <- paste(opts$studyPath,"/user/expansion/other_inputs_for_test/candidates-unknownlink.ini",sep="") 
  expect_error(read_candidates(candidate_file, opts))
})


test_that("Return error when several candidates have the same name ", {
  candidate_file <- paste(opts$studyPath,"/user/expansion/other_inputs_for_test/candidates-identicalnames.ini",sep="") 
  expect_error(read_candidates(candidate_file, opts))
})
