context("Function get_digest()")


test_that("get_digest() works", {
 
  opts_fast <- setSimulationPath(paste0(study_path, "/output/20171108-1548eco-fast/"))
  current_it <- list()
  
  # when current it is full
  current_it$full <- TRUE
  digest <- antaresXpansion:::get_digest(opts_fast, current_it)
  expect_type(digest, "list")
  expect_true(nrow(digest) == 8)
  expect_true("area" %in% names(digest))
  expect_true("LOLD" %in% names(digest))
  expect_true(all(!is.na(digest$LOLD)))
  
  # when current it not full
  current_it$full <- FALSE
  digest <- antaresXpansion:::get_digest(opts_fast, current_it)
  expect_type(digest, "list")
  expect_true(nrow(digest) == 8)
  expect_true("area" %in% names(digest))
  expect_true("LOLD" %in% names(digest))
  expect_true(all(is.na(digest$LOLD)))
  
})
