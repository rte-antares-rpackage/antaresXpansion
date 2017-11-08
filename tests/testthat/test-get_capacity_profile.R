context("Function get_capacity_profile")



test_that(".round_with_threshold() works ", {
  
  # if x contains negative value, function should return an error
  expect_error(.round_with_threshold(-1,0.5, 0))

  #test a few values
  expect_equal(.round_with_threshold(c(0.234, 0.515, 0.887), 0.5 , 0) , c(0, 1, 1))
  expect_equal(.round_with_threshold(c(0.234, 0.515, 0.887), 0.2 , 0) , c(1, 1, 1))
  expect_equal(.round_with_threshold(c(0.234, 0.515, 0.887), 0.8 , 0) , c(0, 0, 1))
  expect_equal(.round_with_threshold(c(0.234, 0.515, 0.887), 0.5 , 2) , c(0.23, 0.52, 0.89))
  expect_equal(.round_with_threshold(c(0.234, 0.515, 0.887), 0.2 , 2) , c(0.24, 0.52, 0.89))
  expect_equal(.round_with_threshold(c(0.234, 0.515, 0.887), 0.8 , 2) , c(0.23, 0.51, 0.88))
  
})


test_that("get_capacity_profile() works ", {
  load_factor_profile <- seq(from = 0, to = 1, length.out = 8760)

  # in fast mode
  uc_mode <- "expansion_fast"
  out_fast <- get_capacity_profile(installed_capacity = 1.5, load_factor_profile = load_factor_profile, uc_mode = uc_mode)
  
  expect_length(out_fast, 8760)
  expect_true(abs(sum(out_fast)- 1.5*sum(load_factor_profile)) <= 10^-6 )
  expect_true(all.equal(out_fast, round(out_fast, digits = 6)))
  
  # in accurate mode
  uc_mode <- "expansion_accurate"
  out_accurate <- get_capacity_profile(installed_capacity = 1.5, load_factor_profile = load_factor_profile, uc_mode = uc_mode)
  
  expect_length(out_accurate, 8760)
  expect_true(abs(sum(out_accurate)- 1.5*sum(load_factor_profile)) <= 1)
  expect_true(all.equal(out_accurate, as.integer(out_accurate)))
  
  # when installed capacity is null
  uc_mode <- "expansion_fast"
  out_fast <- get_capacity_profile(installed_capacity = 0, load_factor_profile = load_factor_profile, uc_mode = uc_mode)
  expect_length(out_fast, 8760)
  expect_true(all(out_fast == 0))
  
})



test_that("get_capacity_profile() returns error when expected ", {
  expect_error(get_capacity_profile(installed_capacity = 1.5, load_factor_profile = seq(from = 0, to = 1, length.out = 8759), uc_mode = "fast")  )
  expect_error(get_capacity_profile(installed_capacity = 1.5, load_factor_profile = seq(from = -1, to = 1, length.out = 8760), uc_mode = "fast")  )
  expect_error(get_capacity_profile(installed_capacity = -1, load_factor_profile = seq(from = 0, to = 1, length.out = 8760), uc_mode = "fast")  )
})
  
