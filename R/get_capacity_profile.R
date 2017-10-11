#' @title Build capacity time series
#' @description Given the installed capacity and the load factor profile,
#' return the new capacity profile.
#' The capacity profile might not be equal to a simple multiplication between the
#' installed capacity and the load factor as it needs to be rounded to match with 
#' the ANTARES data format. The rounding is made so as to respect the total producible
#' energy of all the year.
#' 
#' @param installed_capacity
#' new installed capacity
#' @param load_factor_profile
#' Time series of scalar of actual percentage of available capacity
#' @param uc_mode
#' unit-commitment mode of the simulation (defines which rounding approximation is most appropriate)
#'      
#' @return 
#' capacity time series
#' 
#' @importFrom assertthat assert_that
#' 
get_capacity_profile <- function(installed_capacity, load_factor_profile, uc_mode)
{
  assertthat::assert_that(length(load_factor_profile) %in% c(1,8760))
  assertthat::assert_that(all(load_factor_profile >= 0))
  assertthat::assert_that(installed_capacity >= 0)
  
  
  # if the uc_mode is accurate, 
  if(grepl("accurate", uc_mode))
  {
    # due to a bug with decimal value in ANTARES's NTC in accurate mode, rounding
    # to nearest integer is necessary in accurate mode
    rounding_approximation <- 0
  }
  else
  {
    # max number of digits in ANTARES
    rounding_approximation <- 6
  }
  
  if(length(load_factor_profile) == 1)
  { 
    # in that case there is no capacity profile
    # we return the capacity without rounding it to the nearest integer
    # Careful : in accurate uc-mode, this might cause an ANTARES error (infeasible problem)
    #           but as this problem has not been met yet with constant capacity value
    #           the rounding is not made
    return(installed_capacity * load_factor_profile)
  }
  if(length(load_factor_profile) == 8760)
  {
    producible_energy <- sum(load_factor_profile * installed_capacity)
    capacity_profile <- .round_with_threshold(load_factor_profile * installed_capacity, 0.5, digits = rounding_approximation)
    
    # now, we want to find a rounding option which bring to a yearly producible energy close to this one
    diff <- (sum(capacity_profile) - producible_energy) / producible_energy
    
    round_threshold <- 0.5
    min_threshold <- 0
    max_threshold <- 1
    max_it <- 200
    it <- 1
    
    # iterative dichotomic approach
    while(it < max_it && abs(diff) > 10^-8 && (max_threshold - min_threshold) > 10^-8)
    {
      if(diff >= 0) # too big
      {
        min_threshold <- round_threshold
        round_threshold <- min_threshold + (max_threshold - min_threshold)/2
      }
      else
      {
        max_threshold <- round_threshold
        round_threshold <- min_threshold + (max_threshold - min_threshold)/2
      }
      capacity_profile <- .round_with_threshold(load_factor_profile * installed_capacity, round_threshold, digits = rounding_approximation)
      diff <- (sum(capacity_profile) - producible_energy) / producible_energy
      it <- it+1
      #cat("it : ", it, " /// min = ",  min_threshold, "   max = ", max_threshold, " th = ", round_threshold, "   diff = ", diff,   "  \n")
    }
    return(capacity_profile)
  }
  
}

#'  round with a threshold which might not be 0.5
#'
#' 
#' @param x  
#' (Vector of) value to round
#' @param t  
#' rounding threshold
#' @param digits 
#' rounding after how many digits
#' @return rounded value
#'
#' @noRd
#'
.round_with_threshold <- function(x,t, digits = 0)
{
  if(any(x<0)){ stop("x must be positive")}
  x <- x * 10^digits
  d <- x - trunc(x)
  x[d >= t] <- ceiling(x[d >= t])
  x[d < t] <- floor(x[d < t])
  return(x/10^digits)
}

