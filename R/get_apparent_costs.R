#' Compute apparent cost of investment candidates 
#' 
#' 
#' @param candidates
#'   list of investment candidate with their characteristics, as 
#'   returned by the function \code{\link{read_candidates}}
#' @param settings
#'   list of settings for the optimization algorithm, as 
#'   returned by the function \code{\link{read_options}}
#' @param studies   
#'   list of simulated years with their according antares studies, as 
#'   returned by the function \code{\link{read_studies}}
#'
#' @return 
#' Returns a list containing the different investment candidates. 
#' 
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions
#' @noRd
#' 
get_apparent_cost <- function(candidates, settings ,studies)
{
 
  # get horizons
  horizons <- as.numeric(sapply(studies, FUN = function(x){x$year}))
  
  # get actuation rate coefficient
  q <- 1/(1+settings$discount_rate)
  
  
  # for each candidates 
  for(ca in 1:length(candidates))
  {
    c <- candidates[[ca]]
    # if expected rentability is not defined, it is equal to the discount rate
    if(is.na(c$expected_rentability)) c$expected_rentability <- settings$discount_rate
    
    # compute expected_rentability_overcost
    qc <- 1/(1+c$expected_rentability)
    expected_rentability_overcost <- (1-qc)*(1-q^c$lifetime) / ((1-q) * (1-qc^c$lifetime))
    
    
    # apparent costs initialisation
    candidates[[ca]]$apparent_cost <- rep(NA, length(horizons))

    
    for(t in 1:length(horizons))
    {
       # if the candidate is built in horizons[t], which are the other time cut (horizons[t2], t2>t)
       # for which the investment has not yet reached it lifetime.
      
       active_years <- horizons[horizons >= horizons[t] & horizons <= horizons[t] + c$lifetime - 1]
      
       # in comparison to horizons[t]
       active_years <- active_years - horizons[t]
      
       # compute ratio of costs on active years over total cost
       ratio <- sum(q^active_years) *(1-q) / (1-q^c$lifetime)
       
       # apparent_cost
       candidates[[ca]]$apparent_cost[t] <- ratio * expected_rentability_overcost * c$investment_cost[t]
    }
  }
  return(candidates)
}
