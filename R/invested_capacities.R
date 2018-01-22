#' Return data frame of initial values of invested capacities for all candidates
#' 
#'   
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param horizon
#'   representated year in the Antares Study
#'
#' @return 
#' Returns a vector of link name
#' @noRd
initiate_candidate_capacities <- function(candidates, horizon)
{
  n_candidate <- length(candidates)
  
  # set initial value to each investment candidate 
  # (here put to closest multiple of unit-size below max_invest/2)
  cap <- sapply(candidates, FUN = function(c){
    if(c$unit_size > 0)
    {
      out <- floor(c$max_invest/2/c$unit_size) * c$unit_size
      out <- max(0, min(c$max_invest, out))
    }
    else
    { out <- c$max_invest/2}
    return(out)})
  
  # build invested capacities data.frame
  invested_capacities <- data.frame(
    it = rep(1, n_candidates),
    year = rep(horizon,n_candidates),
    candidate = sapply(candidates, FUN = function(c){c$name}),
    value = cap
  )
  
  return(invested_capacities)
}


#' get invested capacity
#'   
#' @param invested_capacity
#'   invested_capacity data.frame
#' @param it
#'   iteration number
#' @param candidate
#'  candidate name
#' @param horizon
#'   simulated year
#'
#' @return 
#' Returns numerical value with invested capacity for that candidate/it/horizon
#' @importFrom assertthat assert_that
#' @noRd
get_capacity <- function(invested_capacity, it, candidate, horizon = NULL)
{
  
  itb <- it
  candidateb <- candidate
  if(is.null(horizon))
  {
    out <- subset(invested_capacity, candidate == candidateb & it == itb)$value
  }
  else
  {
    out <- subset(invested_capacity, candidate == candidateb & it == itb & year == horizon)$value
  }
  
  assertthat::assert_that(length(out) == 1)
  return(out)
}






