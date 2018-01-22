
#' Check the convergence of the relaxed expansion problem
#' 
#' 
#' @param best_sol
#' cost of best solution
#' @param best_under
#' cost of best under estimator
#' @param benders_options
#'   list of benders decomposition options, as returned by
#'   \code{\link{read_options}}.
#'   
#' @return logical, indicating weither or not the relaxed problem has converged within the optimality gap
#' 
#' @importFrom assertthat assert_that
#' @noRd
convergence_relaxed <-  function(best_sol, best_under, benders_options)
{
  # is the optimality gap given in percentage or absolute value ?
  if(grepl("%$", benders_options$optimality_gap))
  {
    # if yes
    # remove the % at the end, convert in numeric, and multiply by the best current solution
    opt_gap <- as.numeric(gsub("%$", "", benders_options$optimality_gap)) * best_sol / 100 
  }
  else
  {
    # if no, optimality gap is already given in absolute value
    opt_gap <- benders_options$optimality_gap
  }
  
  # is the relaxed optimality gap given in percentage or absolute value ?
  if(grepl("%$", benders_options$relaxed_optimality_gap))
  {
    opt_gap_r <- as.numeric(gsub("%$", "", benders_options$relaxed_optimality_gap)) * best_sol / 100
  }
  else
  {
    opt_gap_r <- benders_options$relaxed_optimality_gap
  }
  
  # check that optimality gaps are not NA
  assertthat::assert_that(!is.na(opt_gap))
  assertthat::assert_that(!is.na(opt_gap_r))
  
  # is convergence of the relaxed problem reached ?
  return((best_sol - best_under) <= max(opt_gap * 1.1, opt_gap_r))
}



#' Check the convergence of the expansion problem
#' 
#' 
#' @param best_sol
#' cost of best solution
#' @param best_under
#' cost of best under estimator
#' @param benders_options
#'   list of benders decomposition options, as returned by
#'   \code{\link{read_options}}.
#'   
#' @return logical, indicating weither or not the expansion problem has converged within the optimality gap
#' 
#' @importFrom assertthat assert_that
#' @noRd
convergence <-  function(best_sol, best_under, benders_options)
{
  # is the optimality gap given in percentage or absolute value ?
  if(grepl("%$", benders_options$optimality_gap))
  {
    # if yes
    # remove the % at the end, convert in numeric, and multiply by the best current solution
    opt_gap <- as.numeric(gsub("%$", "", benders_options$optimality_gap)) * best_sol / 100
  }
  else
  {
    # if no, optimality gap is already given in absolute value
    opt_gap <- benders_options$optimality_gap
  }
  
  # check that optimality gap is not NA
  assertthat::assert_that(!is.na(opt_gap))
  
  # is convergence of the relaxed problem reached ?
  return((best_sol - best_under) <= opt_gap)
}


#' Check if installed capacities have evolved since previous iteration
#' 
#' 
#' @param benders_sol
#' capacities returned by master problem
#' @param installed_capacities
#' data.table of installed capacities
#' @param tol
#' tolerance
#' @return logical, indicating weither or not the expansion problem has converged within the optimality gap
#' 
#' @importFrom assertthat assert_that
#' @noRd
#' 
have_capacities_changed <-function(benders_sol, installed_capacities, tol){
  
  assertthat::assert_that(tol >=0 )
  assertthat::assert_that(is.data.frame(installed_capacities))
  assertthat::assert_that(is.data.frame(benders_sol))
  assertthat::assert_that( all(c("it", "candidate", "value") %in% names(installed_capacities)) )
  assertthat::assert_that( all(c("candidate", "value")  %in%  names(benders_sol)))
  
  # get last iteration number
  it_last <- max(installed_capacities$it)
  
  # focus only on capacities of the last iteration
  capa_last_it <- subset(installed_capacities, it == it_last)
  candidates <- unique(benders_sol$candidate)
  
  # check differences between the two tables
  for(ca in candidates)
  {
    if(abs(get_capacity(installed_capacities, it = it_last, candidate = ca) - subset(benders_sol, candidate == ca)$value) > tol)
    { 
      return(FALSE)
    }
  }
  return(TRUE)
}
