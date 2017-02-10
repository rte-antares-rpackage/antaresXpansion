#' Smart selection of the weeks to simuate
#' 
#' week_selection is a function which select the MC years and 
#' weeks which will be simulated during this iteration
#' 
#' @param current_it
#'   list of current iteration characteristics
#' @param mc_years
#'   vector of all the Monte Carlo years identifier 
#' @param weeks
#'   vector of all the weeks identifier
#' @param tmp_folder
#'   temporary folder of the benders decomposition
#' @param exp_options 
#'   list of options related to the expansion planning, as returned
#'   by the function \code{\link{read_options}}
#'   
#' @return 
#' updated current iteration characteristics
#' 
#' @import assertthat
#' 
week_selection <- function(current_it, mc_years, weeks, tmp_folder, exp_options)
{
  make_full_iteration <- FALSE
  # some constant parameters of the function
  max_iterations_between_full <- 5
  

  # if we are in the first iteration, run all the weeks
  if(current_it$n == 1)
  {
    make_full_iteration <- TRUE
  }
  
  # if full iteration was needed, run all the weeks
  if(current_it$need_full)
  {
    make_full_iteration <- TRUE
  }
  
  # if full iteration wasn't run in the last iterations, run all weeks
  if(current_it$last_full + max_iterations_between_full <  current_it$n)
  {
    make_full_iteration <- TRUE
  } 
  
  ### temporary
  current_it$cut_type <- sample(c("average", "yearly", "weekly"),1)
  
  if(current_it$cut_type == "average")
  {
    make_full_iteration <- TRUE
  } 
  
  ### enf of temporary
  
  # full iteration
  if(make_full_iteration)
  {
    current_it$full <- TRUE
    current_it$weeks <- weeks
    current_it$mc_years <- mc_years
    current_it$need_full <- FALSE
    current_it$last_full <- current_it$n
    return(current_it)
  }
  
  # for now, to test the robustness of the other function, select randomly
  current_it$cut_type <- sample(c("average", "yearly", "weekly"),1)

  if(current_it$cut_type == "yearly")
  {
    current_it$weeks <- weeks
    current_it$mc_years <- sort(unique(sample(mc_years, sample(1:length(mc_years),1))))
    current_it$full <- FALSE
  } 
  if(current_it$cut_type == "weekly")
  {
    current_it$mc_years <- sort(unique(sample(mc_years, sample(1:length(mc_years),1))))
    rand_weeks <- sample(weeks,2)
    current_it$weeks <- min(rand_weeks):max(rand_weeks)
    current_it$full <- FALSE
  } 
  
  
  return(current_it)
}