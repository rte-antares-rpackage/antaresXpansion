#' Set period on which we want the ANTARES simulation to run.
#' Update "first day" and "last day" parameters of the ANTARES study.  
#' 
#' 
#' @param weeks
#'   vector of weeks identiers which should be simulated
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return 
#' 
#' @import assertthat antaresRead
#' @export
#' 
set_simulation_period <- function(weeks, opts = simOptions())
{
  # weeks should be successives 
  assert_that(all(weeks == seq(first(weeks), last(weeks))))
  assert_that(all(weeks <= 52))
  
  # change parameters of the study
  set_first_day(7*(first(weeks) - 1) + 1, opts)
  set_last_day(7*(last(weeks) - 1), opts)
}