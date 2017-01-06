#' Run an Antares Simulation
#' 
#' \code{run_simulation} is a function which runs an ANTARES study
#' in economic mode
#' 
#' @param name
#'   Name of the simulation 
#' @param mode
#'   Simulation mode, can take value "economy", "adequacy" or "draft"
#' @param path_solver
#'   Character containing the Antares Solver path
#' @param show_output_on_console
#'   Logical, indicating whether to capture the ANTARES log and show 
#'   it on the R console
#' @param wait
#'   Logical, indicating whether the R interpreter should wait for the 
#'   simulation to finish, or run it asynchronously. 
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' 
#' @return 
#' The function does not return anything. It is  used to launch an 
#' ANTARES simulation
#' #' 
#' @import assertthat antaresRead
#' @export
#' 
run_simulation <- function(name, mode = "economy", path_solver, wait = TRUE, show_output_on_console = FALSE, opts = simOptions(), ...)
{
  # a few checks
  name = tolower(name)
  assert_that(file.exists(solverPath))
  assert_that(mode %in% c("economy", "adequacy", "draft"))
  
  #Launch simulation
  cmd <- '"%s" "%s" -n "%s" --"%s"'
  cmd <- sprintf(cmd, path_solver, opts$studyPath, name, mode)
  
  system(cmd, ignore.stdout = TRUE, wait = wait,  show.output.on.console = show_output_on_console)
}
