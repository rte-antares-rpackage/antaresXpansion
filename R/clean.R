
#' Delete the output of an ANTARES study
#' 
#' 
#' @param simulation_name
#'   name of the simulation whose output will be deleted
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return 
#' 
#' @importFrom antaresRead simOptions 
#' @export
#' 
delete_antares_output <- function(simulation_name, opts = antaresRead::simOptions())
{
  complete_path <- paste0(opts$studyPath, "/output/", simulation_name)
  if(!dir.exists(complete_path))
  {
    warning("Antares simulation (", simulation_name, ") not found")
  }
  else
  {
    unlink(complete_path, recursive = TRUE)
  }
}



#' Delete the output of the benders iterations which are not usefull anymore
#' 
#' 
#' @param best_solution
#'   identifier of the best solution so far 
#'   (the output related to this iteration will not be removed)
#' @param unique_key
#'   unique string to identify the output of the current expansion problem
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return 
#' 
#' @importFrom antaresRead simOptions 
#' @importFrom assertthat assert_that
#' @export
#' 
clean_output_benders <- function(best_solution, unique_key, opts = antaresRead::simOptions())
{
  # check existence of output path
  output_path <- paste0(opts$studyPath, "/output/")
  if(!dir.exists(output_path))
  {
    return()
  }
  
  # get simulation name list
  list_simu <- list.dirs(output_path, recursive = FALSE, full.names = FALSE)
  
  # check that the best solution has been kept
  best_sol_name <- paste0("expansion-benders-", unique_key, "-it", best_solution, "$")
  assertthat::assert_that(length(grep(best_sol_name, list_simu)) > 0)
  
  
  #list simulations to delete : 
  #   - with the same unique key
  #   - except the best solution
  
  id_to_remove <- grepl(paste0("expansion-benders-", unique_key, "-it"), list_simu)
  id_to_keep <-  grepl(best_sol_name, list_simu)
  list_simu_to_remove <- list_simu[id_to_remove & !id_to_keep]
  
  # delete them
  if(length(list_simu_to_remove) == 0) {return()}
  
  for(sim in list_simu_to_remove)
  {
    delete_antares_output(sim, opts = opts)
  }
}
  