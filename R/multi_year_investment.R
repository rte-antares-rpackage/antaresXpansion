#' Multi-year investment optimization
#' 
#' 
#' @param path_solver
#'   Character containing the Antares Solver path
#' @param directory_path
#'   Character containing the directory path in which are stored the studies.ini, candidates.ini and settings.ini files
#' @param display
#'   Logical. If \code{TRUE} the advancement of the optimisation algorithm
#'   if displayed in the console
#' @param report
#'   Logical. If \code{TRUE} an html report of the expansion results will
#'   be generated.
#' @param clean
#'   Logical. If \code{TRUE} the output of the ANTARES simulations run by the
#'   package will be deleted (except for the output of the simulation which brings
#'   to the best solution).
#' @param parallel
#'   Logical. If \code{TRUE} the ANTARES simulations will be run in parallel mode (Work
#'   only with ANTARES v6.0.0 or more). In that case, the number of cores used by the simulation
#'   is the one set in advanced_settings/simulation_cores (see ANTARES interface).
#'
#' @return 
#' 
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions setSimulationPath getAreas
#' @importFrom rmarkdown render
#' @importFrom utils packageVersion tail
#' @importFrom antaresEditObject setPlaylist getPlaylist
#' @export
#' 
multi_year_investment <- function(path_solver, directory_path = getwd(), display = TRUE, report = TRUE, clean = TRUE, parallel = TRUE)
{
  
  
  # ---- 0. initialize benders iteration ----
  
  # read studies.ini file
  studies <- read_studies(paste0(directory_path,"/studies.ini"))
  
  # save current settings of the ANTARES studies into a temporary file
  lapply(studies$opts, FUN = save_general_settings)
  # reset options of the ANTARES studies to their initial values when the function ends
  on.exit(lapply(studies$opts, FUN = restore_general_settings)) 
  
  # read expansion planning options and investment candidates
  for(stu in studies$opts)
  {
    exp_options <- read_options(file = paste0(directory_path,"/settings.ini"), stu)
    candidates <- read_candidates(file = paste0(directory_path,"/candidates.ini"), studies = studies, opts = stu)
  }
    
  n_candidates <- length(candidates)
  assertthat::assert_that(n_candidates > 0)
  
  # if all investments are distributed (no integer variables), relax master problem
  if(all(sapply(candidates, FUN = function(c){return(c$relaxed)})))
  {
    exp_options$master <- "relaxed"
  }
  
  # set ANTARES study options
  mapply(set_antares_options, opts = studies$opts, 
         MoreArgs = list(benders_options = exp_options, candidates = candidates))
  

  # check that the studies are appropriately set for the expansion problem
  for(stu in studies$opts) assertthat::assert_that(benders_check(candidates, stu))
  
  # initiate a "few" parameters
  studies$n_w <- list()
  studies$n_mc <- list()
  studies$weeks <- list()
  studies$mc_years <- list()
  studies$all_areas <- list()
  
  for(s in 1:studies$n_simulated_years)
  {
    first_sim_week <- 1 + ceiling((studies$opts[[s]]$parameters$general$simulation.start - 1)/7)
    studies$n_w[[s]] <- floor((studies$opts[[s]]$parameters$general$simulation.end - studies$opts[[s]]$parameters$general$simulation.start + 1)/7) # number of weeks 
    studies$weeks[[s]] <- first_sim_week:(first_sim_week + studies$n_w[[s]] - 1) # identifier of weeks to simulate for all expansion planning optimisation
    studies$mc_years[[s]] <- antaresEditObject::getPlaylist(studies$opts[[s]]) # identifier of mc years to simulate for all expansion planning optimisation
    studies$n_mc[[s]] <- length(studies$mc_years[[s]]) # number of mc_years
    studies$all_areas[[s]] <- antaresRead::getAreas(opts = studies$opts[[s]]) # all area of the first
  }
 
  
  has_converged <- FALSE # has the benders decomposition converged ? not yet
  best_solution <- NA  # best solution identifier
  relax_integrality <- exp_options$master %in% c("relaxed", "integer") # should integer problem be relaxed ?
  unique_key <- paste(sample(c(0:9, letters), size = 3, replace = TRUE),collapse = "") # unique key used in output names
  first_iteration <- TRUE # is it the first iteration ?
  
  # initiate text files to communicate with master problem
  # and copy AMPL file into the temporary file 
  
  ## TO DO
  tmp_folder <- paste0(directory_path,"/temp")   # temporary folder
  initiate_master_multi_year(candidates, exp_options, studies, tmp_folder)
  
  
  # create output structure 
  x <- list()
  x$invested_capacities <- initiate_candidate_capacities(candidates, studies$n_simulated_years)
  x$costs <- data.frame(row.names = c("it", "year", "investment_costs", "operation_costs", "overall_costs"))
  x$rentability <- data.frame(row.names = sapply(candidates, FUN = function(c){c$name}))
  x$iterations <- list()
  x$digest <- list()
  
  # create iteration structure
  current_it <- list()
  current_it$n <- 1  # iteration number
  current_it$id <- paste0("it",current_it$n)  # iteration identifier
  current_it$full <- TRUE  # is it an iteration in which we simulate all weeks and all MC years ?
  current_it$mc_years <- studies$mc_years # identidier of mc years to simulate at this current iteration
  current_it$weeks <- studies$weeks # identidier of weeks to simulate at this current iteration
  current_it$cut_type <- exp_options$cut_type # type of cut for this iteration (average, weekly, yearly)
  current_it$need_full <- FALSE # is a complete iteration needed for next step ?
  current_it$last_full <- 1 # last iteration with full simulation

}