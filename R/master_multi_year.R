
#' Initiate master problem for multi-year investment problem
#' 
#' \code{initiate_master_multi_year} copy the AMPL files into the temporary 
#' folder of the expansion planning optimisation and create the other input
#' and output file of the master problem
#' 
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param exp_options 
#'   list of options related to the expansion planning, as returned
#'   by the function \code{\link{read_options}}
#' @param studies
#'   list of simulated years with their according antares studies, as 
#'   returned by the function \code{\link{read_studies}}
#' @param tmp_folder
#'   temporary folder where to write ampl files
#'
#' @return This function does not return anything.
#' 
#' @importFrom antaresRead simOptions
#' @importFrom assertthat assert_that
#' @importFrom antaresEditObject getPlaylist
#' @noRd
#' 
initiate_master_multi_year <- function(candidates, exp_options , studies, tmp_folder)
{
  # ampl file names (stored in inst folder)
  run_file <- "ampl/master_multiy_run.ampl"
  mod_file <- "ampl/master_multiy_mod.ampl"
  dat_file <- "ampl/master_multiy_dat.ampl"
  
  # master input/output files (interface with AMPL is ensured with .txt files)
  in_out_files <- list()
  in_out_files$horizon <- "in_horizon.txt"
  in_out_files$mc <- "in_mc.txt"
  in_out_files$w <- "in_week.txt"
  in_out_files$candidates <- "in_candidates.txt"
  in_out_files$candidate_costs <- "in_candidate_costs.txt"
  in_out_files$iterations  <- "in_iterations.txt"
  in_out_files$z0 <- "in_z0.txt"
  in_out_files$avg_rentability <- "in_avgrentability.txt"
  in_out_files$yearly_rentability <- "in_yearlyrentability.txt"
  in_out_files$weekly_rentability <- "in_weeklyrentability.txt"
  in_out_files$avg_cuts <- "in_avgcuts.txt"
  in_out_files$yearly_cuts <- "in_yearlycuts.txt"
  in_out_files$weekly_cuts <- "in_weeklycuts.txt"
  in_out_files$options <- "in_options.txt"
  in_out_files$sol_master <- "out_solutionmaster.txt"
  in_out_files$underestimator <- "out_underestimator.txt"
  in_out_files$log <- "out_log.txt"
  in_out_files$dual_averagecut <- "out_dualaveragecut.txt"
  in_out_files$dual_yearlycut <- "out_dualyearlycut.txt"
  in_out_files$dual_weeklycut <- "out_dualweeklycut.txt"
  in_out_files$theta <- "out_theta.txt"
  
  # check if temporary folder exists, if not create it
  if(!dir.exists(tmp_folder))
  {
    dir.create(tmp_folder)
  }
  
  # copy AMPL files into the temporary folder
  run_file <- system.file(run_file, package = "antaresXpansion")
  mod_file <- system.file(mod_file, package = "antaresXpansion")
  dat_file <- system.file(dat_file, package = "antaresXpansion")
  
  assertthat::assert_that(file.copy(from = run_file, to = tmp_folder, overwrite = TRUE))
  assertthat::assert_that(file.copy(from = mod_file, to = tmp_folder, overwrite = TRUE))
  assertthat::assert_that(file.copy(from = dat_file, to = tmp_folder, overwrite = TRUE))
  
  # create empty in_out files
  lapply(in_out_files, FUN = function(x, folder){file.create(paste0(folder, "/", x))}, folder = tmp_folder)
  
  # fill files which will be similar for every iteration of the benders decomposition
  # 1 - in_nmc.txt
  script <- ""
  for(i in 1:studies$n_simulated_years)
  {
    script_t <- paste0(studies$mc_years[[i]], collapse = paste0("\n", studies$simulated_years[[i]], " "))
    if (i==1) script <- paste0(studies$simulated_years[[i]], " ", script_t)
    else script <- paste0(script, "\n", studies$simulated_years[[i]], " ", script_t)
  }
  
  write(script, file = paste0(tmp_folder, "/", in_out_files$mc))
  
  # 2 - in_nw.txt
  script <- ""
  for(i in 1:studies$n_simulated_years)
  {
    script_t <- paste0(studies$weeks[[i]], collapse = paste0("\n", studies$simulated_years[[i]], " "))
    if (i==1) script <- paste0(studies$simulated_years[[i]], " ", script_t)
    else script <- paste0(script, "\n", studies$simulated_years[[i]], " ", script_t)
  }
  
  write(script, file = paste0(tmp_folder, "/", in_out_files$w))
  
  
  # 3 - in_candidates.txt
  script <- ""
  for(i in 1:length(candidates))
  {
    script <- paste0(script, candidates[[i]]$name, " ", 
                     candidates[[i]]$unit_size, " ", 
                     candidates[[i]]$max_unit, " ",
                     tolower(as.character(candidates[[i]]$relaxed)))
    if(i != length(candidates))
    {
      script <- paste0(script, "\n")
    }
  }
  write(script, file = paste0(tmp_folder, "/", in_out_files$candidates))
  
  
  # 4 - in_candidate_costs.txt
  script <- ""
  for(i in 1:length(candidates))
  {
    for(s in 1:studies$n_simulated_years)
    {
      script <- paste0(script, candidates[[i]]$name, " ", 
                     studies$simulated_years[s], " ", 
                     candidates[[i]]$investment_cost[s], " ", 
                     candidates[[i]]$operation_and_maintenance_cost[s]) 
                     #, " ", candidates[[i]]$mothball_cost[s])
      
      if(i != length(candidates) | s != studies$n_simulated_years)
      {
        script <- paste0(script, "\n")
      }
    }
  }
  write(script, file = paste0(tmp_folder, "/", in_out_files$candidate_costs))
  
  # 5 - in_horizon.txt
  write(paste0(studies$simulated_years, collapse = " "), file = paste0(tmp_folder, "/", in_out_files$horizon))
  

}


#' Solver master problem for multi-year investment
#' 
#' \code{solver_master_multi_year} execute the AMPL file master_multiy_run.ampl
#' located in the temporary folder of the current expansion 
#' planning optimisation
#' 
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param relax_integrality
#'   logical, indicating whether (TRUE) or not (FALSE) the integer variables
#'   should be relaxed
#' @param ampl_path
#'   Character containing the path to the ampl.exe file
#'   
#' @return This function does not return anything.
#' 
#' @importFrom antaresRead simOptions
#' @importFrom assertthat assert_that
#' @noRd
#' 
solve_master_multi_year <- function(opts = antaresRead::simOptions(), relax_integrality = FALSE, ampl_path = NULL)
{
  tmp_folder <- paste(opts$studyPath,"/user/expansion/temp",sep="")
  
  if(relax_integrality)
  {
    write("option relax_integrality 1;", file = paste0(tmp_folder, "/in_options.txt"))
  }
  else
  {
    write("option relax_integrality 0;", file = paste0(tmp_folder, "/in_options.txt"))
  }
  
  
  assertthat::assert_that(file.exists(paste0(tmp_folder, "/master_multiy_run.ampl")))
  assertthat::assert_that(file.exists(paste0(tmp_folder, "/master_multiy_mod.ampl")))
  assertthat::assert_that(file.exists(paste0(tmp_folder, "/master_multiy_dat.ampl")))
  
  if(is.null(ampl_path))
  {
    cmd <- paste0('', substr(tmp_folder, 1, 2), ' & cd "', tmp_folder, '" & ampl "', tmp_folder, '/master_run.ampl" ')
  }
  else
  {
    assertthat::assert_that(file.exists(ampl_path))
    cmd <- paste0('', substr(tmp_folder, 1, 2), ' & cd "', tmp_folder, '" & "', ampl_path ,'" "', tmp_folder, '/master_run.ampl" ')
  }
  a <- shell(cmd, wait = TRUE, intern = TRUE)
}
