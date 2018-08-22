
#' Initiate master problem
#' 
#' \code{initiate_master} copy the AMPL files into the temporary 
#' folder of the expansion planning optimisation and create the other input
#' and output file of the master problem
#' 
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param exp_options 
#'   list of options related to the expansion planning, as returned
#'   by the function \code{\link{read_options}}
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return This function does not return anything.
#' 
#' @importFrom antaresRead simOptions
#' @importFrom assertthat assert_that
#' @importFrom antaresEditObject getPlaylist
#' @noRd
#' 
initiate_master <- function(candidates, exp_options , opts = antaresRead::simOptions())
{
  # ampl file names (stored in inst folder)
  run_file <- "ampl/master_run.ampl"
  mod_file <- "ampl/master_mod.ampl"
  dat_file <- "ampl/master_dat.ampl"
  opt_file <- "ampl/master_default_options.txt"
  
  # master input/output files (interface with AMPL is ensured with .txt files)
  in_out_files <- list()
  in_out_files$mc <- "in_mc.txt"
  in_out_files$w <- "in_week.txt"
  in_out_files$candidates <- "in_candidates.txt"
  in_out_files$iterations  <- "in_iterations.txt"
  in_out_files$z0 <- "in_z0.txt"
  in_out_files$avg_rentability <- "in_avgrentability.txt"
  in_out_files$yearly_rentability <- "in_yearlyrentability.txt"
  in_out_files$weekly_rentability <- "in_weeklyrentability.txt"
  in_out_files$avg_cuts <- "in_avgcuts.txt"
  in_out_files$yearly_cuts <- "in_yearlycuts.txt"
  in_out_files$weekly_cuts <- "in_weeklycuts.txt"
  in_out_files$options <- "in_options.txt"
  in_out_files$y_weights <- "in_yweights.txt"
  in_out_files$sol_master <- "out_solutionmaster.txt"
  in_out_files$underestimator <- "out_underestimator.txt"
  in_out_files$log <- "out_log.txt"
  in_out_files$dual_averagecut <- "out_dualaveragecut.txt"
  in_out_files$dual_yearlycut <- "out_dualyearlycut.txt"
  in_out_files$dual_weeklycut <- "out_dualweeklycut.txt"
  in_out_files$theta <- "out_theta.txt"
  in_out_files$capacity_bounds <- "in_out_capacitybounds.txt"
  
  # check if temporary folder exists, if not create it
  tmp_folder <- paste(opts$studyPath,"/user/expansion/temp",sep="")
  if(!dir.exists(tmp_folder))
  {
    dir.create(tmp_folder)
  }
  
  # copy AMPL files into the temporary folder
  run_file <- system.file(run_file, package = "antaresXpansion")
  mod_file <- system.file(mod_file, package = "antaresXpansion")
  dat_file <- system.file(dat_file, package = "antaresXpansion")
  opt_file <- system.file(opt_file, package = "antaresXpansion")
  
  assertthat::assert_that(file.copy(from = run_file, to = tmp_folder, overwrite = TRUE))
  assertthat::assert_that(file.copy(from = mod_file, to = tmp_folder, overwrite = TRUE))
  assertthat::assert_that(file.copy(from = dat_file, to = tmp_folder, overwrite = TRUE))
  assertthat::assert_that(file.copy(from = opt_file, to = tmp_folder, overwrite = TRUE))
  
  # create empty in_out files
  lapply(in_out_files, FUN = function(x, folder){file.create(paste0(folder, "/", x))}, folder = tmp_folder)
  
  # fill files which will be similar for every iteration of the benders decomposition
  
  # 1 - in_nmc.txt
  mc <- antaresEditObject::getPlaylist(opts)
  write(paste0(mc, collapse = " "), file = paste0(tmp_folder, "/", in_out_files$mc))
  
  # 2 - in_nw.txt
  n_w <- floor((opts$parameters$general$simulation.end - opts$parameters$general$simulation.start + 1)/7)
  n_w <- min(n_w, 52)
  #     number of week does not necessarily starts at 1
  #     it depends on the beginning of the simulated period (opts$parameters$general$simulation.start)
  weeks <- (ceiling((opts$parameters$general$simulation.start - 1) /7) + 1) : (ceiling((opts$parameters$general$simulation.start - 1) /7) + n_w) 
  
  write(paste0(weeks, collapse = " "), file = paste0(tmp_folder, "/", in_out_files$w))
  
  # 3 - in_candidates.txt
  script <- ""
  for(i in 1:length(candidates))
  {
    script <- paste0(script, candidates[[i]]$name, " ", 
                     candidates[[i]]$cost, " ", 
                     candidates[[i]]$unit_size, " ", 
                     candidates[[i]]$max_unit, " ",
                     tolower(as.character(candidates[[i]]$relaxed)))
    if(i != length(candidates))
    {
      script <- paste0(script, "\n")
    }
  }
  write(script, file = paste0(tmp_folder, "/", in_out_files$candidates))
  
  
  # 4 - in_solver.txt
  change_option_master(option_name = "solver", option_value = exp_options$solver, opts = opts)
  
  # 5 - in_yweights.txt
  if(all(is.na(exp_options$y_weights)))
  {
    weights_y <- rep(1, length(mc))/length(mc)
  }
  else{weights_y <- exp_options$y_weights}
  script <- sapply(1:length(mc), FUN = function(n){paste0(mc[n], " ", weights_y[n])})
  write(paste0(script, collapse = "\n"), file = paste0(tmp_folder, "/", in_out_files$y_weights))
  
}


#' modify master option
#' 
#' \code{change_option_master} modify the options of the 
#' master problem
#' 
#' @param option_name
#'    vector of option names
#' @param option_value
#'    vector of option values
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'   
#' @return This function does not return anything.
#' 
#' @importFrom antaresRead simOptions
#' @importFrom assertthat assert_that
#' @noRd
change_option_master <- function(option_name, option_value, opts = antaresRead::simOptions())
{
  
  option_file_path <- paste0(opts$studyPath,"/user/expansion/temp/in_options.txt")

  assertthat::assert_that(length(option_name) == length(option_value))
  assertthat::assert_that(file.exists(option_file_path))
  
  # read option file
  if(file.size(option_file_path) == 0) options <- data.frame(name = c(), value = c())
  else options <- read.table(option_file_path, header = FALSE, col.names = c("name", "value"), colClasses = "character")
  
  # for each new option
  for(n in 1:length(option_name))
  {
    # if option was already present in the option file, update it
    if(any(option_name[n] == options$name))
    {
      options$value[options$name == option_name[n]] <- option_value[n]
    }
    # if option was not present in the option file, add it
    else
    {
      options <- rbind(options, data.frame(name = option_name[n], value = option_value[n]))
    }
  }
  # write new options
  write.table(options, file = option_file_path, append = FALSE, sep = " ", col.names = FALSE, row.names = FALSE, quote = FALSE)
  
}

#' Solver master problem
#' 
#' \code{solver_master} execute the AMPL file master_run.ampl
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
solve_master <- function(opts = antaresRead::simOptions(), relax_integrality = FALSE, ampl_path = NULL)
{
  tmp_folder <- paste(opts$studyPath,"/user/expansion/temp",sep="")
  
  if(relax_integrality) change_option_master(option_name = "relaxed", option_value = 1, opts = opts)
  else change_option_master(option_name = "relaxed", option_value = 0, opts = opts)

  
  assertthat::assert_that(file.exists(paste0(tmp_folder, "/master_run.ampl")))
  assertthat::assert_that(file.exists(paste0(tmp_folder, "/master_mod.ampl")))
  assertthat::assert_that(file.exists(paste0(tmp_folder, "/master_dat.ampl")))
  
  
  # launch master_run.ampl
  if(tolower(.Platform$OS.type) == "windows") 
  {
    # Windows version
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
  } else 
  {
    # Linux version
    if(is.null(ampl_path))
    {
      cmd <- paste0('cd "', tmp_folder, '" & ampl "', tmp_folder, '/master_run.ampl" ')
    }
    else
    {
      assertthat::assert_that(file.exists(ampl_path))
      cmd <- paste0('cd "', tmp_folder, '" & "', ampl_path ,'" "', tmp_folder, '/master_run.ampl" ')
    }
    a <- system(cmd, wait = TRUE, intern = TRUE)
  }

    # check if AMPL returned an error
  if(any(grepl("error", tolower(a)) | grepl("cannot", tolower(a))))
  {
     stop("master problem returned the following error: ", a)
  }
}