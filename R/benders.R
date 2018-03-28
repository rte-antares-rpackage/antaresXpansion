#' Launch benders decomposition
#' 
#' 
#' @param path_solver
#'   Character containing the Antares Solver path
#' @param display
#'   Logical. If \code{TRUE} the advancement of the benders decomposition
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
#' @param recovery_mode
#'   Logical. If \code{TRUE} will launch the benders decomposition keeping the cut files already saved
#'   in the temporary folder of the ANTARES study. Can be used if a previous simulation has crashed.
#' @param ampl_path
#'   Character containing the path to the ampl.exe file
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
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
benders <- function(path_solver, display = TRUE, report = TRUE, clean = TRUE, parallel = TRUE, recovery_mode = FALSE, ampl_path = NULL, opts = antaresRead::simOptions())
{
  # ---- 0. initialize benders iteration ----

    # save current settings of the ANTARES study into a temporary file
  assertthat::assert_that(file.exists(paste0(opts$studyPath, "/settings/generaldata.ini")))
  assertthat::assert_that(file.copy(from = paste0(opts$studyPath, "/settings/generaldata.ini"), 
            to = paste0(opts$studyPath, "/settings/generaldata_tmpsvg.ini"),
            overwrite = TRUE))
  
  # read expansion planning options
  exp_options <- read_options(file = paste(opts$studyPath,"/user/expansion/settings.ini",sep=""), opts)
  
  # read investment candidates file
  candidates <- read_candidates(file = paste(opts$studyPath,"/user/expansion/candidates.ini",sep=""), opts)
  n_candidates <- length(candidates)
  assertthat::assert_that(n_candidates > 0)
  
  # if all investments are distributed (no integer variables), relax master problem
  if(all(sapply(candidates, FUN = function(c){return(c$relaxed)})))
  {
    exp_options$master <- "relaxed"
  }
  
  # set ANTARES study options
  set_antares_options(exp_options, candidates, opts)
  
  # check that the study is appropriately set for the expansion problem
  assertthat::assert_that(benders_check(candidates, opts))
  
  # initiate text files to communicate with master problem
  # and copy AMPL file into the temporary file 
  if(!recovery_mode){initiate_master(candidates, exp_options, opts)}
  
  # initiate a few parameters
  first_sim_week <- 1 + ceiling((opts$parameters$general$simulation.start - 1)/7)
  n_w <- floor((opts$parameters$general$simulation.end - opts$parameters$general$simulation.start + 1)/7) # number of weeks 
  weeks <- first_sim_week:(first_sim_week + n_w - 1) # identifier of weeks to simulate for all expansion planning optimisation
  mc_years <- getPlaylist(opts) # identifier of mc years to simulate for all expansion planning optimisation
  n_mc <- length(mc_years) # number of mc_years
  has_converged <- FALSE # has the benders decomposition converged ? not yet
  best_solution <- NA  # best solution identifier
  tmp_folder <- paste(opts$studyPath,"/user/expansion/temp",sep="")   # temporary folder
  relax_integrality <- exp_options$master %in% c("relaxed", "integer") # should integer problem be relaxed ?
  unique_key <- paste(sample(c(0:9, letters), size = 3, replace = TRUE),collapse = "") # unique key used in output names
  all_areas <- antaresRead::getAreas(opts = opts) # all area of the first
  horizon <- opts$parameters$general$horizon # representated year in the Antares Study
  first_iteration <- TRUE # is it the first iteration ?
  
  
  # create output structure 
  x <- list()
  x$invested_capacities <- initiate_candidate_capacities(candidates, horizon)
  x$costs <- data.frame(row.names = c("it", "year", "investment_costs", "operation_costs", "overall_costs"))
  x$rentability <- data.frame(row.names = sapply(candidates, FUN = function(c){c$name}))
  x$iterations <- list()
  x$digest <- list()

  # create iteration structure
  current_it <- list()
  current_it$n <- 1  # iteration number
  current_it$id <- paste0("it",current_it$n)  # iteration identifier
  current_it$full <- TRUE  # is it an iteration in which we simulate all weeks and all MC years ?
  current_it$mc_years <- mc_years # identidier of mc years to simulate at this current iteration
  current_it$weeks <- weeks # identidier of weeks to simulate at this current iteration
  current_it$cut_type <- exp_options$cut_type # type of cut for this iteration (average, weekly, yearly)
  current_it$need_full <- FALSE # is a complete iteration needed for next step ?
  current_it$last_full <- 1 # last iteration with full simulation
  
  if(recovery_mode){  #recover mode : we keep what is saved in the temporary file and start after
    recovered_it <- scan(paste0(tmp_folder, "/in_iterations.txt"), what=character(), sep="/", quiet = TRUE)
    current_it$n <- as.numeric(substr(tail(recovered_it, n=1), start = 3, stop = 100)) + 1
    current_it$id <- paste0("it",current_it$n)
  }
  
  # prepare cuts tables
  # cuts <- list()
  # cuts$avg <- data.table(name = character(), candidate = character(), lambda = double())
  # cuts$yearly <- data.table(name = character(), mc_year = integer(), candidate = character(), lambda = double())
  # cuts$weekly <- data.table(name = character(), mc_year = integer(), week = integer(), candidate = character(), lambda = double())
  # cuts$avg_cost <- data.table(name = character(), cost = double())
  # cuts$yearly_cost <- data.table(name = character(), mc_year = integer(), cost = double())
  # cuts$weekly_cost <- data.table(name = character(), mc_year = integer(), week = integer(), cost = double())

  
    
  # ----
  # iterate until convergence or until the max number of iteration has been reached
  while(!has_converged && current_it$n <= exp_options$max_iteration)
  {
    # ---- 0. Initiate iteration ----
    
    # not much to do here
    
    current_it$id <- paste0("it", current_it$n)
    
    # ---- 1. Select weeks to simulate at this iteration ----
    
    # a smart selection of weeks is performed at each iteration in order to
    # accelerate computation time by simulating only the weeks whose cuts are
    # more likely to be activated in the master problem
   
    current_it <- week_selection(current_it, mc_years, weeks, tmp_folder, exp_options)
    
    # set simulation period
    set_simulation_period(current_it$weeks, opts)
    # set playlist
    antaresEditObject::setPlaylist(current_it$mc_years, opts)
    
    
    if(current_it$full & display){
      cat("--- ITERATION ", current_it$n, " (complete iteration, ", n_w * n_mc, " simulated weeks) ---\n", sep="")
    }
    if(!current_it$full & display){
      cat("--- ITERATION ", current_it$n, " (partial iteration, ", length(current_it$mc_years) * length(current_it$weeks), " simulated weeks) ---\n", sep="")
    }
    
    
    # ---- 2. Set installed capacities ---- 
    
    # update study with current invested capacities on links
    
    
    
    for(c in candidates)
    {
      
      new_capacity_direct   <- get_capacity_profile(get_capacity(x$invested_capacities, candidate = c$name, it = current_it$n),
                                                    c$link_profile, exp_options$uc_type)+c$already_installed_capacity*c$already_installed_link_profile

      new_capacity_indirect <- get_capacity_profile(get_capacity(x$invested_capacities, candidate = c$name, it = current_it$n),
                                           c$link_profile_indirect, exp_options$uc_type)+c$already_installed_capacity*c$already_installed_link_profile_indirect

      
            # update study
      update_link(c$link, "direct_capacity", new_capacity_direct , opts)
      update_link(c$link, "indirect_capacity", new_capacity_indirect, opts)
    }
    
    
    
    
    # ---- 3. Simulate ---- 
    
    # run the ANTARES simulation, load the path related to this
    # simulation and read the outputs
    
    simulation_name <- paste0("expansion-benders-", unique_key, "-", current_it$id)
    if(display){  cat("   ANTARES simulation running ... ", sep="")}
    run_simulation(simulation_name, mode = ifelse(exp_options$uc_type == "expansion_accurate", "expansion", "economy"),
                   path_solver, wait = TRUE, show_output_on_console = FALSE, parallel = parallel, opts)
    if(display){  cat("[done] \n", sep="")}
    
    output_antares <- antaresRead::setSimulationPath(paste0(opts$studyPath, "/output/", get_whole_simulation_name(simulation_name, opts)))
    # note : to avoid the sum of numeric approximations, it is advised to use the most aggregated output of ANTARES
    # (e.g. to use annual results of ANTARES instead of the sum of the weekly results)
    

    # ---- 4. Assess system costs and marginal rentability of each investment candidate ---- 
    
    # analyse some outputs of the just finished ANTARES simulation
    
    
    # compute system operationnal and investment costs 
    op_cost <- get_op_costs(output_antares, current_it, exp_options)
    inv_cost <- sum(sapply(candidates, FUN = function(c){c$cost * get_capacity(x$invested_capacities, candidate = c$name, it = current_it$n)}))
    inv_cost <- inv_cost * n_w / 52 # adjusted to the period of the simulation
    ov_cost <-  op_cost + inv_cost
  
    # update output structure
    x$costs <- rbind(x$costs, data.frame(
      it = current_it$n,
      year = horizon,
      investment_costs = inv_cost,
      operation_costs = op_cost,
      overall_costs = ov_cost
    ))
    
    
    if(current_it$full)
    {
      # check if the current iteration provides the best solution
      if(ov_cost <= min(x$costs$overall_costs, na.rm = TRUE)) {best_solution <- current_it$n}
    }
    
    # compute average rentability of each candidate 
    x$rentability[[current_it$id]] <- get_expected_rentability(output_antares, current_it, candidates, n_w, x)
    
    # compute lole for each area
    x$digest[[current_it$id]] <- get_digest(output_antares, current_it)
    
  
    # print results of the ANTARES simulation
    if(display & current_it$full)
    {
      for (c in candidates){cat( "     . ", c$name, " -- ", get_capacity(x$invested_capacities, candidate = c$name, it = current_it$n), " invested MW -- rentability = ", round(x$rentability[c$name, current_it$id]/1000), "ke/MW \n" , sep="")}
      cat("--- op.cost = ", op_cost/1000000, " Me --- inv.cost = ", inv_cost/1000000, " Me --- ov.cost = ", ov_cost/1000000, " Me ---\n")
    }
    
    
    
    
    # ---- 5. Update cuts ---- 
    
    # update cuts of the benders master problem, based on the marginal
    # rentability of each investment candidates and on the obtained system
    # costs
    # cuts can be averaged on all MC years, yearly or weekly
    write_master_files(tmp_folder, output_antares, current_it, candidates, exp_options, x, n_w)
  
    
    
    # ---- 6. Solve master problem ---- 
    
    # solve master optimisation problem (using AMPL) and read results of
    # this problem
    
    # if option "integer" has been chosen, should the integrality be added ?
    if(exp_options$master == "integer" && !first_iteration && relax_integrality)
    {
      if(convergence_relaxed(best_sol = min(x$costs$overall_costs, na.rm = TRUE), best_under_estimator, exp_options))
      {
        relax_integrality <- FALSE
        # reintialize ov.cost and op.costs (which are not admissible because computed with relaxed investments decisions)
        x$costs$operation_costs <- rep(NA, nrow(x$costs))
        x$costs$overall_costs <- rep(NA, nrow(x$costs))
        current_it$need_full <- TRUE
        
        if (display){cat("--- ADDITION of INTEGER variables into investment decisions --- \n")}
      }
    }
    
    # run AMPL with system command
    log <- solve_master(opts, relax_integrality, ampl_path)
    
    # load AMPL output
    #     - underestimator
    x$under_estimator  <-  unname(unlist(read.table(paste0(tmp_folder,"/out_underestimator.txt"), header = FALSE)))
    best_under_estimator <-  max(x$under_estimator)
    
    #    - investment solution
    benders_sol <-  read.table(paste0(tmp_folder,"/out_solutionmaster.txt"), sep =";", col.names = c("candidate", "value"))
    if(display)
    {
      cat("--- lower bound on ov.cost = ", best_under_estimator/1000000 ," Me --- best solution (it", best_solution, ") = ", subset(x$costs, it == best_solution)$overall_costs/1000000   ,"Me \n")
    }
 
    
    # ---- 7. Check convergence ---- 
    
    # check convergence of the benders decomposition
    
    # if difference between the under estimator and the best solution
    # is lower than the optimality gap, then the convergence has been reached
    if(!all(is.na(x$costs$overall_costs)))
    {
      if(convergence(best_sol = min(x$costs$overall_costs, na.rm = TRUE), best_under_estimator, exp_options)) 
      {
        has_converged <- TRUE
      }
      
      # if master problem solution didn't evolve at this (full) iteration, then the decomposition has
      # converged
      
      if(have_capacities_changed(benders_sol, x$invested_capacities, tol = 0.05))
      {
        if(current_it$full)
        { 
          has_converged <- TRUE
          if(display){
            cat("--- installed capacities did not evolve between the two last iterations \n", sep ="")
          }
        }
        else
        {
          current_it$need_full <- TRUE
        }
      }
    }  
    
    # if option integer has been chosen and integer has not yet been used, convergence cannot be reached
    if(exp_options$master == "integer" && relax_integrality)
    {
      has_converged <- FALSE
    }
    
    
    # display end messages
    if(has_converged & display)
    { 
        cat("--- CONVERGENCE within optimality gap: best solution = it", best_solution, " --- ov.cost = ", min(x$costs$overall_costs, na.rm = TRUE)/1000000 ," Me --- Best Lower Bound = ",best_under_estimator/1000000 , " Me \n")
    }
    if(display & current_it$n >= exp_options$max_iteration)
    { 
      cat("--- END, the maximum number of iteration (", exp_options$max_iteration, ") has been reached \n", sep ="")
    }
    
    # go to next iteration
    x$iterations[[current_it$n]] <- current_it
    current_it$n = current_it$n +1
    first_iteration <- FALSE
    
    # ---- 8. Update investment decisions ---- 
    
    # update investment decision to prepare next iteration
    
    if(!has_converged && current_it$n <= exp_options$max_iteration)
    {
      new_capacity <- data.frame(
        it = rep(current_it$n, n_candidates),
        year = rep(horizon,n_candidates),
        candidate = benders_sol$candidate,
        value = benders_sol$value
      )
      x$invested_capacities <- rbind(x$invested_capacities, new_capacity)
    }
    
    
    # ---- 9. Clean ANTARES output ----
    if(clean) { clean_output_benders(best_solution, unique_key, opts)}

  }
  
  
  
  # add information in the output file
  x$expansion_options <- read_options(file = paste(opts$studyPath,"/user/expansion/settings.ini",sep=""), opts)
  x$study_options <- opts
  x$candidates <- read_candidates(file = paste(opts$studyPath,"/user/expansion/candidates.ini",sep=""), opts)
  
  # reset options of the ANTARES study to their initial values
  assertthat::assert_that(file.remove(paste0(opts$studyPath, "/settings/generaldata.ini")))
  assertthat::assert_that(file.rename(from = paste0(opts$studyPath, "/settings/generaldata_tmpsvg.ini"), 
                                    to = paste0(opts$studyPath, "/settings/generaldata.ini")))


  # set link capacities to their optimal value
  for(c in candidates)
  {
    update_link(c$link, "direct_capacity", c$link_profile*get_capacity(x$invested_capacities, candidate = c$name, it =best_solution)+c$already_installed_link_profile*c$already_installed_capacity , opts)
    update_link(c$link, "indirect_capacity", c$link_profile_indirect*get_capacity(x$invested_capacities, candidate = c$name, it =best_solution)+c$already_installed_link_profile_indirect*c$already_installed_capacity , opts)

  }
 
 

  # save output file
  # copy the benders_out into a Rdata in the temporary folder
  tmp_folder <- paste(opts$studyPath,"/user/expansion/temp",sep="")
  if(!dir.exists(tmp_folder))
  {
    dir.create(tmp_folder)
  }
  
  saveRDS(x, file = paste0(tmp_folder, "/data_for_report.RDS"))
  
  # create report
  if(report)
  {
    if(display)
    {
      cat("Write report in user/expansion/report directory \n")
    }
    
    rmarkdown::render(input = system.file("rmd/report.Rmd", package = "antaresXpansion"), 
                      output_file = default_report_file(opts), params = x, quiet = TRUE)
  }
}