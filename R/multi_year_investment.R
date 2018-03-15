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
  studies <- antaresXpansion::read_studies(paste0(directory_path,"/studies.ini"))
  assertthat::assert_that(length(studies) > 1)
  
  # save current settings of the ANTARES studies into a temporary file
  lapply(studies, FUN = function(x){save_general_settings(opts = x$opts)})
  # reset options of the ANTARES studies to their initial values when the function ends
  on.exit(lapply(studies$opts, FUN = function(x){restore_general_settings(opts = x$opts)})) 
  
  # read expansion planning options and investment candidates
  for(s in studies)
  {
    exp_options <- read_options(file = paste0(directory_path,"/settings.ini"), opts = s$opts)
    candidates <- read_candidates(file = paste0(directory_path,"/candidates.ini"), studies = studies, opts = s$opts)
  }
    
  n_candidates <- length(candidates)
  assertthat::assert_that(n_candidates > 0)
  
  # if all investments are distributed (no integer variables), relax master problem
  if(all(sapply(candidates, FUN = function(c){return(c$relaxed)})))
  {
    exp_options$master <- "relaxed"
  }
  
  # set ANTARES study options
  for(s in studies) set_antares_options(benders_options = exp_options, candidates = candidates, opts = s$opts)

  # check that the studies are appropriately set for the expansion problem
  for(s in studies) assertthat::assert_that(benders_check(candidates, opts = s$opts))
  

  # initiate a few parameters
  has_converged <- FALSE # has the benders decomposition converged ? not yet
  best_solution <- NA  # best solution identifier
  relax_integrality <- exp_options$master %in% c("relaxed", "integer") # should integer problem be relaxed ?
  unique_key <- paste(sample(c(0:9, letters), size = 3, replace = TRUE),collapse = "") # unique key used in output names
  first_iteration <- TRUE # is it the first iteration ?

  # initiate text files to communicate with master problem
  # and copy AMPL file into the temporary file 
  tmp_folder <- paste0(directory_path,"/temp")   # temporary folder
  initiate_master_multi_year(candidates, exp_options, studies, tmp_folder)
  
  
  # create output structure 
  x <- list()
  x$invested_capacities <- initiate_candidate_capacities_multi_year(candidates, studies)
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
    # (not activated in multi-year investment for now)  
    # current_it <- week_selection(current_it, mc_years, weeks, tmp_folder, exp_options)
  
    n_weeks <- 0
    for(s in 1:studies$n_simulated_years)
    {
      # set simulation period
      antaresXpansion:::set_simulation_period(current_it$weeks[[s]], studies$opts[[s]])
      # set playlist
      antaresEditObject::setPlaylist(current_it$mc_years[[s]], studies$opts[[s]])
      # n_weeks 
      n_weeks <- n_weeks + length(current_it$mc_years[[s]]) * length(current_it$weeks[[s]])
    }
    
    if(current_it$full & display){
      cat("--- ITERATION ", current_it$n, " (complete iteration, ", n_weeks, " simulated weeks) ---\n", sep="")
    }
    if(!current_it$full & display){
      cat("--- ITERATION ", current_it$n, " (partial iteration, ", n_weeks, " simulated weeks) ---\n", sep="")
    }
    
    
    # ---- 2. Set installed capacities ---- 
    
    # update studies with current invested capacities on links
    
    
    for(c in candidates) { for (s in  1:studies$n_simulated_years)
    {
      new_capacity <- get_capacity_profile(get_capacity(x$invested_capacities, candidate = c$name, it = current_it$n, 
                                                        horizon = studies$simulated_years[s]), c$link_profile, exp_options$uc_type)
      # update study
      update_link(c$link, "direct_capacity", new_capacity , studies$opts[[s]])
      update_link(c$link, "indirect_capacity", new_capacity, studies$opts[[s]])
    }}
    
    
    
    
    # ---- 3. Simulate ---- 
    
    # run the ANTARES simulations, load the path related to this
    # simulation and read the outputs
    
    simulation_name <- paste0("expansion-benders-", unique_key, "-", current_it$id)
    
    for (s in  1:studies$n_simulated_years)
    {
      if(display){  cat("   ANTARES simulation running [ year ", studies$simulated_year[s], " ] ... ", sep="")}
      run_simulation(simulation_name, mode = ifelse(exp_options$uc_type == "expansion_accurate", "expansion", "economy"),
                    path_solver, wait = TRUE, show_output_on_console = FALSE, parallel = parallel, opts = studies$opts[[s]])
      if(display){  cat("[done] \n", sep="")}
      
      output_antares[[s]] <- antaresRead::setSimulationPath(paste0(studies$opts[[s]]$studyPath, "/output/", get_whole_simulation_name(simulation_name, opts = studies$opts[[s]])))
    }

    
    # note : to avoid the sum of numeric approximations, it is advised to use the most aggregated output of ANTARES
    # (e.g. to use annual results of ANTARES instead of the sum of the weekly results)
    
    
    # ---- 4. Assess system costs and marginal rentability of each investment candidate ---- 
    
    # analyse some outputs of the just finished ANTARES simulation
    
    for (s in  1:studies$n_simulated_years){
      
      # compute system operationnal and investment costs 
      op_cost <- get_op_costs(output_antares[[s]], current_it, exp_options)
      inv_cost <- sum(sapply(candidates, FUN = function(c){c$cost[s] * get_capacity(x$invested_capacities, candidate = c$name, it = current_it$n, horizon = studies$simulated_years[s])}))
      inv_cost <- inv_cost * n_w / 52 # adjusted to the period of the simulation
      ov_cost <-  op_cost + inv_cost
      
      # update output structure
      x$costs <- rbind(x$costs, data.frame(
        it = current_it$n,
        year = studies$simulated_years[s],
        investment_costs = inv_cost,
        operation_costs = op_cost,
        overall_costs = ov_cost
      ))
    }
    
    if(current_it$full)
    {
      # check if the current iteration provides the best solution
      if(ov_cost <= min(x$costs$overall_costs, na.rm = TRUE)) {best_solution <- current_it$n}
    }
    
    # compute average rentability of each candidate 
    x$rentability[[current_it$id]] <- get_expected_rentability(output_antares, current_it, candidates, n_w)
    
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
  
  

}