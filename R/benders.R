
#' Launch benders decomposition
#' 
#' 
#' @param path_solver
#'   Character containing the Antares Solver path
#' @param display
#'   Logical. If \code{TRUE} the advancement of the benders decomposition
#'   if displayed in the console
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return 
#' 
#' @import assertthat antaresRead
#' @export
#' 
benders <- function(path_solver, display = TRUE, opts = simOptions())
{
  # ---- 0. initiale benders iteration ----
  # read expansion planning options
  exp_options <- read_options(opts)
  
  # read investment candidates file
  candidates <- read_candidates(opts)
  n_candidates <- length(candidates)
  
  # set ANTARES study options
  #    option - output filtering
  enable_custom_filtering(TRUE, opts)
  enable_year_by_year(TRUE, opts)
  filter_output_areas(areas = getAreas(opts = opts), filter = c("weekly", "annual"), type = c("year-by-year", "synthesis"), opts = opts)
  filter_output_links(links = getLinks(opts = opts), filter = c("weekly", "annual"), type = c("year-by-year", "synthesis"), opts = opts)
  
  #    option - unit-commitment mode
  if(exp_options$uc_type == "accurate")
  {
    set_uc_mode(mode = "accurate", opts = opts)
    enable_uc_heuristic(enable = TRUE, opts = opts)
  }
  if(exp_options$uc_type == "fast")
  {
    set_uc_mode(mode = "fast", opts = opts)
    enable_uc_heuristic(enable = TRUE, opts = opts)
  }
  if(exp_options$uc_type == "relaxed_fast")
  {
    set_uc_mode(mode = "fast", opts = opts)
    enable_uc_heuristic(enable = FALSE, opts = opts)
  }
  
  #    option - week
  #    we need to ensure the consistency between the weekly optimisation and the weekly
  #    aggregation of the output
  
  month_name <- c("january", "december", "november", "october", "september", "august", "july", "june", "may", "april", "march", "february")
  day_per_month <- c(0, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 28)
  
  day_name <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  if (opts$parameters$general$leapyear)
  {
    day_per_month[12] <- 29
  }
  month_id <- which(month_name == opts$parameters$general$"first-month-in-year")
  assert_that(length(month_id) == 1)
  n_day <- (-sum(day_per_month[1:month_id]) + opts$parameters$general$simulation.start - 1) %% 7
  
  first_day_week <- day_name[((which(day_name == opts$parameters$general$january.1st) + n_day - 1) %% 7 ) +1]
  set_week(first_day = first_day_week, opts)
  
  # initiate text files to communicate with master problem
  # and copy AMPL file into the temporary file 
  initiate_master(candidates, exp_options, opts)
  
  # initiate a few parameters
  n_w <- floor((opts$parameters$general$simulation.end - opts$parameters$general$simulation.start + 1)/7)
  mc_years <- get_playlist(opts)
  n_mc <- length(mc_years)
  has_converged <- FALSE
  current_iteration <- 1
  best_solution <- NA
  
  # create output structure 
  x <- list()
  x$invested_capacities <- data.frame()
  x$overall_costs <- numeric()
  x$investment_costs <- numeric()
  x$operation_costs <- numeric()
  x$rentability <- data.frame()
  
  # set initial value to each investment candidate (here put to max_invest/2)
  x$invested_capacities <- data.frame( it1 = sapply(candidates, FUN = function(c){c$max_invest/2}))
  row.names(x$invested_capacities) <- sapply(candidates, FUN = function(c){c$name})
  
  
  
  
  # ----
  # iterate until convergence or until the max number of iteration has been reached
  while(!has_converged && current_iteration <= exp_options$max_iteration)
  {
    it_id <- paste0("it", current_iteration)
    if(display){  cat("--- ITERATION ", current_iteration, " ---\n", sep="")}
    
    
    # ---- 1. Set installed capacities ---- 
    
    for(c in candidates)
    {
      update_link(c$link, "direct_capacity", x$invested_capacities[c$name, it_id] , opts)
      update_link(c$link, "indirect_capacity", x$invested_capacities[c$name, it_id], opts)
    }
    
    
    
    
    
    # ---- 2. Simulate ---- 
    
    simulation_name <- paste0("benders-", it_id)
    if(display){  cat("   ANTARES simulation running ... ", sep="")}
    run_simulation(simulation_name, mode = "economy", path_solver, wait = TRUE, show_output_on_console = FALSE, opts)
    if(display){  cat("[done] \n", sep="")}
    
    output_antares <- setSimulationPath(paste0(opts$studyPath, "/output/", get_whole_simulation_name(simulation_name, opts)))
    

    

    # ---- 3. Assess costs and marginal rentability of each investment candidates ---- 
    
    # read output of the simulation
    output_area = readAntares(areas = "all", links = NULL, synthesis = FALSE, 
                              timeStep = "weekly", opts = output_antares, showProgress = FALSE)
    output_link = readAntares(areas = NULL, links = "all", synthesis = FALSE, 
                              timeStep = "weekly", opts = output_antares, showProgress = FALSE)
    
    # compute costs
    op_cost <-  sum(as.numeric(output_area[,"OV. COST"]))/n_mc + sum(as.numeric(output_link[,"HURDLE COST"]))/n_mc
    inv_cost <- sum(sapply(candidates, FUN = function(c){c$cost * x$invested_capacities[c$name, it_id]}))
    inv_cost <- inv_cost * n_w / 52 # adjusted to the period of the simulation
    ov_cost <-  op_cost + inv_cost
    
    # update output structure
    x$investment_costs<- c(x$investment_costs, inv_cost)
    x$operation_costs <- c(x$operation_costs, op_cost)
    x$overall_costs <- c(x$overall_costs, ov_cost)
    
    # check if the current iteration provides the best solution
    if(ov_cost <= min(x$overall_costs)) {best_solution = current_iteration}

    # read rentability 

    average_rentability <- sapply(candidates, 
                          FUN = function(c){sum(as.numeric(subset(output_link, link == c$link)$"MARG. COST"))/n_mc - c$cost * n_w / 52 }) 

    if(current_iteration == 1)
    {
      x$rentability <- data.frame(it1 = average_rentability)
      row.names(x$rentability) <- sapply(candidates, FUN = function(c){c$name})
    }
    else {x$rentability[[it_id]] <- average_rentability}
    
    # print results of the ANTARES simulation
    if(display)
    {
      for (c in candidates){cat( "     . ", c$name, " -- ", x$invested_capacities[c$name, it_id], " invested MW -- rentability = ", round(x$rentability[c$name, it_id]/1000), "ke/MW \n" , sep="")}
      cat("--- op.cost = ", op_cost/1000000, " Me --- inv.cost = ", inv_cost/1000000, " Me --- ov.cost = ", ov_cost/1000000, " Me ---\n")
    }
    
    
    
    
    # ---- 4. Update cuts ---- 
    
    cut_type <- exp_options$cut_type
    tmp_folder <- paste(opts$studyPath,"/user/expansion/temp",sep="")

    # write in_cut.txt file 
    script  <-  paste0(it_id, " ", ov_cost, " ", inv_cost, " ", op_cost, " ", cut_type)
    write(script, file = paste0(tmp_folder, "/in_cut.txt"), append = TRUE )      
    
    # write current invested capacity in in_z0.txt
    script <-  ""
    for (c in 1:n_candidates)
    {
      script <- paste0(script, it_id, " ", candidates[[c]]$name, " ", x$invested_capacities[candidates[[c]]$name, it_id])
      if (c != n_candidates) {script <- paste0(script, "\n")}
    }
    write(script, file = paste0(tmp_folder, "/in_z0.txt"), append = TRUE )      
    
    # write cost and rentability files (depending on cut type)
    if(cut_type == "average")
    {
      script  <-  ""
      for (c in 1:n_candidates)
      {
        script <- paste0(script, it_id, " ", candidates[[c]]$name, " ",
                         x$rentability[candidates[[c]]$name, it_id])
        if (c != n_candidates) { script <- paste0(script, "\n")}
      }
      write(script, file = paste0(tmp_folder, "/in_avgrentability.txt"), append = TRUE )
    }
    
    if(cut_type == "yearly")
    {
      script_rentability  <-  ""
      script_cost <- ""
      for(y in mc_years)
      {
        script_cost <- paste0(script_cost, it_id, " ", y , " ",
                              sum(as.numeric(subset(output_area, mcYear == y)$"OV. COST")) +
                              sum(as.numeric(subset(output_link, mcYear == y)$"HURDLE COST")) +
                              inv_cost)
        if (y != mc_years[n_mc]) {script_cost <- paste0(script_cost, "\n")}
        
        for(c in 1:n_candidates)
        {
          script_rentability <- paste0(script_rentability, it_id, " ", candidates[[c]]$name, " ", y , " ",
                                       sum(as.numeric(subset(output_link, link == candidates[[c]]$link & mcYear == y)$"MARG. COST")) -
                                         candidates[[c]]$cost * n_w / 52)
          if (c != n_candidates || y != mc_years[n_mc])
          {
            script_rentability <- paste0(script_rentability, "\n")
          }
        }
      }
      write(script_rentability, file = paste0(tmp_folder, "/in_yearlyrentability.txt"), append = TRUE )
      write(script_cost, file = paste0(tmp_folder, "/in_yearlycosts.txt"), append = TRUE )
    }
    
    if(cut_type == "weekly")
    {
      script_rentability  <-  ""
      script_cost <- ""
      weeks <- unique(output_link$timeId)

      for(y in mc_years)
      {
        for(w in weeks)

        {
          script_cost <- paste0(script_cost, it_id, " ", y , " ", w, " ", 
                                sum(as.numeric(subset(output_area, mcYear == y & timeId == w)$"OV. COST")) +
                                sum(as.numeric(subset(output_link, mcYear == y & timeId == w)$"HURDLE COST")) +
                                inv_cost/52)
          if (y != mc_years[n_mc] || w != last(weeks)) {script_cost <- paste0(script_cost, "\n")}


          for(c in 1:n_candidates)
          {
            script_rentability <- paste0(script_rentability, it_id, " ", candidates[[c]]$name, " ", y , " ", w, " ", 
                                         sum(as.numeric(subset(output_link, link == candidates[[c]]$link & mcYear == y & timeId == w)$"MARG. COST")) -
                                         candidates[[c]]$cost /52)

            if (c != n_candidates || y != mc_years[n_mc] || w != last(weeks))
            {
              script_rentability <- paste0(script_rentability, "\n")
            }
          }
        }
      }
      write(script_rentability, file = paste0(tmp_folder, "/in_weeklyrentability.txt"), append = TRUE )
      write(script_cost, file = paste0(tmp_folder, "/in_weeklycosts.txt"), append = TRUE )
    }
    
    # ---- 5. Solve master problem ---- 
    
    # run AMPL with system command
    log <- solve_master(opts)
    
    # load AMPL output
    #     - underestimator
    under_estimator = read.table(paste0(tmp_folder,"/out_underestimator.txt"), header = FALSE)
    best_under_estimator = max(under_estimator)
    
    #    - investment solution
    benders_sol = read.table(paste0(tmp_folder,"/out_solutionmaster.txt"), sep =";")[,2]
   

    # ---- 6. Update investment decisions ---- 
    x$invested_capacities[[paste0("it", current_iteration+1)]] <- benders_sol
    
    
    # ---- 7. Check convergence ---- 
    
    if( (min(x$overall_costs) - best_under_estimator) <= exp_options$optimality_gap)
    {
      has_converged = TRUE
      if(display)
      {
        cat("--- CONVERGENCE : best solution = it ", best_solution, " --- ov.cost = ", min(x$overall_costs)/1000000 ," Me --- Best Lower Bound = ",best_under_estimator/1000000 , " Me \n")
      }
    }
    if(display)
    {
      cat("--- lower bound on ov.cost = ", best_under_estimator/1000000 ," Me --- best solution (it ", best_solution, ") = ", x$overall_costs[best_solution]/1000000   ,"Me \n")
    }
    current_iteration = current_iteration +1
  }
  
}