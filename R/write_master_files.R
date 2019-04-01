#' Write files which are necessary for master problem (in_iterations,
#' in_z0, in_costs and in _cuts)
#' 
#' @param folder
#'   folder where to write master files
#' @param output_antares
#'   output from an Antares study, as obtained by \code{antaresRead::setSimulationPath}
#' @param current_it
#'   current iteration of the benders problem with its associated characteristics
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param exp_options
#'   list of benders decomposition options, as returned by
#'   \code{\link{read_options}}.
#' @param x
#'   benders data structure
#' @param n_w
#'   number of weeks in the study
#' @return 
#' nothing
#' 
#' @importFrom antaresRead readAntares  readOptimCriteria
#' @importFrom assertthat assert_that
#' @noRd

write_master_files <- function(folder, output_antares, current_it, candidates, exp_options, x, n_w)
{
  # 0. check : 
  assert_that(dir.exists(folder))
  
  # 1. update iteration file
  write(current_it$id, file = paste0(folder, "/in_iterations.txt"), append = TRUE)  
  
  # 2. write current invested capacity in in_z0.txt
  n_candidates <- length(candidates)
  script <-  ""
  #capa_zero <- data.frame(link = c(), capa = c() )
  for (c in 1:n_candidates)
  {
   capacite <- antaresXpansion:::get_capacity(x$invested_capacities, candidate = candidates[[c]]$name, it = current_it$n)
    script <- paste0(script, current_it$id, " ", candidates[[c]]$name, " ", capacite)
    if (c != n_candidates) {script <- paste0(script, "\n")}
  #  temp <- data.frame(link = candidates[[c]]$link, capa = capacite)
  #  capa_zero <- rbind(capa_zero, temp)
  }
  #capa_zero <- unique(capa_zero)
  write(script, file = paste0(folder, "/in_z0.txt"), append = TRUE )  
  
  # 3 and 4. write costs and cuts files 
  #      average cuts and cost file
  if(current_it$cut_type == "average")
  {
    # make sense only if the iteration if "full"
    assert_that(current_it$full)
    
    # write in_avgcuts.txt file 
    script  <-  paste0(current_it$id, " ", subset(x$costs, it == current_it$n)$overall_costs)
    write(script, file = paste0(folder, "/in_avgcuts.txt"), append = TRUE )      
    
    # read Antares data
    output <- extract_output(output_antares, current_it, candidates)
           
    # write in_avgrentability.txt file
    script  <-  ""
    for (c in 1:n_candidates)
    {
 
      tmp_rentability <- get_aggr_rentability(candidates[[c]],output, current_it, n_w)

      script <- paste0(script, current_it$id, " ", candidates[[c]]$name, " ", tmp_rentability)
      if (c != n_candidates) { script <- paste0(script, "\n")}
    }
    write(script, file = paste0(folder, "/in_avgrentability.txt"), append = TRUE )
  }
  
  #      yearly cuts and cost file
  if(current_it$cut_type == "yearly")
  {
    # make sense only if all the weeks are simulated
    #assert_that(length(current_it$weeks) == length(weeks))
    #assert_that(all(current_it$weeks == weeks))
    
    # compute a few intermediate variables
    n_candidates <- length(candidates)
    last_y <- current_it$mc_years[length(current_it$mc_years)]
    
    # initiate scripts
    script_rentability  <-  ""
    script_cost <- ""
    
    # read antares data
    # first, read antares outputs 
    output <- extract_output(output_antares, current_it, candidates, cut_type = "yearly")
  
    output_link_y = antaresRead::readAntares(areas = NULL, links = "all", mcYears = current_it$mc_years, 
                                               timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                               select = c("MARG. COST", "HURDLE COST"))
    
    output_area_y = antaresRead::readAntares(areas = "all", links = NULL, mcYears = current_it$mc_years, 
                                             timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                             select = c("OV. COST", "NP COST"))
    
    # load cost in expansion_accurate mode
    if(exp_options$uc_type == "expansion_accurate")
    {
      criterion <- antaresRead::readOptimCriteria(opts = output_antares)
    }
    
    # for every mc year
    for(y in current_it$mc_years)
    {
      if(exp_options$uc_type == "expansion_fast")
      {
        # in that case, non-linear cost has to be removed because they are computed in a post-processing and are not
        # part of the ANTARES optimization  
        y_cost <- sum(as.numeric(subset(output_area_y, mcYear == y)$"OV. COST")) +
          sum(as.numeric(subset(output_link_y, mcYear == y)$"HURDLE COST")) -
          sum(as.numeric(subset(output_area_y, mcYear == y)$"NP COST")) +
          subset(x$costs, it == current_it$n)$investment_costs
      }
      else if(exp_options$uc_type == "expansion_accurate")
      {
        # in that case, the considered cost is the criterion of the optimization problem (not yet post-treated)
        y_cost <- sum(as.numeric(subset(criterion, mcYear == y)$"criterion1")) + subset(x$costs, it == current_it$n)$investment_costs
      }
      else
      {
        # old "fast" and "accurate" mode which shouldn't be used anymore
        y_cost <- sum(as.numeric(subset(output_area_y, mcYear == y)$"OV. COST")) +
          sum(as.numeric(subset(output_link_y, mcYear == y)$"HURDLE COST")) +
          subset(x$costs, it == current_it$n)$investment_costs
      }
      
      
      script_cost <- paste0(script_cost, current_it$id, " ", y , " ",y_cost)
      if (y != last_y) {script_cost <- paste0(script_cost, "\n")}
      
      # for every candidate
      for(c in 1:n_candidates)
      {
        tmp_rentability <- get_aggr_rentability_yearly(candidates[[c]],output,output_link_y, current_it, n_w,y)
        script_rentability <- paste0(script_rentability, current_it$id, " ", y, " ", candidates[[c]]$name, " ", tmp_rentability)
        if (c != n_candidates || y != last_y)
        {
          script_rentability <- paste0(script_rentability, "\n")
        }
      }
    }
    write(script_rentability, file = paste0(folder, "/in_yearlyrentability.txt"), append = TRUE )
    write(script_cost, file = paste0(folder, "/in_yearlycuts.txt"), append = TRUE )
    
  }
  
  
  #      weekly cuts and cost file
  
  if(current_it$cut_type == "weekly")
  {
    
    # compute a few intermediate variables
    last_w <- current_it$weeks[length(current_it$weeks)]
    last_y <- current_it$mc_years[length(current_it$mc_years)]
    
    # initiate scripts
    script_rentability  <-  ""
    script_cost <- ""
    
    output <- extract_output(output_antares, current_it, candidates, cut_type = "weekly")
    
    output_link_w = antaresRead::readAntares(areas = NULL, links = "all", mcYears = current_it$mc_years, 
                                             timeStep = "weekly", opts = output_antares, showProgress = FALSE,
                                             select = c("MARG. COST", "HURDLE COST"))
    
    output_area_w = antaresRead::readAntares(areas = "all", links = NULL, mcYears = current_it$mc_years, 
                                             timeStep = "weekly", opts = output_antares, showProgress = FALSE,
                                             select = c("OV. COST", "NP COST"))
    
    
    
    # load cost in expansion mode
    if(exp_options$uc_type == "expansion_accurate")
    {
      criterion <- antaresRead::readOptimCriteria(opts = output_antares)
    }
    
    # for every mc years and every week
    for(y in current_it$mc_years)
    {
      for(w in current_it$weeks)
        
      {
        
        if(exp_options$uc_type == "expansion_fast")
        {
          # in that case, non-linear cost has to be removed because they are computed in a post-processing and are not
          # part of the ANTARES optimization  
          w_cost <- sum(as.numeric(subset(output_area_w, mcYear == y & timeId == w)$"OV. COST")) +
            sum(as.numeric(subset(output_link_w, mcYear == y & timeId == w)$"HURDLE COST")) -
            sum(as.numeric(subset(output_area_w, mcYear == y & timeId == w)$"NP COST")) +
            subset(x$costs, it == current_it$n)$investment_costs /n_w
        }
        else if(exp_options$uc_type == "expansion_accurate")
        {
          # in that case, the considered cost is the criterion of the optimization problem (not yet post-treated)
          w_cost <- sum(as.numeric(subset(criterion, mcYear == y & timeId == w)$"criterion1")) + 
            subset(x$costs, it == current_it$n)$investment_costs /n_w
        }
        else
        {
          # old "fast" and "accurate" mode which shouldn't be used anymore
          w_cost <- sum(as.numeric(subset(output_area_w, mcYear == y & timeId == w)$"OV. COST")) +
            sum(as.numeric(subset(output_link_w, mcYear == y & timeId == w)$"HURDLE COST")) +
            subset(x$costs, it == current_it$n)$investment_costs /n_w
        }
        
        script_cost <- paste0(script_cost, current_it$id, " ", y , " ", w, " ", w_cost)
        if (y != last_y || w != last_w) {script_cost <- paste0(script_cost, "\n")}
        
        # for every candidate
        for(c in 1:n_candidates)
        {
          tmp_rentability <- get_aggr_rentability_weekly(candidates[[c]],output,output_link_w, current_it, n_w,y,w)          
          script_rentability <- paste0(script_rentability, current_it$id, " ", y , " ", w , " ", candidates[[c]]$name, " ", tmp_rentability)
          if (c != n_candidates || y != last_y || w != last_w)
          {
            script_rentability <- paste0(script_rentability, "\n")
          }
        }
      }
    }
    write(script_rentability, file = paste0(folder, "/in_weeklyrentability.txt"), append = TRUE )
    write(script_cost, file = paste0(folder, "/in_weeklycuts.txt"), append = TRUE )
  }
  
  
  
  #5. write ubcost file
  
  ub <- min(x$costs$overall_costs, na.rm = TRUE)
  if (length(x$under_estimator) > 0) best_under_estimator <-  max(x$under_estimator) 
  else best_under_estimator <- -Inf
  if(is.infinite(ub)) ub <- c()
  else if (best_under_estimator > ub) ub <- best_under_estimator
  write(ub, file = paste0(folder, "/in_ubcosts.txt"), append = FALSE )
}