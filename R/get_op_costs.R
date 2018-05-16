#' Get operationnal cost from an Antares Study
#' 
#' 
#' @param output_antares
#'   output from an Antares study, as obtained by \code{antaresRead::setSimulationPath}
#' @param current_it
#'   current iteration of the benders problem with its associated characteristics
#' @param exp_options
#'   list of benders decomposition options, as returned by
#'   \code{\link{read_options}}.
#' @return 
#' Returns the operationnal cost of the given study 
#' 
#' @importFrom antaresRead readAntares
#' @importFrom assertthat assert_that 
#' @noRd
get_op_costs <- function(output_antares, current_it, exp_options)
{
  if(!current_it$full)
  {
    # in case of a partial iteration, operation costs cannot be computed
    return(NA)
  }
  else
  {
    # in case of a full iteration, operation costs can be computed
    
    if (exp_options$uc_type == "expansion_fast")
    {
      # in that case, non-linear cost has to be removed because they are computed in a post-processing and are not
      # part of the ANTARES optimization
      if(all(is.na(exp_options$y_weights)))
      {
        # if Monte-Carlo years are equally weighted, synthetic data are loaded
        output_area_s <- antaresRead::readAntares(areas = "all", links = NULL, mcYears = NULL, 
                                                timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                                select = c("OV. COST", "NP COST"))
      
        output_link_s <- antaresRead::readAntares(areas = NULL, links = "all", mcYears = NULL, 
                                                timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                                select = c("HURDLE COST"))
      
      
        op_cost <-  sum(as.numeric(output_area_s$"OV. COST"))  + sum(as.numeric(output_link_s$"HURDLE COST")) -
        sum(as.numeric(output_area_s$"NP COST"))
      }
      else
      {
        # if Monte-Carlo years are not equally weighted
        output_area_y <- antaresRead::readAntares(areas = "all", links = NULL, mcYears = "all", 
                                                  timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                                  select = c("OV. COST", "NP COST"))
        
        output_link_y <- antaresRead::readAntares(areas = NULL, links = "all", mcYears = "all", 
                                                  timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                                  select = c("HURDLE COST"))
        
        
        op_cost_y <- sapply(output_antares$mcYears,
                          FUN = function(n){ sum(as.numeric(output_area_y[mcYear ==n]$"OV. COST"))+ 
                                             sum(as.numeric(output_link_y[mcYear ==n]$"HURDLE COST")) -
                                             sum(as.numeric(output_area_y[mcYear ==n]$"NP COST"))})
        
        # works only in case of full iteration
        op_cost <- sum(op_cost_y * exp_options$y_weights)
      }
    }
    else if (exp_options$uc_type == "expansion_accurate")
    {
      # in that case, the costs must be read in the criterion (.txt) files
      # they correspond to the cost returned by the optimization problems while the ov.cost in output of ANTARES
      # is post-treated with some small corrections for more consistency between the weeks
      if(all(is.na(exp_options$y_weights)))
      {
        # if Monte-Carlo years are equally weighted
        op_cost <- sum(antaresRead::readOptimCriteria(opts = output_antares)$"criterion1") / length(current_it$mc_years)
      }
      else
      { 
        # if Monte-Carlo years are not equally weighted
        crit <- antaresRead::readOptimCriteria(opts = output_antares)
        op_cost_y <- sapply(output_antares$mcYears, FUN = function(n){ sum(crit[mcYear ==n ]$"criterion1")})
        
        # works only in case of full iteration
        op_cost <- sum(op_cost_y * exp_options$y_weights)
      }
    }
    else
    {
      # that case should no longer be used (old fast and accurate mode, which are not relaxed
      # and therefore not fitted for a benders decomposition)
      assertthat::assert_that(TRUE)
      
      # output_area_s <- antaresRead::readAntares(areas = "all", links = NULL, mcYears = NULL, 
      #                                           timeStep = "annual", opts = output_antares, showProgress = FALSE,
      #                                           select = "OV. COST")
      # 
      # output_link_s <- antaresRead::readAntares(areas = NULL, links = "all", mcYears = NULL, 
      #                                           timeStep = "annual", opts = output_antares, showProgress = FALSE,
      #                                           select = c("HURDLE COST"))
      # 
      # 
      # op_cost <-  sum(as.numeric(output_area_s$"OV. COST"))  + sum(as.numeric(output_link_s$"HURDLE COST")) 
    }
  }
  return(op_cost)
}