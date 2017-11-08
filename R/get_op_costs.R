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
    
    if (exp_options$uc_type %in% c("relaxed_fast", "expansion_fast"))
    {
      # in that case, non-linear cost has to be removed because they are computed in a post-processing and are not
      # part of the ANTARES optimization
      output_area_s <- antaresRead::readAntares(areas = "all", links = NULL, mcYears = NULL, 
                                                timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                                select = c("OV. COST", "NP COST"))
      
      output_link_s <- antaresRead::readAntares(areas = NULL, links = "all", mcYears = NULL, 
                                                timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                                select = c("HURDLE COST"))
      
      
      op_cost <-  sum(as.numeric(output_area_s$"OV. COST"))  + sum(as.numeric(output_link_s$"HURDLE COST")) -
        sum(as.numeric(output_area_s$"NP COST"))
    }
    else if (exp_options$uc_type %in% c("relaxed_accurate", "expansion_accurate"))
    {
      # in that case, the costs must be read in the criterion (.txt) files
      # they correspond to the cost returned by the optimization problems while the ov.cost in output of ANTARES
      # is post-treated with some small corrections for more consistency between the weeks
      op_cost <- sum(antaresRead::readOptimCriteria(opts = output_antares)$"criterion1") / length(current_it$mc_years)
    }
    else
    {
      # that case should no longer be used (old fast and accurate mode, which are not relaxed
      # and therefore not fitted for a benders decomposition)
      output_area_s <- antaresRead::readAntares(areas = "all", links = NULL, mcYears = NULL, 
                                                timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                                select = "OV. COST")
      
      output_link_s <- antaresRead::readAntares(areas = NULL, links = "all", mcYears = NULL, 
                                                timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                                select = c("HURDLE COST"))
      
      
      op_cost <-  sum(as.numeric(output_area_s$"OV. COST"))  + sum(as.numeric(output_link_s$"HURDLE COST")) 
    }
  }
  return(op_cost)
}