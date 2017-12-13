#' Get rentability of investments candidates from an antares output
#' 
#' @param output_antares
#'   output from an Antares study, as obtained by \code{antaresRead::setSimulationPath}
#' @param current_it
#'   current iteration of the benders problem with its associated characteristics
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param n_w
#'   number of weeks in the study
#' @return 
#' vector of rentability
#' 
#' @importFrom antaresRead readAntares 
#' @noRd

get_expected_rentability <- function(output_antares, current_it, candidates, n_w)
{
  n_candidates <- length(candidates)
  if(!current_it$full)
  {
    # in case of a partial iteration, expected rentability over all weeks 
    # of all mc years cannot be computed
    return(rep(NA, n_candidates))
  }
  else
  {
    # first, read antares outputs 
    # hourly results (for candidates with link profile only)
    if (length(with_profile(candidates)) > 0 )
    {
      output_link_h_s = readAntares(areas = NULL, links = with_profile(candidates), mcYears = NULL, 
                                    timeStep = "hourly", opts = output_antares, showProgress = FALSE,
                                    select = "MARG. COST")
    }
    # synthetic results for other candidates 
    if (length(without_profile(candidates)) > 0 )
    {
      output_link_s = antaresRead::readAntares(areas = NULL, links = without_profile(candidates), mcYears = NULL, 
                                             timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                             select = "MARG. COST")
    }
    
    # second, get aggregated rentability for those links
    get_aggr_rentability <- function(c)
    {
      if(c$has_link_profile)
      {
        # carreful : not sure the following line works in case of a partial iteration
        return(sum(as.numeric(subset(output_link_h_s, link == c$link)$"MARG. COST")*c$link_profile[1:8736]) - c$cost * n_w / 52) 
      }
      else 
      {
        return(sum(as.numeric(subset(output_link_s, link == c$link)$"MARG. COST")) - c$cost * n_w / 52)
      }
    }
    average_rentability <- sapply(candidates, FUN = get_aggr_rentability)
    return(average_rentability)
  }
    
}