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
#' @param x
#'   data structure of the benders decomposition
#' @return 
#' vector of rentability
#' 
#' @importFrom antaresRead readAntares 
#' @noRd

get_expected_rentability <- function(output_antares, current_it, candidates, n_w, x)
{
  n_candidates <- length(candidates)
  capa_zero <- data.frame(link = 0, capa = 0 )
  for (c in 1:n_candidates)
  {
    capacite <- antaresXpansion:::get_capacity(x$invested_capacities, candidate = candidates[[c]]$name, it = current_it$n)
    temp <- data.frame(link = candidates[[c]]$link, capa = capacite)
    capa_zero <- rbind(capa_zero, temp)
  }
  capa_zero <- capa_zero[-1,]  

  if(!current_it$full)
  {
    # in case of a partial iteration, expected rentability over all weeks 
    # of all mc years cannot be computed
    return(rep(NA, n_candidates))
  }
  else
  {
    # first, read antares outputs 
    output <- extract_output(output_antares, current_it, candidates, capa_zero)
    
    # second, get aggregated rentability for those links

    average_rentability <- sapply(candidates, FUN = get_aggr_rentability, output, current_it, n_w)

    return(average_rentability)
  }
    
}