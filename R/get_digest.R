#' Get digest of a given antares output
#' 
#' @param output_antares
#'   output from an Antares study, as obtained by \code{antaresRead::setSimulationPath}
#' @param current_it
#'   current iteration of the benders problem with its associated characteristics
#' @return 
#' digest of the current iteration
#' 
#' @importFrom antaresRead readAntares 
get_digest <- function(output_antares, current_it)
{
  # read antares output
  output_area_s <- antaresRead::readAntares(areas = "all", links = NULL, mcYears = NULL, 
                                            timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                            select = c("LOLD")) 
  if(current_it$full)
  {
    # digest is defined equal to antares output
    digest <- output_area_s[, c("area", "LOLD")]
  }
  else
  {
    # digest is not defined
    n_areas  <-  nrow(output_area_s)
    digest <- data.table(area = output_area_s$area, LOLD = rep(NA, n_areas))
  }
  return(digest)
}