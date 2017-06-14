
#' Check that the ANTARES study is ready for benders decomposition
#' 

#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return boolean, indicating whether or not the test are fullfilled 
#' 
#' @importFrom  antaresRead simOptions readClusterDesc
#' @importFrom dplyr filter
#' 
#' 
benders_check <- function(opts = antaresRead::simOptions())
{
 
  checked  <-  TRUE
  
  #1. for all the clusters of the study, the market bid should be equal to the marginal cost
  cluster <- antaresRead::readClusterDesc(opts = opts)
  
  if (nrow(cluster) > 0)
  {
    if (nrow(dplyr::filter(cluster, marginal.cost != market.bid.cost)) > 0)
    {
      checked <- FALSE
      stop("for all thermal clusters, market bid must be equal to marginal cost (e.g. see cluster ",
           dplyr::filter(cluster, marginal.cost != market.bid.cost)[1, "area"] , " - ", 
           dplyr::filter(cluster, marginal.cost != market.bid.cost)[1, "cluster"], ")")
    }
  }
  
  return(checked)
  
}