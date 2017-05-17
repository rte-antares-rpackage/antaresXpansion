
#' Check that the ANTARES study is ready for benders decomposition
#' 
#' 
#' @param benders_options
#'   list of benders decomposition options, as returned by
#'   \code{\link{read_options}}.
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return boolean, indicating whether or not the test are fullfilled 
#' 
#' @import  antaresRead
#' @export
#' 
#' 
benders_check <- function(benders_options, opts)
{
 
  checked  <-  TRUE
  
  #1. for all the clusters of the study, the market bid should be equal to the marginal cost
  cluster <- antaresRead::readClusterDesc(opts = opts)
  
  if (nrow(cluster[marginal.cost != market.bid.cost]) > 0)
  {
    checked <- FALSE
    stop("for all thermal clusters, market bid must be equal to marginal cost (e.g. see cluster ",
         cluster[marginal.cost != market.bid.cost][1, area] , " - ", 
         cluster[marginal.cost != market.bid.cost][1, cluster], ")")
  }
  
  
  return(checked)
  
}