
#' Check that the ANTARES study is ready for benders decomposition
#' 

#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#'   
#' @return boolean, indicating whether or not the test are fullfilled 
#' 
#' @importFrom  antaresRead simOptions readClusterDesc
#' @importFrom dplyr filter
#' @importFrom antaresEditObject readIniFile
#' @noRd
#' 
benders_check <- function(candidates, opts = antaresRead::simOptions())
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
  
  #2. for all link which are candidates, transmission capacities should not be put to infinite
  for(c in candidates)
  {
    #load file containing link properties            
    link_file_name <- paste(opts$inputPath,"/links/" ,from(c$link),"/", "properties.ini",sep="")
    
    # check that this file exists
    assertthat::assert_that(file.exists(link_file_name))
    
    # read it
    link_properties <- antaresEditObject::readIniFile(link_file_name)
    assertthat::assert_that(to(c$link) %in% names(link_properties))
    assertthat::assert_that("transmission-capacities" %in% names(link_properties[[to(c$link)]]))
    
    if(link_properties[[to(c$link)]]$`transmission-capacities` != "enabled")
    {
      stop("transmission capacities of expansion candidates must be 'enabled' (see : ", c$link, ")") 
    }
    
  }
  
  #3. Check that simplex range is week
  return(checked)
  
}