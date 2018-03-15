#' Read studies.ini file 
#' 
#' The studies.ini file contains path towards several Antares studies indexed
#' by the year their represent. For instance \code{2025 = D:/antares1}.
#' 
#' 
#' @param studies_file_name character. file path of a studies.ini file
#'
#' @return 
#' Returns a list containing the different paths of the studies
#' 
#' @importFrom assertthat assert_that
#' @export
#'
read_studies <- function(studies_file_name)
{

  assertthat::assert_that((file.exists(studies_file_name)))
  
  #change the working directory while executing the function to the studies.ini directory
  #enables to work with relative paths
  current_wd <- getwd()
  on.exit(setwd(current_wd))
  setwd(dirname(studies_file_name))
  
  # initiate output structure 
  studies <-list()
  n_simulated_years <- 0
  first_line <- 0
  
  #read file
  param_data <- readLines(studies_file_name,warn=FALSE)
  
  # go through every line of the file from the first non-empty line (identified by id_year) to the end
  for(line in 1:length(param_data))
  {
    if (param_data[line]==""){
      first_line<-first_line+1
      next
    }
    
    option_name <-strsplit(param_data[line], "=")[[1]][1]
    option_value <- strsplit(param_data[line], "=")[[1]][2]
    
    # remove white spaces in the beginning and the end
    option_name <- sub("^\\s+", "", option_name)
    option_name <- sub("\\s+$", "", option_name)
    option_value <- sub("^\\s+", "", option_value)
    option_value <- sub("\\s+$", "", option_value)
    
    #option_value<-gsub(\,/,option_value)
    if(!dir.exists(option_value))
    {
      stop("The following antares study does not exist : ", option_value)
    }
    
    n_simulated_years<-n_simulated_years+1
    studies[[n_simulated_years]] <- list()
    
    studies[[n_simulated_years]]$year <- option_name

    id_year=line-first_line
    #id_year=1 for the first non empty line
    studies[[n_simulated_years]]$opts <- setSimulationPath(option_value,simulation=0)
    assertthat::assert_that(file.exists(studies[[n_simulated_years]]$opts$studyPath))
    
  }
  return(studies)
}
