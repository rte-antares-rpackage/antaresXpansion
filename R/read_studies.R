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
    
    studies[[n_simulated_years]]$year <- as.numeric(option_name)

    id_year=line-first_line
    #id_year=1 for the first non empty line
    studies[[n_simulated_years]]$opts <- setSimulationPath(option_value,simulation=0)
    assertthat::assert_that(file.exists(studies[[n_simulated_years]]$opts$studyPath))
    
    # add a few information on the study
    first_sim_week <- 1 + ceiling((studies[[n_simulated_years]]$opts$parameters$general$simulation.start - 1)/7)
    studies[[n_simulated_years]]$n_w <- floor((studies[[n_simulated_years]]$opts$parameters$general$simulation.end - studies[[n_simulated_years]]$opts$parameters$general$simulation.start + 1)/7) # number of weeks 
    studies[[n_simulated_years]]$weeks <- first_sim_week:(first_sim_week + studies[[n_simulated_years]]$n_w - 1) # identifier of weeks to simulate for all expansion planning optimisation
    studies[[n_simulated_years]]$mc_years <- antaresEditObject::getPlaylist(studies[[n_simulated_years]]$opts) # identifier of mc years to simulate for all expansion planning optimisation
    studies[[n_simulated_years]]$n_mc <- length(studies[[n_simulated_years]]$mc_years) # number of mc_years
    studies[[n_simulated_years]]$output_antares <- NA
    #studies[[n_simulated_years]]$all_areas <- antaresRead::getAreas(opts = studies[[n_simulated_years]]$opts) # all area of the first
    
    
  }
  
  return(studies)
}
