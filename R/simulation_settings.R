

#' Enable or disable the custom filtering of the output
#' 
#' \code{enable_custom_filtering} is a function which modifies the input file of an ANTARES
#' study by enabling (or disabling) the custom filtering of the output
#' 
#' @param enable
#'   Should the custom filtering be enabled (\code{TRUE}) or disabled (\code{FALSE}) ?
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return 
#' The function does not return anything. It is  used to modify the input of an 
#' ANTARES study
#' 
#' @import assertthat antaresRead
#' @export
#' 
#' 
enable_custom_filtering <- function(enable = TRUE, opts = simOptions())
{
  # load setting file and check if it exists
  general_parameters_file_name <- paste(simOptions()$studyPath,"/settings/generaldata.ini",sep="")
  assert_that(file.exists(general_parameters_file_name))
  assert_that(file.info(general_parameters_file_name)$size !=0)
  
  # read file
  param_data <- scan(general_parameters_file_name, what=character(), sep="/", quiet = TRUE)
  
  # find line describing the filtering setting
  index = grep("filtering =",param_data,  fixed = TRUE)
  assert_that(length(index) == 1)
  
  # update line
  if (enable) 
  {
    param_data[index] = "filtering = true"
  }
  else
  {
    param_data[index] = "filtering = false"
  }
    
  # write updated file
  write(param_data, general_parameters_file_name, sep = "/")
}




#' Enable or disable the year by year output writing
#' 
#' \code{enable_year_by_year} is a function which modifies the input file of an ANTARES
#' study by activating (or disactivating) the year by year writing of the output
#' 
#' @param enable
#'   Should the year by year outputs be written (\code{TRUE}) or not (\code{FALSE}) ?
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return 
#' The function does not return anything. It is  used to modify the input of an 
#' ANTARES study
#' 
#' @import assertthat antaresRead
#' @export
#' 
#' 
enable_year_by_year <- function(enable = TRUE, opts = simOptions())
{
  # load setting file and check if it exists
  general_parameters_file_name <- paste(simOptions()$studyPath,"/settings/generaldata.ini",sep="")
  assert_that(file.exists(general_parameters_file_name))
  assert_that(file.info(general_parameters_file_name)$size !=0)
  
  # read file
  param_data <- scan(general_parameters_file_name, what=character(), sep="/", quiet = TRUE)
  
  # find line describing the filtering setting
  index = grep("year-by-year =",param_data,  fixed = TRUE)
  assert_that(length(index) == 1)
  
  # update line
  if (enable) 
  {
    param_data[index] = "year-by-year = true"  
  }
  else
  {
    param_data[index] = "year-by-year = false"
  }
  
  # write updated file
  write(param_data, general_parameters_file_name, sep = "/")
}