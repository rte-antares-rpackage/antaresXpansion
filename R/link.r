

#' Return the first area that a link connects
#' 
#' @param link_name 
#'   Name of one link

#' @return 
#' name of the first area that the link connects
#' 
#' 
from <- function(link_name)
{
  strsplit(link_name, " - ")[[1]][1]
}



#' Return the second area that a link connects
#' 
#' @param link_name 
#'   Name of one link

#' @return 
#' name of the second area that the link connects
#' 
#' 
to <- function(link_name)
{
  strsplit(link_name, " - ")[[1]][2]
}


#' Enable or disable the hurdle cost of a link or a list of links
#' 
#' \code{enable_hurdle_costs} which modifies the input file of an ANTARES
#' study and enable (or disable) the hurdle costs of a link
#' 
#' @param link_names
#'   Name of one link, or vector with several link names
#' @param enable
#'   Should the hurdle costs be enabled (\code{TRUE}) or disabled (\code{FALSE}) ?
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

enable_hurdle_costs <- function(link_names, enable = TRUE, opts = simOptions())
{
  
  for(link in link_names)
  {
    # load file properties.ini of the links starting from area from(link)
    link_file_name <- paste(opts$inputPath,"/links/" ,from(link),"/properties.ini",sep="")
    
    # check that this file exists
    assert_that(file.exists(link_file_name))
    assert_that(file.info(link_file_name)$size != 0)

    # load the file
    param_data <-scan(link_file_name, what=character(), sep="/", quiet = TRUE)
       
    # get the line which correspond to the link we look at
    index = grep(paste("[",to(link),"]",sep=""),param_data,  fixed = TRUE)
    assert_that(length(index)==1)

    # Iterate on the following lines until we find the one we should be modified    
    change_property <- FALSE  
    
    while (!change_property && index<length(param_data))
    {
      index = index + 1
      hu = grep("hurdles-cost",param_data[index])
      # line associated with hurdle cost property
      if (length(hu)!= 0)
      {
        if (enable)
        {
          param_data[index] = "hurdles-cost = true"
        }
        else
        {
          param_data[index] = "hurdles-cost = false"
        }  
        change_property = TRUE
      }
           
      ol = grep("[",param_data[index], fixed = TRUE)
      #line associated with other link description
      if (length(ol)!= 0 || index == length(param_data))
      {
        n = length(param_data)
        if (enable)
        {
          param_data = c(param_data[1:index-1], "hurdles-cost = true", param_data[index:n])
        }
        else
        {
          param_data = c(param_data[1:index-1], "hurdles-cost = false", param_data[index:n])
        }
        change_property = TRUE
      }
    }
    # We write the modified property file
    write(param_data, link_file_name, sep = "/")
  }
}