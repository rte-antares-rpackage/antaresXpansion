

#' Return the first area that a link connects
#' 
#' @param link_name 
#'   Name of one link
#' @return 
#' name of the first area that the link connects
#' @noRd
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
#' @noRd
#' 
to <- function(link_name)
{
  strsplit(link_name, " - ")[[1]][2]
}


#' Enable or disable the hurdle cost of a link or a list of links
#' 
#' \code{enable_hurdle_costs} is a function which modifies the input file of an ANTARES
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
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions
#' @noRd
#' 
#' 

enable_hurdle_costs <- function(link_names, enable = TRUE, opts = antaresRead::simOptions())
{
  
  for(link in link_names)
  {
    # load file properties.ini of the links starting from area from(link)
    link_file_name <- paste(opts$inputPath,"/links/" ,from(link),"/properties.ini",sep="")
    
    # check that this file exists
    assertthat::assert_that(file.exists(link_file_name))
    assertthat::assert_that(file.info(link_file_name)$size != 0)

    # load the file
    param_data <-scan(link_file_name, what=character(), sep="/", quiet = TRUE)
       
    # get the line which correspond to the link we look at
    index = grep(paste("[",to(link),"]",sep=""),param_data,  fixed = TRUE)
    assertthat::assert_that(length(index)==1)

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


#' Update the properties of a link (capacity, impedance or hurdle costs)
#' 
#' \code{update_link} is a function which modifies the input file of an ANTARES
#' study and update the properties of a link
#' 
#' @param link_name
#'   Name of one link
#' @param property_name
#'   Name of the propery to change, must be equal to "direct_capacity" or "indirect_capacity" or
#'   "impedance" or "direct_hurdle_cost" or "indirect_hurdle_cost"
#' @param new_value
#'   New value of the property 
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return 
#' The function does not return anything. It is  used to modify the input of an 
#' ANTARES study
#' 
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions
#' @importFrom utils write.table
#' @noRd
#' 
#' 
update_link <- function(link_name, property_name, new_value, opts = antaresRead::simOptions())
{
  new_value <- as.matrix(new_value)
  
  # check which column have to be updated
  if(opts$antaresVersion < 700)
  {
    n_col_dataformat <- 5
    n_col <- ifelse(property_name=='direct_capacity', 1,
            ifelse(property_name=='indirect_capacity', 2,
            ifelse(property_name=='impedance', 3,
            ifelse(property_name=='direct_hurdle_cost', 4,
            ifelse(property_name=='indirect_hurdle_cost', 5,
            NA )))))
  }
  else
  {
    n_col_dataformat <- 8
    n_col <- ifelse(property_name=='direct_capacity', 1,
            ifelse(property_name=='indirect_capacity', 2,
            ifelse(property_name=='direct_hurdle_cost', 3,
            ifelse(property_name=='indirect_hurdle_cost', 4,
            ifelse(property_name=='impedance', 5,
            ifelse(property_name=="loop_flow",6),
            ifelse(property_name=="p_shift_min",7),
            ifelse(property_name=="p_shift_max",8),
            NA )))))
  }
  
  if(is.na(n_col)){stop("unknown property")}
  
  
  #correct negative values for capacities        
  if(n_col<=2)
  {
    f1 <- function(x){max(new_value[x],0)}
    new_value = sapply(1:length(new_value), FUN=f1)
  }
  
  # duplicate value if its a scalar
  assertthat::assert_that(length(new_value) == 8760 || length(new_value) == 1)
  if (length(new_value) == 1)
  {
    new_value = rep(new_value,8760)
  }
  
  #load file containing link properties            
  link_file_name <- paste(opts$inputPath,"/links/" ,from(link_name),"/", to(link_name), ".txt",sep="")
  
  # check that this file exists
  assertthat::assert_that(file.exists(link_file_name))
  
  if (file.info(link_file_name)$size != 0)
  {
    # read file
    param_data <- read.table(link_file_name)
              
    # update column with new value 
    param_data[,n_col] = new_value
  }
  else if (file.info(link_file_name)$size ==0)
  {
    # The file exists but is empty : i.e. all column contains default value
    # file is built from scratch
                
    param_data <- as.table(matrix(0,8760,n_col_dataformat))
    
    # update column with new value
    param_data[,n_col] = new_value
  }
  
  # check if all value are null
  if(all(param_data == 0)){param_data <- c()}
  
  # write file
  utils::write.table(param_data, link_file_name, sep="\t", col.names = FALSE, row.names = FALSE)
  
}