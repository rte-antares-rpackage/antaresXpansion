

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
  general_parameters_file_name <- paste(opts$studyPath,"/settings/generaldata.ini",sep="")
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
  general_parameters_file_name <- paste(opts$studyPath,"/settings/generaldata.ini",sep="")
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



#' Modify the filters on the output of the areas
#' 
#' \code{filter_output_areas} is a function which modifies the input file of an ANTARES
#' study by updating the output filters of some areas
#' 
#' @param areas 
#'   Area, or vector of areas whose filter will be changed.
#' @param filter
#'   Vector of filters to adopt for the area. Can contain "annual", "monthly", "weekly", 
#'   "daily", "hourly" or a combination of them.
#' @param type
#'   On which type of output the filters will applied ? The MC synthesis (\code{type = "synthesis"}) ? The 
#'   year by year outputs (\code{type = "year-by-year"}) ? Or both(\code{type = c("synthesis", "year-by-year")}) 
#'   ? 
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
filter_output_areas <- function(areas, filter, type, opts = simOptions())
{
  # check that filter names are correct
  assert_that(all(filter %in% c("annual", "monthly", "weekly", "daily", "hourly")))
  
  # check that types  are correct
  assert_that(all(type %in% c("synthesis", "year-by-year")))
  
  # which are the activated types ?
  synthesis <-  any(type == "synthesis")
  yearByYear  <-  any(type == "year-by-year")
  
  # for each area
  for (a in areas)
  {
    # check if the file related to the area exists
    optimization_file_name <- paste(opts$inputPath,"/areas/", a, "/optimization.ini" ,sep="")
    
    assert_that(file.exists(optimization_file_name))
    assert_that(file.info(optimization_file_name)$size !=0)
    
    # read file
    param_data <- scan(optimization_file_name, what=character(), sep="/", quiet = TRUE)
    
    # update
    if(synthesis)
    {
      index <-  grep("filter-synthesis =",param_data,  fixed = TRUE)
      param_data[index] <-  paste0("filter-synthesis = ", paste0(filter, collapse = ", "))
    }
    if(yearByYear)
    {
      index <-  grep("filter-year-by-year =",param_data,  fixed = TRUE)
      param_data[index]  <-  paste0("filter-year-by-year = ", paste0(filter, collapse = ", "))
    }
    
    # write file
    write(param_data, optimization_file_name, sep = "/")
    
  }
}

#' Modify the filters on the output of the links
#' 
#' \code{filter_output_links} is a function which modifies the input file of an ANTARES
#' study by updating the output filters of some links
#' 
#' @param links 
#'   Link, or vector of links whose filter will be changed.
#' @param filter
#'   Vector of filters to adopt for the area. Can contain "annual", "monthly", "weekly", 
#'   "daily", "hourly" or a combination of them.
#' @param type
#'   On which type of output the filters will applied ? The MC synthesis (\code{type = "synthesis"}) ? The 
#'   year by year outputs (\code{type = "year-by-year"}) ? Or both(\code{type = c("synthesis", "year-by-year")}) 
#'   ? 
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
filter_output_links <- function(links, filter, type, opts = simOptions())
{
  # check that filter names are correct
  assert_that(all(filter %in% c("annual", "monthly", "weekly", "daily", "hourly")))
  
  # check that types  are correct
  assert_that(all(type %in% c("synthesis", "year-by-year")))
  
  # which are the activated types ?
  synthesis  <-  any(type == "synthesis")
  yearByYear  <-  any(type == "year-by-year")
  
  # for each links
  for (link in links)
  {
    # check if the file related to the area exists
    properties_file_name <- paste(opts$inputPath,"/links/", from(link), "/properties.ini" ,sep="")
    
    assert_that(file.exists(properties_file_name))
    assert_that(file.info(properties_file_name)$size !=0)
    
    # read file
    param_data <- scan(properties_file_name, what=character(), sep="/", quiet = TRUE)
    
    # get lines related with the corresponding link
    indexes = grep("^\\[",param_data)
    indexes = c(indexes, length(param_data))
    min_id = grep(paste("\\[",to(link), "\\]",sep=""),param_data)
    assert_that(length(min_id) == 1)
    max_id = min(indexes[(indexes > min_id)])
    id = min_id:max_id
    
    # update
    if(synthesis)
    {
      index <-  grep("filter-synthesis =",param_data[id],  fixed = TRUE)
      param_data[min_id + index - 1] <-  paste0("filter-synthesis = ", paste0(filter, collapse = ", "))
    }
    if(yearByYear)
    {
      index  <-  grep("filter-year-by-year =",param_data[id],  fixed = TRUE)
      param_data[min_id + index - 1] <-  paste0("filter-year-by-year = ", paste0(filter, collapse = ", "))
    }
    # write file
    write(param_data, properties_file_name, sep = "/")
  }
}




#' Set the unit-commitment mode of the ANTARES study
#' 
#' \code{set_uc_mode} is a function which modifies the input file of an ANTARES
#' study and set the simulation setting "unit-commitment-mode"
#' 
#' @param mode
#'   Unit-commitment mode. Must be equal to "fast", "accurate"
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
set_uc_mode <- function(mode, opts = simOptions())
{
  assert_that(mode %in% c("fast", "accurate"))

  # load setting file and check if it exists
  general_parameters_file_name <- paste(opts$studyPath,"/settings/generaldata.ini",sep="")
  assert_that(file.exists(general_parameters_file_name))
  assert_that(file.info(general_parameters_file_name)$size !=0)
  
  # read file
  param_data <- scan(general_parameters_file_name, what=character(), sep="/", quiet = TRUE)
  
  # find line describing the filtering setting
  index = grep("unit-commitment-mode =",param_data,  fixed = TRUE)
  assert_that(length(index) == 1)
  
  # update line
  if (mode == "accurate") 
  {
    param_data[index] = "unit-commitment-mode = accurate"
  }
  else
  {
    param_data[index] = "unit-commitment-mode = fast"
  }
  
  # write updated file
  write(param_data, general_parameters_file_name, sep = "/")
}


#' Enable or disable the options which allow (or not) the excecution of the heuristic
#' in fast mode
#' 
#' \code{enable_uc_heuristic} is a function which modifies the input file of an ANTARES
#' study and set the simulation setting "include-tc-minstablepower", "include-tc-min-ud-time" and
#' "include-dayahead"
#' 
#' @param enable
#'   Should the heuristic be excecuted (\code{TRUE}) or not (\code{FALSE}) ?
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
enable_uc_heuristic <- function(enable = TRUE, opts = simOptions())
{
  # load setting file and check if it exists
  general_parameters_file_name <- paste(opts$studyPath,"/settings/generaldata.ini",sep="")
  assert_that(file.exists(general_parameters_file_name))
  assert_that(file.info(general_parameters_file_name)$size !=0)
  
  # read file
  param_data <- scan(general_parameters_file_name, what=character(), sep="/", quiet = TRUE)
  
  # find line describing the filtering setting
  index_pmin = grep("include-tc-minstablepower =",param_data,  fixed = TRUE)
  index_minud = grep("include-tc-min-ud-time =",param_data,  fixed = TRUE)
  index_dayah = grep("include-dayahead =",param_data,  fixed = TRUE)
  
  assert_that(length(index_pmin) == 1)
  assert_that(length(index_minud) == 1)
  assert_that(length(index_dayah) == 1)
  
  # update line
  if (enable)
  {
    param_data[index_pmin] = "include-tc-minstablepower = true"
    param_data[index_minud] = "include-tc-min-ud-time = true"
    param_data[index_dayah] = "include-dayahead = true"
  }
  else
  {
    param_data[index_pmin] = "include-tc-minstablepower = false"
    param_data[index_minud] = "include-tc-min-ud-time = false"
    param_data[index_dayah] = "include-dayahead = false"
  }
  
  # write updated file
  write(param_data, general_parameters_file_name, sep = "/")
}

