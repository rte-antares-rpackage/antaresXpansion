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
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions
#' @export
#' 
#' 
filter_output_areas <- function(areas, filter, type, opts = antaresRead::simOptions())
{
  # check that filter names are correct
  assertthat::assert_that(all(filter %in% c("annual", "monthly", "weekly", "daily", "hourly")))
  
  # check that types  are correct
  assertthat::assert_that(all(type %in% c("synthesis", "year-by-year")))
  
  # which are the activated types ?
  synthesis <-  any(type == "synthesis")
  yearByYear  <-  any(type == "year-by-year")
  
  # for each area
  for (a in areas)
  {
    # check if the file related to the area exists
    optimization_file_name <- paste(opts$inputPath,"/areas/", a, "/optimization.ini" ,sep="")
    
    assertthat::assert_that(file.exists(optimization_file_name))
    assertthat::assert_that(file.info(optimization_file_name)$size !=0)
    
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
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions
#' @export
#' 
#' 
filter_output_links <- function(links, filter, type, opts = antaresRead::simOptions())
{
  # check that filter names are correct
  assertthat::assert_that(all(filter %in% c("annual", "monthly", "weekly", "daily", "hourly")))
  
  # check that types  are correct
  assertthat::assert_that(all(type %in% c("synthesis", "year-by-year")))
  
  # which are the activated types ?
  synthesis  <-  any(type == "synthesis")
  yearByYear  <-  any(type == "year-by-year")
  
  # for each links
  for (link in links)
  {
    # check if the file related to the area exists
    properties_file_name <- paste(opts$inputPath,"/links/", from(link), "/properties.ini" ,sep="")
    
    assertthat::assert_that(file.exists(properties_file_name))
    assertthat::assert_that(file.info(properties_file_name)$size !=0)
    
    # read file
    param_data <- scan(properties_file_name, what=character(), sep="/", quiet = TRUE)
    
    # get lines related with the corresponding link
    indexes = grep("^\\[",param_data)
    indexes = c(indexes, length(param_data))
    min_id = grep(paste("\\[",to(link), "\\]",sep=""),param_data)
    assertthat::assert_that(length(min_id) == 1)
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

#' Set period on which we want the ANTARES simulation to run.
#' Update "first day" and "last day" parameters of the ANTARES study.  
#' 
#' 
#' @param weeks
#'   vector of weeks identiers which should be simulated
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return 
#' 
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions
#' @importFrom data.table first last
#' @importFrom antaresEditObject updateGeneralSettings
#' @export
#' 
set_simulation_period <- function(weeks, opts = antaresRead::simOptions())
{
  # weeks should be successives 
  assertthat::assert_that(all(weeks == seq(first(weeks), last(weeks))))
  assertthat::assert_that(all(weeks <= 52))
  
  # change parameters of the study
  first_d <- 7*(weeks[1] - 1) + 1
  last_d <- 7*(weeks[length(weeks)])
  antaresEditObject::updateGeneralSettings(simulation.start = first_d, 
                                           simulation.end = last_d, 
                                           opts = opts)
}