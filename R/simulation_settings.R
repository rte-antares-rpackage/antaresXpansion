

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


#' Set parameters week
#' 
#' \code{set_week} is a function which modifies the input file of an ANTARES
#' study and set the "week" parameter. This parameters defines how the weekly output
#' are aggregated.
#' 
#' @param first_day
#'   First day of the week
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
set_week <- function(first_day, opts = simOptions())
{
  # check first day
  # standardise first day
  first_day <- tolower(first_day)
  first_day <- paste(toupper(substring(first_day, 1,1)), substring(first_day, 2),sep="" )
  assert_that(first_day %in% c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  
  # load setting file and check if it exists
  general_parameters_file_name <- paste(opts$studyPath,"/settings/generaldata.ini",sep="")
  assert_that(file.exists(general_parameters_file_name))
  assert_that(file.info(general_parameters_file_name)$size !=0)
  
  # read file
  param_data <- scan(general_parameters_file_name, what=character(), sep="/", quiet = TRUE)
  
  # find line describing the filtering setting
  index = grep("first.weekday =",param_data,  fixed = TRUE)

  assert_that(length(index) == 1)
  
  # update line
  param_data[index] = paste0("first.weekday = ", first_day)

  # write updated file
  write(param_data, general_parameters_file_name, sep = "/")
}


#' Get playlist of simulated MC years
#' 
#' \code{get_playlist} gives the identifier of the MC years which
#' will be simulated, taking into account the potential use of a 
#' playlist which can disactivate some MC years
#' 
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'   
#' @return 
#' Returns a vector of the identifier of the simulated MC year
#' 
#' @import assertthat antaresRead
#' @export
#' 
#' 
get_playlist <- function(opts = simOptions())
{
  # get all MC years
  mc_years <- 1:opts$parameters$general$nbyears
  
  # if no playlist is used, return all mc years
  if(opts$parameters$general$`user-playlist` == FALSE)
  {
    return(mc_years)
  }
  # otherwise, update the vector of mc_years by removing disabled years
  playlist_update_type <- names(opts$parameters$playlist)
  playlist_update_value <- opts$parameters$playlist
  
  # untouched playlist - no modification have been made 
  if(length(playlist_update_type) == 0)
  {
    return(mc_years)
  }
  
  # modified playlist - take into account the modifications
  assert_that(all(playlist_update_type %in% c("playlist_reset", "playlist_year +", "playlist_year -")))
  activated <- rep(TRUE, length(mc_years))
  
  for(i in 1:length(playlist_update_type))
  {
    # playlist_reset means that we start from a playlist where every MC year is disactivated
    if(playlist_update_type[i] == "playlist_reset")
    {
      activated <- rep(FALSE, length(mc_years))
    }
    # playlist_year + means that the corresponding year is added to the playlist
    if(playlist_update_type[i] == "playlist_year +")
    {
      activated[playlist_update_value[[i]]+1] <- TRUE
    }
    # playlist_year - means that the corresponding year is removed from the playlist
    if(playlist_update_type[i] == "playlist_year -")
    {
      activated[playlist_update_value[[i]]+1] <- FALSE
    } 
  }
  return(mc_years[activated])
}


#' Set parameter simulation start
#' 
#' \code{set_first_day} is a function which modifies the input file of an ANTARES
#' study and set the "simulation_start" parameter. This parameters defines which is the first
#' day of the study horizon which should be simulated
#' 
#' @param first_day
#'   day of the beginning of the simulated period. 
#'   Numeric between 1 and 365.
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
set_first_day <- function(first_day, opts = simOptions())
{
  # check first day
  # standardise first day
  first_day <- as.numeric(first_day)
  assert_that(length(first_day) == 1)
  assert_that(first_day %in% 1:365)

  # load setting file and check if it exists
  general_parameters_file_name <- paste(opts$studyPath,"/settings/generaldata.ini",sep="")
  assert_that(file.exists(general_parameters_file_name))
  assert_that(file.info(general_parameters_file_name)$size !=0)
  
  # read file
  param_data <- scan(general_parameters_file_name, what=character(), sep="/", quiet = TRUE)
  
  # find line describing the filtering setting
  index = grep("simulation.start =",param_data,  fixed = TRUE)
  
  assert_that(length(index) == 1)
  
  # update line
  param_data[index] = paste0("simulation.start = ", first_day)
  
  # write updated file
  write(param_data, general_parameters_file_name, sep = "/")
}


#' Set parameter simulation start
#' 
#' \code{set_last_day} is a function which modifies the input file of an ANTARES
#' study and set the "simulation_end" parameter. This parameters defines which is the last
#' day of the study horizon which should be simulated
#' 
#' @param last_day
#'   day of the beginning of the simulated period. 
#'   Numeric between 1 and 365.
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
set_last_day <- function(last_day, opts = simOptions())
{
  # check first day
  # standardise first day
  last_day <- as.numeric(last_day)
  assert_that(length(last_day) == 1)
  assert_that(last_day %in% 1:365)
  
  # load setting file and check if it exists
  general_parameters_file_name <- paste(opts$studyPath,"/settings/generaldata.ini",sep="")
  assert_that(file.exists(general_parameters_file_name))
  assert_that(file.info(general_parameters_file_name)$size !=0)
  
  # read file
  param_data <- scan(general_parameters_file_name, what=character(), sep="/", quiet = TRUE)
  
  # find line describing the filtering setting
  index = grep("simulation.end =",param_data,  fixed = TRUE)
  
  assert_that(length(index) == 1)
  
  # update line
  param_data[index] = paste0("simulation.end = ", last_day)
  
  # write updated file
  write(param_data, general_parameters_file_name, sep = "/")
}
