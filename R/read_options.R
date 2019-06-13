
#' Read expansion planning options
#' 
#' \code{read_options} is a function which read the options related to the
#' expansion planning optimization. The options are usually stored in the file 
#' antaresStudyPath/user/expansion/settings.ini.
#' 
#' @param file
#'   path toward the text file which contains the settings 
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return 
#' Returns a list containing the different optimisation options 
#' 
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions
#' @importFrom antaresEditObject getPlaylist
#' @export
#' 
#' 
#' 
read_options <- function(file, opts = antaresRead::simOptions())
{
  assertthat::assert_that(file.exists(file))
  #assertthat::assert_that(file.info(option_file_name)$size !=0)
  
  # read file
  param_data <- scan(file, what=character(), sep="/", quiet = TRUE)
  
  # initiate option list, with default values
  options <- list()
  options$method <- "benders_decomposition"
  options$uc_type <- "expansion_fast"
  options$master <- "integer"
  options$optimality_gap <- 0
  options$max_iteration <- Inf
  options$cut_type <- "yearly"
  options$week_selection <- FALSE
  options$relaxed_optimality_gap <- "0.01%"
  #options$solver <- "cbc"   
  options$y_weights <- NA
  options$ampl <- list()
  options$additional_constraints <- NULL
  
  
  # if the file is empty, the default values are kept 
  if(length(param_data) == 0){return(options)}
  
  # go through every line of the file
  for(line in 1:length(param_data))
  {
    # read option and value
    option_name <- strsplit(param_data[line], "=")[[1]][1]
    if(is.na(option_name)){next} # empty line
    if(option_name == ""){next} # empty line
    
    option_value <- strsplit(param_data[line], "=")[[1]][2]
    
    # remove white spaces in the beginning and the end
    option_name <- sub("^\\s+", "", option_name)
    option_name <- sub("\\s+$", "", option_name)
    option_value <- sub("^\\s+", "", option_value)
    option_value <- sub("\\s+$", "", option_value)
    
    # see what option it is referring to
    if (option_name == "method")
    {
      assertthat::assert_that(option_value %in% c("benders_decomposition"))
      options$method <- option_value
    }
    else if (option_name == "uc_type")
    {
      if(option_value == "fast")
      { stop("Since v0.7 of antaresXpansion package, 'fast' mode does no longer exist, use 'expansion_fast' instead")}
      if(option_value == "accurate")
      { stop("Since v0.7 of antaresXpansion package, 'accurate' mode does no longer exist, use 'expansion_accurate' instead")}
      
      assertthat::assert_that(option_value %in% c("expansion_accurate", "expansion_fast", "relaxed_fast", "relaxed_accurate"))
      
      options$uc_type <- option_value
      
      if(option_value == "relaxed_fast")
      { 
        warning("Since v0.7 of antaresXpansion package, 'relaxed_fast' mode has been renamed 'expansion_fast'")
        options$uc_type <- "expansion_fast"
      }
      if(option_value == "relaxed_accurate")
      { 
        warning("Since v0.7 of antaresXpansion package, 'relaxed_accurate' mode has been renamed 'expansion_accurate'")
        options$uc_type <- "expansion_accurate"
      }
      
      
    }
    else if (option_name == "master")
    {
      assertthat::assert_that(option_value %in% c("integer", "relaxed", "full_integer"))
      options$master <- option_value
    }
    else if (option_name == "optimality_gap")
    {
      # if the value is numeric, it is the optimality gap in euros
      if (!is.na(suppressWarnings(as.numeric(option_value)))) 
      {
        options$optimality_gap <- as.numeric(option_value)

      }
      # else, the optimality gap is in % of the best found solution
      else
      {
        option_value <- gsub(" ", "", option_value, fixed = TRUE)
        assertthat::assert_that(grepl("%$", option_value))
        option_value_bis <- gsub("%$", "", option_value) 
        assertthat::assert_that(!is.na(as.numeric(option_value_bis)))
        options$optimality_gap <- option_value
      }
    }
    else if (option_name == "max_iteration")
    {
      assertthat::assert_that(!is.na(as.numeric(option_value)))
      options$max_iteration <- round(as.numeric(option_value))
    }
    else if (option_name == "cut_type")
    {
      assertthat::assert_that(option_value %in% c("average", "yearly", "weekly"))
      options$cut_type <- option_value
    }
    else if (option_name == "week_selection")
    {
      assertthat::assert_that(option_value %in% c("true", "false"))
      # for now, week selection is forced to false
      # since link-profile has been added, consistency with update_weekly_cuts has to be checked
      # options$week_selection <- as.logical(option_value) 
    }
    else if (option_name == "relaxed_optimality_gap")
    {
      # if the value is numeric, it is the optimality gap in euros
      if (!is.na(suppressWarnings(as.numeric(option_value)))) 
      {
        options$relaxed_optimality_gap <- as.numeric(option_value)
      }
      # else, the optimality gap is in % of the best found solution
      else
      {
        option_value <- gsub(" ", "", option_value, fixed = TRUE)
        assertthat::assert_that(grepl("%$", option_value))
        option_value_bis <- gsub("%$", "", option_value) 
        assertthat::assert_that(!is.na(as.numeric(option_value_bis)))
        options$relaxed_optimality_gap <- option_value
      }
    }
    else if (option_name == "solver")  # old option
    {
      options$ampl$solver <- option_value
      warning("Since v0.10 of antaresXpansion package, 'solver' option has been renamed 'ampl.solver'")
      
    }
    else if (option_name == "yearly-weights")
    {
      weight_file <- paste0(paste(opts$studyPath,"/user/expansion/", sep=""), option_value)
      assert_that(file.exists(weight_file))
      yearly_weights <- scan(weight_file, quiet = TRUE)
      assertthat::assert_that(all(yearly_weights >= 0))
      assertthat::assert_that(sum(yearly_weights) > 0)

      if(length(yearly_weights) == length(antaresEditObject::getPlaylist(opts)))
      {
        # same number of weights than number of MC years in PLAYLIST
        options$y_weights <- yearly_weights / sum(yearly_weights)
        names(options$y_weights) <- antaresEditObject::getPlaylist(opts)
      }
      else if(length(yearly_weights) == opts$parameters$general$nbyears)
      {
        #same number of weights than number of MC years
        options$y_weights <- yearly_weights[antaresEditObject::getPlaylist(opts)]
        options$y_weights <- options$y_weights/sum(options$y_weights)
        names(options$y_weights) <- antaresEditObject::getPlaylist(opts)
      }
      else
      {
        stop("Number of yearly weights does not correspond to number of MC years in the Antares study")
      }
    }
    else if (grepl(pattern = "^ampl\\.", option_name))  # old option
    {
      restricted_option_name <- gsub(pattern = "^ampl\\.", replacement = "", x =  option_name)
      options$ampl[[restricted_option_name]] <- option_value
    }
    else if (option_name == "additional-constraints")
    {
      constraints_file <- paste0(paste(opts$studyPath,"/user/expansion/", sep=""), option_value)
      assert_that(file.exists(constraints_file))
      constraints <- readChar(con = constraints_file, nchars = file.info(constraints_file)$size)
      options$additional_constraints <- paste0(options$additional_constraints, "\n", constraints)
    }
    else
    {
      warning(paste0("Unknown option : ", option_name))
    }
  }
  
  # check consistency of options
  if(any(!is.na(options$y_weights)) & options$cut_type == "average")
  {
    stop('options "yearly-weights" and "cut-type = average" are not compatible')
  }
  return(options)
}
