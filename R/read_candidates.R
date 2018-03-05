
#' Read investment candidates with their characteristics
#' 
#' \code{read_candidates} is a function which read the investments candidates
#' of the expansion planning problem and their characteristics. The information on
#' the candidates is usually stored in the file antaresStudyPath/user/expansion/candidates.ini.
#' 
#' @param file
#'   path toward the text file which contains the candidates description
#' @param studies
#'   list of simulated years with their according antares studies, as 
#'   returned by the function \code{\link{read_studies}}
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return 
#' Returns a list containing the different investment candidates. 
#' 
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions
#' @export
#' 
#' 
#' 
read_candidates <- function(file, studies = NULL, opts = antaresRead::simOptions())
{
  assertthat::assert_that(file.exists(file))
  assertthat::assert_that(file.info(file)$size !=0)
  
  # read file
  param_data <- scan(file, what=character(), sep="/", quiet = TRUE)
  
  # Get the candidates id
  candidates_id <- param_data[grep("^\\[",param_data)]
  f1 <- function(x){gsub("\\]","",gsub("\\[","",candidates_id[x]))}
  candidate_id <- sapply(1:length(candidates_id),FUN=f1)
  
  # Get the indexes on which starts the description of each candidate
  n_candidates = length(candidates_id)
  index <- grep("^\\[",param_data)
  index <- c(index,length(param_data)+1)
  inv <- list()
  i <- 1
  
  for(pr in 1:n_candidates)
  {
    # initiate candidate characteristics, with default values
    candidate <- list()
    candidate$id <- candidate_id[pr]
    candidate$name <- NA
    candidate$enable <- TRUE
    candidate$candidate_type <- "investment"
    candidate$investment_type <- "generation"
    candidate$link <- NA
    candidate$unit_size <- 0
    candidate$max_unit <- 0
    candidate$max_invest <- 0
    candidate$relaxed <- FALSE
    candidate$has_link_profile <- FALSE
    candidate$link_profile <- 1#  data.frame(rep(1,8760))
    
    # the next parameter is only used with one-year investment mode
    candidate$cost <- NA  #here cost = annuity
    
    # the next parameters are only used when multi-year investment is made
    candidate$lifetime <- NA
    if(!is.null(studies))
    {
      candidate$investment_cost <- rep(NA, studies$n_simulated_years)
      candidate$operation_and_maintenance_cost <- rep(NA, studies$n_simulated_years)
      candidate$mothball_cost <- rep(NA, studies$n_simulated_years)
    }
    
    # read candidate characteristics
    for(line in (index[pr]+1):(index[pr+1]-1))
    {
      # read option and value
      option_name <- strsplit(param_data[line], "=")[[1]][1]
      if(option_name == ""){next} # empty line
      
      option_value <- strsplit(param_data[line], "=")[[1]][2]
      
      # remove white spaces in the beginning and the end
      option_name <- sub("^\\s+", "", option_name)
      option_name <- sub("\\s+$", "", option_name)
      option_value <- sub("^\\s+", "", option_value)
      option_value <- sub("\\s+$", "", option_value)
      
      # see what option it is referring to
      if (option_name == "name")
      {
        option_value <- sub(pattern = " ", replacement = "_", option_value)
        candidate$name <- option_value
      }
      else if (option_name == "enable")
      {
        assertthat::assert_that(option_value %in% c("true", "false"))
        candidate$enable <- as.logical(option_value) 
      }
      else if (option_name == "candidate-type")
      {
        assertthat::assert_that(option_value %in% c("investment", "decommissioning"))
        candidate$candidate_type <- option_value
      }
      else if (option_name == "investment-type")
      {
        candidate$investment_type <- option_value
      }
      else if (option_name == "link")
      {
        option_value <- tolower(option_value)
        if(!(option_value %in% opts$linkList)) stop("Unknown link in candidates.ini : ", option_value)
        candidate$link <- option_value
      }
      else if (option_name == "annual-cost-per-mw")
      {
        assertthat::assert_that(!is.na(as.numeric(option_value)))
        candidate$cost <- as.numeric(option_value)
      }
      else if (option_name == "max-units")
      {
        assertthat::assert_that(!is.na(as.numeric(option_value)))
        candidate$max_unit <- round(as.numeric(option_value))
      }
      else if (option_name == "unit-size")
      {
        assertthat::assert_that(!is.na(as.numeric(option_value)))
        candidate$unit_size <- as.numeric(option_value)
      }
      else if (option_name == "link-profile")
      {
        profile_file <- paste0(paste(opts$studyPath,"/user/expansion/capa/", sep=""), option_value)
        assert_that(file.exists(profile_file))
        
        if (file.info(profile_file)$size !=0) 
        {
          # read file
          profile_data <-  read.table(profile_file, header=FALSE)
          profile_data <- as.vector(t(profile_data))
          assert_that(is.numeric(profile_data))
          candidate$link_profile <- profile_data
          candidate$has_link_profile <- TRUE
        }
      }
      else if (option_name == "max-investment")
      {
        assertthat::assert_that(!is.na(as.numeric(option_value)))
        candidate$max_invest <- as.numeric(option_value)
        candidate$relaxed <- TRUE
      }
      else if (grepl("investment-cost", option_name))
      {
        id <- identify_horizon(option_name, variable = "investment-cost", studies)
        if(is.null(id)) next()
        
        assertthat::assert_that(!is.na(as.numeric(option_value)))
        candidate$investment_cost[id] <- as.numeric(option_value)
      }
      else if (grepl("operation-and-maintenance-cost", option_name))
      {
        
        id <- identify_horizon(option_name, variable = "operation-and-maintenance-cost", studies)
        if(is.null(id)) next()
        
        assertthat::assert_that(!is.na(as.numeric(option_value)))
        candidate$operation_and_maintenance_cost[id] <- as.numeric(option_value)
      }
      else if (grepl("mothball-cost", option_name)) 
      {
        id <- identify_horizon(option_name, variable = "mothball-cost", studies)
        if(is.null(id)) next()
        
        assertthat::assert_that(!is.na(as.numeric(option_value)))
        candidate$mothball_cost[id] <- as.numeric(option_value)
      }
      else if (option_name == "lifetime")
      {
        assertthat::assert_that(!is.na(as.numeric(option_value)))
        candidate$lifetime <- round(as.numeric(option_value))
      }
      else
      {
        warning(paste0("Unknown candidate characteristic : ", option_name))
      }
    }
    
    # update max_invest, max_units and unit_size to fil with all options
    if(!candidate$relaxed)
    {
      candidate$max_invest <- candidate$max_unit * candidate$unit_size
    }
    if(candidate$relaxed)
    {
      candidate$unit_size <- 1
      candidate$max_unit <-  candidate$max_invest
    }
    
    #  do not add the candidate the to the list if its max possible capacity equals 0
    if(candidate$max_invest == 0){next}
      
    
    # check that candidate is valid 
    assertthat::assert_that(candidate$unit_size >= 0)
    assertthat::assert_that(candidate$max_unit >= 0)
    assertthat::assert_that(candidate$max_invest >= 0)
    assertthat::assert_that(!is.na(candidate$name))
    assertthat::assert_that(!is.na(candidate$link))
    assertthat::assert_that(candidate$link %in% opts$linkList)
    if(is.null(studies))  assertthat::assert_that(!is.na(candidate$cost))
    if(!is.null(studies)) assertthat::assert_that(all(!is.na(candidate$investment_cost)))
    if(!is.null(studies)) assertthat::assert_that(all(!is.na(candidate$operation_and_maintenance_cost)))
    
    # update candidate list if the candidate is enabled
    if(candidate$enable)
    {
      inv[[i]] <- candidate
      i <- i + 1
    }
  }
  
  # check that candidates names are unique
  if(anyDuplicated(sapply(inv, FUN = function(c){c$name})) > 0)
  {
    stop("investment candidate name must be unique")
  }
  # check that candidates links are unique
  if(anyDuplicated(sapply(inv, FUN = function(c){c$link})) > 0)
  {
    stop("investment candidate link must be unique")
  }
  return(inv)
}



#' Return vector of candidate names which have a link profile
#' 
#'   
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#'
#' @return 
#' Returns a vector of link name
#' @noRd
#' 
with_profile <- function(candidates)
{
  f <- function(c)
  {if(c$has_link_profile) return(c$link)
    else return (NA)
  }
  link_with_profile <- sapply(candidates, FUN = f)
  return(link_with_profile[!is.na(link_with_profile)])
}


#' Return vector of candidate names which do not have a link profile
#' 
#'   
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#'
#' @return 
#' Returns a vector of link name
#' @noRd
#' 
without_profile <- function(candidates)
{
  f <- function(c)
  {if(!c$has_link_profile) return(c$link)
    else return (NA)
  }
  link_without_profile <- sapply(candidates, FUN = f)
  return(link_without_profile[!is.na(link_without_profile)])
}

#' Identify the identifier(s) of the horizons set for that candidates.ini line
#' The following syntax is used in candidate.ini
#' 
#' investment_cost[2020] = 60000 # cost for 2020 only
#' investment_cost = 60000 # cost for all simulated years
#'   
#' @param option_name
#' option read in candidates.ini
#' @param variable
#' variable identified for this option_name
#' @param studies
#'   list of simulated years with their according antares studies, as 
#'   returned by the function \code{\link{read_studies}}
#' 
#' @return 
#' Returns a vector of link name
#' @importFrom assertthat assert_that
#' @noRd
#' 
identify_horizon <- function(option_name, variable , studies)
{
  if(option_name == variable)
  {
     # syntax : investment_cost = 60000 # cost for all simulated years
    return(1:studies$n_simulated_years)
  }
  
  # else, syntax investment_cost[2020]
  # check syntax
  if(!grepl(paste0( "^", variable, "[[[:digit:]]+]$"), option_name))
  {
    warning("Unknown syntax in candidates.ini file : '", option_name, " = ...'. The correct syntax is (e.g.): '", variable, "[2030] = ...'")
    return(NULL)
  }
  
  # retreive date
  t <- unlist(strsplit(unlist(strsplit(option_name, "[", fixed = TRUE)), "]", fixed = TRUE))
  assertthat::assert_that(length(t) == 2)
  year <- as.numeric(t[2])
  assertthat::assert_that(!is.na(year))
  
  if(!(year %in% studies$simulated_years))
  {
    warning("In candidates.ini file : '", option_name, " = ...'. Year ", year, " is not part of studies.ini.")
    return(NULL)
  }
  
  # retreive identifier
  return(which(year == studies$simulated_years))
}

