
#' Read investment candidates with their characteristics
#' 
#' \code{read_candidates} is a function which read the investments candidates
#' of the expansion planning problem and their characteristics. The information on
#' the candidates is stored in the file antaresStudyPath/user/expansion/candidates.ini.
#'   
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
read_candidates <- function(opts = antaresRead::simOptions())
{
  candidates_file_name <- paste(opts$studyPath,"/user/expansion/candidates.ini",sep="")
  assertthat::assert_that(file.exists(candidates_file_name))
  assertthat::assert_that(file.info(candidates_file_name)$size !=0)
  
  # read file
  param_data <- scan(candidates_file_name, what=character(), sep="/", quiet = TRUE)
  
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
    candidate$candidate_type <- "investment"
    candidate$investment_type <- "generation"
    candidate$link <- NA
    candidate$cost <- 0
    candidate$unit_size <- 0
    candidate$max_unit <- 0
    candidate$max_invest <- 0
    candidate$relaxed <- FALSE
      
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
        candidate$name <- option_value
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
      else if (option_name == "max-investment")
      {
        assertthat::assert_that(!is.na(as.numeric(option_value)))
        candidate$max_invest <- as.numeric(option_value)
        candidate$relaxed <- TRUE
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
    
    # update candidate list
    inv[[i]] <- candidate
    i <- i + 1
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
