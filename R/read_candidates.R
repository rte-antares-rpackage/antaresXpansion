
#' Read investment candidates with their characteristics
#' 
#' \code{read_candidates} is a function which read the investments candidates
#' of the expansion planning problem and their characteristics. The information on
#' the candidates is usually stored in the file antaresStudyPath/user/expansion/candidates.ini.
#' 
#' @param file
#'   path toward the text file which contains the candidates description
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
read_candidates <- function(file, opts = antaresRead::simOptions())
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
    candidate$cost <- 0
    candidate$unit_size <- 0
    candidate$max_unit <- 0
    candidate$max_invest <- 0
    candidate$relaxed <- FALSE
    candidate$has_link_profile <- FALSE
    candidate$has_link_profile_indirect <- FALSE
    candidate$link_profile <- 1 #data.frame(rep(1,8760))
    candidate$link_profile_indirect <- 1 #data.frame(rep(1,8760))
    candidate$already_installed_capacity <- 0 #     
    candidate$already_installed_link_profile <- 1  #data.frame(rep(1,8760))
    candidate$already_installed_link_profile_indirect <- 1  #data.frame(rep(1,8760))
    
    
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
      else if (option_name == "already-installed-capacity")
      {
        assertthat::assert_that(!is.na(as.numeric(option_value)))
        candidate$already_installed_capacity <- as.numeric(option_value)
      }
      else if (option_name == "link-profile")
      {
        profile_file <- paste0(paste(opts$studyPath,"/user/expansion/capa/", sep=""), option_value)
        assert_that(file.exists(profile_file))
        
        if (file.info(profile_file)$size !=0) 
        {
          # read file
          profile_data <-  read.table(profile_file, header=FALSE,sep = "\t")
          
          profile_data1 <- as.vector(t(profile_data[,c(1)]))
          assert_that(is.numeric(profile_data1))
          assert_that(length(profile_data1) %in% c(1,8760))
          
         if (ncol(profile_data)==2)  
         {
           profile_data2 <- as.vector(t(profile_data[,c(2)]))
           assert_that(is.numeric(profile_data2))
           assert_that(length(profile_data2) %in% c(1,8760))
           if(!isTRUE(all.equal(unique(profile_data1),1)) || !isTRUE(all.equal(unique(profile_data2),1)) )
           {
             candidate$link_profile_indirect <- profile_data2
             candidate$has_link_profile_indirect <- TRUE
             candidate$link_profile <- profile_data1
             candidate$has_link_profile <- TRUE
           }
         }
         else
         { 
           if(!isTRUE(all.equal(unique(profile_data1),1)))
           {
             candidate$link_profile <- profile_data1
             candidate$link_profile_indirect <- profile_data1
             candidate$has_link_profile <- TRUE
           }
         }
          
        }
      }
      else if (option_name == "already-installed-link-profile")
      {
        profile_file <- paste0(paste(opts$studyPath,"/user/expansion/capa/", sep=""), option_value)
        assert_that(file.exists(profile_file))
        
        if (file.info(profile_file)$size !=0) 
        {
          # read file
          profile_data <-  read.table(profile_file, header=FALSE,sep = "\t")
          
          profile_data1 <- as.vector(t(profile_data[,c(1)]))
          assert_that(is.numeric(profile_data1))
          assert_that(length(profile_data1) %in% c(1,8760))
          candidate$already_installed_link_profile <- profile_data1
          if (ncol(profile_data)==2)  
          {
            profile_data2 <- as.vector(t(profile_data[,c(2)]))
            assert_that(is.numeric(profile_data2))
            assert_that(length(profile_data2) %in% c(1,8760))
            candidate$already_installed_link_profile_indirect <- profile_data2
          }
          else
          {
            candidate$already_installed_link_profile_indirect <- profile_data1          
          }
        }
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
    if(candidate$max_invest == 0){
      update_link(candidate$link, "direct_capacity", 0 , opts)
      update_link(candidate$link, "indirect_capacity", 0, opts)
      next
    }
      
    
    # check that candidate is valid 
    assertthat::assert_that(candidate$unit_size >= 0)
    assertthat::assert_that(candidate$max_unit >= 0)
    assertthat::assert_that(candidate$max_invest >= 0)
    assertthat::assert_that(candidate$already_installed_capacity >= 0)
    assertthat::assert_that(!is.na(candidate$name))
    assertthat::assert_that(!is.na(candidate$link))
    assertthat::assert_that(candidate$link %in% opts$linkList)
    
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
    # if they are not, candidate$already_installed_capacity, candidate$already_installed_link_profile
    # and candidate$already_installed_link_profile_indirect should be equal for all candidates on the same
    # link
    links_with_candidates <- sapply(inv, FUN = function(c){c$link})
    links_with_several_candidates <- unique(links_with_candidates[duplicated(links_with_candidates)])
    
    for(l in links_with_several_candidates)
    {
      id_list <- which(links_with_candidates == l)
      assertthat::assert_that(length(id_list) >= 2)
      ref_l <- id_list[1]
      for(id_l in id_list[-1])
      {
        if( !(inv[[id_l]]$already_installed_capacity == inv[[ref_l]]$already_installed_capacity))
        {
          stop(inv[[ref_l]]$name, " and ", inv[[id_l]]$name, " are investment candidates applying on the same 
               link and should therefore have similar already-installed-capacities")
        }
        if( !(length(inv[[id_l]]$already_installed_link_profile) == length(inv[[ref_l]]$already_installed_link_profile)))
        {
          stop(inv[[ref_l]]$name, " and ", inv[[id_l]]$name, " are investment candidates applying on the same 
               link and should therefore have similar already-installed-link-profile")
        }
        if( ! all(inv[[id_l]]$already_installed_link_profile == inv[[ref_l]]$already_installed_link_profile))
        {
          stop(inv[[ref_l]]$name, " and ", inv[[id_l]]$name, " are investment candidates applying on the same 
               link and should therefore have similar already-installed-link-profile")
        }
        if( !(length(inv[[id_l]]$already_installed_link_profile_indirect) == length(inv[[ref_l]]$already_installed_link_profile_indirect)))
        {
          stop(inv[[ref_l]]$name, " and ", inv[[id_l]]$name, " are investment candidates applying on the same 
               link and should therefore have similar already-installed-link-profile")
        }
        if( ! all(inv[[id_l]]$already_installed_link_profile_indirect == inv[[ref_l]]$already_installed_link_profile_indirect))
        {
          stop(inv[[ref_l]]$name, " and ", inv[[id_l]]$name, " are investment candidates applying on the same 
               link and should therefore have similar already-installed-link-profile")
        }
      }
    }
  }
  return(inv)
}



#' Return vector of candidate names which have a link profile in general 
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

#' Return vector of candidate names which have just a unique link profile
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
with_profile_unique <- function(candidates)
{
  f <- function(c)
  {if((c$has_link_profile)&(!c$has_link_profile_indirect)) return(c$link)
    else return (NA)
  }
  link_with_profile <- sapply(candidates, FUN = f)
  return(link_with_profile[!is.na(link_with_profile)])
}


#' Return vector of candidate names which have a direct link profile and an indirect profile
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
with_profile_indirect <- function(candidates)
{
  f <- function(c)
  {if(c$has_link_profile_indirect) return(c$link)
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



