
#' update link capacities with new value of x$installed_capacities
#' 
#' \code{read_candidates} is a function which read the investments candidates
#' of the expansion planning problem and their characteristics. The information on
#' the candidates is usually stored in the file antaresStudyPath/user/expansion/candidates.ini.
#' 
#' @param current_it
#'   current iteration of the benders problem with its associated characteristics
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param exp_options
#'   list of benders decomposition options, as returned by
#'   \code{\link{read_options}}.
#' @param x
#'   benders data structure
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return 
#' Returns a list containing the different investment candidates. 
#' 
#' @noRd
#' 
update_link_capacities <- function(x, current_it, candidates, exp_options, opts)
{
  for(c in candidates)
  {
    
    new_capacity_direct   <- get_capacity_profile(get_capacity(x$invested_capacities, candidate = c$name, it = current_it$n),
                                                  c$link_profile, exp_options$uc_type)+c$already_installed_capacity*c$already_installed_link_profile
    
    new_capacity_indirect <- get_capacity_profile(get_capacity(x$invested_capacities, candidate = c$name, it = current_it$n),
                                                  c$link_profile_indirect, exp_options$uc_type)+c$already_installed_capacity*c$already_installed_link_profile_indirect
    
    if(c$has_link_profile_indirect)
    { 
      new_capacity_direct[new_capacity_direct < 1 ] <- 1
      new_capacity_indirect[new_capacity_indirect < 1 ] <- 1
    }
    
    # update study
    update_link(c$link, "direct_capacity", new_capacity_direct , opts)
    update_link(c$link, "indirect_capacity", new_capacity_indirect, opts)
  }
  
}