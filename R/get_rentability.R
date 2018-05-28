#' Get rentability of investments candidates from an antares output
#' 
#' @param output_antares
#'   output from an Antares study, as obtained by \code{antaresRead::setSimulationPath}
#' @param current_it
#'   current iteration of the benders problem with its associated characteristics
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param n_w
#'   number of weeks in the study
#' @return 
#' vector of rentability
#' 
#' @importFrom antaresRead readAntares 
#' @noRd

get_expected_rentability <- function(output_antares, current_it, candidates, n_w)
{
  n_candidates <- length(candidates)
  if(!current_it$full)
  {
    # in case of a partial iteration, expected rentability over all weeks 
    # of all mc years cannot be computed
    return(rep(NA, n_candidates))
  }
  else
  {
    # first, read antares outputs 
    # hourly results (for candidates with link profile only)
    if (length(with_profile(candidates)) > 0 )
    {
      output_link_h_s = readAntares(areas = NULL, links = with_profile(candidates), mcYears = NULL, 
                                    timeStep = "hourly", opts = output_antares, showProgress = FALSE,
                                    select = "MARG. COST")
    }
    # synthetic results for other candidates 
    if (length(without_profile(candidates)) > 0 )
    {
      output_link_s = antaresRead::readAntares(areas = NULL, links = without_profile(candidates), mcYears = NULL, 
                                             timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                             select = "MARG. COST")
    }
    
    # second, get aggregated rentability for those links
    # carreful : not sure the following line works in case of a partial iteration
    
    get_aggr_rentability <- function(c)
    {
      if((c$has_link_profile)&(!c$has_link_profile_indirect))
      { 
        # if link_profile is constant during the 8760 hours, we prefer extract annual values from Antares (better rounding)
        if(length(unique(c$link_profile))==1)
        {
          return(as.numeric(subset(output_link_y_s, link == c$link)$"MARG. COST"*unique(c$link_profile))- c$cost * n_w / 52)

        }
        else
        {
          tmp_rentability_w <- 0
          for(w in current_it$weeks)
          {
            first_h <- 7*24*(w-1)+1
            last_h <- 7*24*w
            # if link_profile is constant during a week, we prefer extract weekly values from Antares (better rounding)
            if (length(unique(c$link_profile[first_h:last_h]))==1)
            {
              tmp_rentability_w <-  tmp_rentability_w + as.numeric(subset(output_link_w_s, link == c$link & timeId == w)$"MARG. COST"*unique(c$link_profile[first_h:last_h]))
            }
            else
            {
              tmp_rentability_w <- tmp_rentability_w + sum(as.numeric(subset(output_link_h_s, link == c$link & timeId >= first_h & timeId <= last_h)$"MARG. COST")*c$link_profile[first_h:last_h])
            }
          }
          return(tmp_rentability_w - c$cost * n_w / 52)
                   
        }       
      }

      else if(c$has_link_profile_indirect)
      {        
        ###### # Direct :
        tmp_rentability_direct <- 0
        tmp_rentability_indirect <- 0        
        for(y in current_it$mc_years)
        {
          # if link_profile is constant during the 8760 hours, we prefer extract annual values from Antares (better rounding)          
          if(length(unique(c$link_profile))==1 && sum(subset(output_link_h_s_i, mcYear == y)$sens_direct) == 8736)
          {
            tmp_rentability_direct <- tmp_rentability_direct + as.numeric(subset(output_link_y_s_i, link == c$link & mcYear == y)$"MARG. COST"*unique(c$link_profile))
          }
          else
          {
            for(w in current_it$weeks)
            {
              first_h <- 7*24*(w-1)+1
              last_h <- 7*24*w
              # if link_profile is constant during a week, we prefer extract weekly values from Antares (better rounding)
              if (length(unique(c$link_profile[first_h:last_h]))==1 && sum(subset(output_link_h_s_i,mcYear == y )$sens_direct[first_h:last_h]) == 168)
              {
                tmp_rentability_direct <-  tmp_rentability_direct + as.numeric(subset(output_link_w_s_i, link == c$link & timeId == w  & mcYear == y)$"MARG. COST"*unique(c$link_profile[first_h:last_h]))
              }
              else
              {
                tmp_rentability_direct <- tmp_rentability_direct + sum(as.numeric(subset(output_link_h_s_i, link == c$link & timeId >= first_h & timeId <= last_h & mcYear == y)$"MARG. COST DIRECT")*c$link_profile[first_h:last_h])
              }
            }
          }  
          
          ####### Indirect :          
          if(length(unique(c$link_profile_indirect))==1 &&  sum(subset(output_link_h_s_i, mcYear == y)$sens_indirect) == 8736)
          {
            tmp_rentability_indirect <-  as.numeric(subset(output_link_y_s_i, link == c$link & mcYear == y)$"MARG. COST"*unique(c$link_profile_indirect))
          }
          else
          {
            for(w in current_it$weeks)
            {
              first_h <- 7*24*(w-1)+1
              last_h <- 7*24*w
              # if link_profile is constant during a week, we prefer extract weekly values from Antares (better rounding)
              if (length(unique(c$link_profile_indirect[first_h:last_h]))==1 && sum(subset(output_link_h_s_i,mcYear == y )$sens_indirect[first_h:last_h]) == 168)
              {
                tmp_rentability_indirect <-  tmp_rentability_indirect + as.numeric(subset(output_link_w_s_i, link == c$link & timeId == w  & mcYear == y)$"MARG. COST"*unique(c$link_profile_indirect[first_h:last_h]))
              }
              else
              {
                tmp_rentability_indirect <- tmp_rentability_indirect + sum(as.numeric(subset(output_link_h_s_i, link == c$link & timeId >= first_h & timeId <= last_h  & mcYear == y)$"MARG. COST INDIRECT")*c$link_profile_indirect[first_h:last_h])
              }
            }
          }
        }
        
        return( 1/length(current_it$mc_years)*(tmp_rentability_direct+tmp_rentability_indirect)- c$cost * n_w / 52)
      }
      else 
      {
        return(sum(as.numeric(subset(output_link_s, link == c$link)$"MARG. COST")) - c$cost * n_w / 52)
      }
    }
    
    average_rentability <- sapply(candidates, FUN = get_aggr_rentability)
    return(average_rentability)
  }
    
}