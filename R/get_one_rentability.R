#' Get rentability of one investment candidate
#' 
#' @param output
#'   output from an Antares study, as obtained by \code{\link{get_output_antares}}
#' @param current_it
#'   current iteration of the benders problem with its associated characteristics
#' @param c
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param n_w
#'   number of weeks in the study
#' @return 
#' vector of rentability
#' 
#' @importFrom antaresRead readAntares 
#' @noRd

get_aggr_rentability <- function(c,output, current_it, n_w)
{
  # carreful : not sure the following line works in case of a partial iteration
  if((c$has_link_profile)&(!c$has_link_profile_indirect))
  { 
    # if link_profile is constant during the 8760 hours, we prefer extract annual values from Antares (better rounding)
    if(length(unique(c$link_profile))==1)
    {
      return(as.numeric(subset(output$output_link_y_s, link == c$link)$"MARG. COST"*unique(c$link_profile))- c$cost * n_w / 52)
      
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
          tmp_rentability_w <-  tmp_rentability_w + as.numeric(subset(output$output_link_w_s, link == c$link & timeId == w)$"MARG. COST"*unique(c$link_profile[first_h:last_h]))
        }
        else
        {
          tmp_rentability_w <- tmp_rentability_w + sum(as.numeric(subset(output$output_link_h_s, link == c$link & timeId >= first_h & timeId <= last_h)$"MARG. COST")*c$link_profile[first_h:last_h])
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
      if(length(unique(c$link_profile))==1 && sum(subset(output$output_link_h_s_i, mcYear == y)$sens_direct) == 8736)
      {
        tmp_rentability_direct <- tmp_rentability_direct + as.numeric(subset(output$output_link_y_s_i, link == c$link & mcYear == y)$"MARG. COST"*unique(c$link_profile))
      }
      else
      {
        for(w in current_it$weeks)
        {
          first_h <- 7*24*(w-1)+1
          last_h <- 7*24*w
          # if link_profile is constant during a week, we prefer extract weekly values from Antares (better rounding)
          if (length(unique(c$link_profile[first_h:last_h]))==1 && sum(subset(output$output_link_h_s_i,mcYear == y )$sens_direct[first_h:last_h]) == 168)
          {
            tmp_rentability_direct <-  tmp_rentability_direct + as.numeric(subset(output$output_link_w_s_i, link == c$link & timeId == w  & mcYear == y)$"MARG. COST"*unique(c$link_profile[first_h:last_h]))
          }
          else
          {
            tmp_rentability_direct <- tmp_rentability_direct + sum(as.numeric(subset(output$output_link_h_s_i, link == c$link & timeId >= first_h & timeId <= last_h & mcYear == y)$"MARG. COST DIRECT")*c$link_profile[first_h:last_h])
          }
        }
      }  
      
      ###### # Indirect :          
      if(length(unique(c$link_profile_indirect))==1 &&  sum(subset(output$output_link_h_s_i, mcYear == y)$sens_indirect) == 8736)
      {
        tmp_rentability_indirect <-  as.numeric(subset(output$output_link_y_s_i, link == c$link & mcYear == y)$"MARG. COST"*unique(c$link_profile_indirect))
      }
      else
      {
        for(w in current_it$weeks)
        {
          first_h <- 7*24*(w-1)+1
          last_h <- 7*24*w
          # if link_profile is constant during a week, we prefer extract weekly values from Antares (better rounding)
          if (length(unique(c$link_profile_indirect[first_h:last_h]))==1 && sum(subset(output$output_link_h_s_i,mcYear == y )$sens_indirect[first_h:last_h]) == 168)
          {
            tmp_rentability_indirect <-  tmp_rentability_indirect + as.numeric(subset(output$output_link_w_s_i, link == c$link & timeId == w  & mcYear == y)$"MARG. COST"*unique(c$link_profile_indirect[first_h:last_h]))
          }
          else
          {
            tmp_rentability_indirect <- tmp_rentability_indirect + sum(as.numeric(subset(output$output_link_h_s_i, link == c$link & timeId >= first_h & timeId <= last_h  & mcYear == y)$"MARG. COST INDIRECT")*c$link_profile_indirect[first_h:last_h])
          }
        }
      }
    }
    
    return( 1/length(current_it$mc_years)*(tmp_rentability_direct+tmp_rentability_indirect)- c$cost * n_w / 52)
    
  }
  else 
  {
    return(sum(as.numeric(subset(output$output_link_s, link == c$link)$"MARG. COST")) - c$cost * n_w / 52)
  }
}

#' Get rentability of one investment candidate
#' 
#' @param output
#'   output from an Antares study, as obtained by \code{\link{get_output_antares}}
#' @param output_link_y
#'   output from an Antares study annual
#' @param current_it
#'   current iteration of the benders problem with its associated characteristics
#' @param y
#'   mc_year  
#' @param c
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param n_w
#'   number of weeks in the study
#' @return 
#' vector of rentability
#' 
#' @importFrom antaresRead readAntares 
#' @noRd

get_aggr_rentability_yearly <- function(c,output, output_link_y, current_it, n_w,y)
{
  if((c$has_link_profile)&(!c$has_link_profile_indirect))
  {
    if(length(unique(c$link_profile))==1)
    {
      return(as.numeric(subset(output_link_y, link == c$link  & mcYear == y)$"MARG. COST"*unique(c$link_profile))- c$cost * n_w / 52)
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
          tmp_rentability_w <-  tmp_rentability_w + as.numeric(subset(output$output_link_w_s, link == c$link & timeId == w & mcYear == y)$"MARG. COST"*unique(c$link_profile[first_h:last_h]))
        }
        else
        {
          tmp_rentability_w <- tmp_rentability_w + sum(as.numeric(subset(output$output_link_h_s, link == c$link & timeId >= first_h & timeId <= last_h & mcYear == y)$"MARG. COST")*c$link_profile[first_h:last_h])
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
    # if link_profile is constant during the 8760 hours, we prefer extract annual values from Antares (better rounding)          
    if(length(unique(c$link_profile))==1 && sum(subset(output$output_link_h_s_i, mcYear == y)$sens_direct) == 8736)
    {
      tmp_rentability_direct <- tmp_rentability_direct + as.numeric(subset(output_link_y, link == c$link & mcYear == y)$"MARG. COST"*unique(c$link_profile))
    }
    else
    {
      for(w in current_it$weeks)
      {
        first_h <- 7*24*(w-1)+1
        last_h <- 7*24*w
        # if link_profile is constant during a week, we prefer extract weekly values from Antares (better rounding)
        if (length(unique(c$link_profile[first_h:last_h]))==1 && sum(subset(output$output_link_h_s_i,mcYear == y )$sens_direct[first_h:last_h]) == 168)
        {
          tmp_rentability_direct <-  tmp_rentability_direct + as.numeric(subset(output$output_link_w_s_i, link == c$link & timeId == w  & mcYear == y)$"MARG. COST"*unique(c$link_profile[first_h:last_h]))
        }
        else
        {
          tmp_rentability_direct <- tmp_rentability_direct + sum(as.numeric(subset(output$output_link_h_s_i, link == c$link & timeId >= first_h & timeId <= last_h & mcYear == y)$"MARG. COST DIRECT")*c$link_profile[first_h:last_h])
        }
      }
    }  
    
    ###### # Indirect :          
    if(length(unique(c$link_profile_indirect))==1 &&  sum(subset(output$output_link_h_s_i, mcYear == y)$sens_indirect) == 8736)
    {
      tmp_rentability_indirect <-  as.numeric(subset(output_link_y, link == c$link & mcYear == y)$"MARG. COST"*unique(c$link_profile_indirect))
    }
    else
    {
      for(w in current_it$weeks)
      {
        first_h <- 7*24*(w-1)+1
        last_h <- 7*24*w
        # if link_profile is constant during a week, we prefer extract weekly values from Antares (better rounding)
        if (length(unique(c$link_profile_indirect[first_h:last_h]))==1 && sum(subset(output$output_link_h_s_i,mcYear == y )$sens_indirect[first_h:last_h]) == 168)
        {
          tmp_rentability_indirect <-  tmp_rentability_indirect + as.numeric(subset(output$output_link_w_s_i, link == c$link & timeId == w  & mcYear == y)$"MARG. COST"*unique(c$link_profile_indirect[first_h:last_h]))
        }
        else
        {
          tmp_rentability_indirect <- tmp_rentability_indirect + sum(as.numeric(subset(output$output_link_h_s_i, link == c$link & timeId >= first_h & timeId <= last_h  & mcYear == y)$"MARG. COST INDIRECT")*c$link_profile_indirect[first_h:last_h])
        }
      }
    }          
    return(tmp_rentability_direct+tmp_rentability_indirect- c$cost * n_w / 52)
  }
  
  else
  {
    return(sum(as.numeric(subset(output_link_y, link == c$link & mcYear == y)$"MARG. COST")) - c$cost * n_w / 52)
  }
}

#' Get rentability of one investment candidate
#' 
#' @param output
#'   output from an Antares study, as obtained by \code{\link{get_output_antares}}
#' @param output_link_w
#'   output from an Antares study annual
#' @param current_it
#'   current iteration of the benders problem with its associated characteristics
#' @param y
#'   mc_year
#' @param w
#'   week   
#' @param c
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param n_w
#'   number of weeks in the study
#' @return 
#' vector of rentability
#' 
#' @importFrom antaresRead readAntares 
#' @noRd

get_aggr_rentability_weekly <- function(c,output, output_link_w, current_it, n_w,y,w)
{

  if((c$has_link_profile)&(!c$has_link_profile_indirect))
  {
    # (!!!!) carreful : will no work if number of simulated week is not 52 (???)
    first_h <- 7*24*(w-1)+1
    last_h <- 7*24*w
    # if link_profile is constant during a week, we prefer extract weekly values from Antares (better rounding)
    if (length(unique(c$link_profile[first_h:last_h]))==1)
    {
      tmp_rentability_w <- as.numeric(subset(output_link_w, link == c$link & timeId == w & mcYear == y)$"MARG. COST"*unique(c$link_profile[first_h:last_h]))
    }
    else
    {
      tmp_rentability_w <- sum(as.numeric(subset(output$output_link_h_s_i, link == c$link & timeId >= first_h & timeId <= last_h & mcYear == y)$"MARG. COST")*c$link_profile[first_h:last_h])
    }
    
    return(tmp_rentability_w - c$cost /52)
    
  }      
  
  else if(c$has_link_profile_indirect)
  {
    first_h <- 7*24*(w-1)+1
    last_h <- 7*24*w            
    ###### # Direct :
    tmp_rentability_direct <- 0
    tmp_rentability_indirect <- 0        
    
    # if link_profile is constant during a week, we prefer extract weekly values from Antares (better rounding)
    if (length(unique(c$link_profile[first_h:last_h]))==1 && sum(subset(output$output_link_h_s_i,mcYear == y )$sens_direct[first_h:last_h]) == 168)
    {
      tmp_rentability_direct <-  tmp_rentability_direct + as.numeric(subset(output_link_w, link == c$link & timeId == w  & mcYear == y)$"MARG. COST"*unique(c$link_profile[first_h:last_h]))
    }
    else
    {
      tmp_rentability_direct <- tmp_rentability_direct + sum(as.numeric(subset(output$output_link_h_s_i, link == c$link & timeId >= first_h & timeId <= last_h & mcYear == y)$"MARG. COST DIRECT")*c$link_profile[first_h:last_h])
    }
    
    ###### # Indirect :          
    # if link_profile is constant during a week, we prefer extract weekly values from Antares (better rounding)
    if (length(unique(c$link_profile_indirect[first_h:last_h]))==1 && sum(subset(output$output_link_h_s_i,mcYear == y )$sens_indirect[first_h:last_h]) == 168)
    {
      tmp_rentability_indirect <-  tmp_rentability_indirect + as.numeric(subset(output_link_w, link == c$link & timeId == w  & mcYear == y)$"MARG. COST"*unique(c$link_profile_indirect[first_h:last_h]))
    }
    else
    {
      tmp_rentability_indirect <- tmp_rentability_indirect + sum(as.numeric(subset(output$output_link_h_s_i, link == c$link & timeId >= first_h & timeId <= last_h  & mcYear == y)$"MARG. COST INDIRECT")*c$link_profile_indirect[first_h:last_h])
    }
    
    return(tmp_rentability_direct+tmp_rentability_indirect- c$cost / 52)
    
  }
  else
  {
    return(sum(as.numeric(subset(output_link_w, link == c$link & mcYear == y & timeId == w)$"MARG. COST")) - c$cost /52)
  }
}